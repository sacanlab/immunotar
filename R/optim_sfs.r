source_disabled__=function(...){invisible(NULL)}
###############################################################
#optimize the parameters one-by-one. We re-use optim_sfs() implementation for this.
optim_onebyone=function(par,fn,o,...){
  o=opt_set(onebyone__=TRUE,singlepar=TRUE,o);
  return(optim_sfs(par,fn,o));
}
###############################################################
#Sequential forward or backward selection.
optim_sfs=function(par,fn,o,...){
o=opt_set(
  direction='forward'
  ,onebyone__=FALSE #used by optim_onebyone() to reuse this function implementation for a different purpose (optimize pars one-by-one (setting them to zero is not done, that's not a concern/objective))
  ,lockzeropars=NULL #whether to exclude the par entries that are already zero from exploration. Setting to FALSE only makes sense in combination with suboptim=T. defaults to TRUE for direction=reverse.
  ,selectnonzeropars=F #in forward-selection whether to pre-select the parameters whose initial values are non-zero.
  ,locknonzeropars=F #whether to lock the nonzeropars from changing during optimization (useful in combination with suboptim=T and selectnonzeropars=T)
  ,suboptim=F
  ,osuboptim=list(
    method="Nelder-Mead"
    ,control=list(maxit=10)
    ,jitter=0 #whether & how much noise to add to parameters (except the locked ones) before an optimization. This is to help get out of local minima that was established by the paramters that have been selected.
    )
  ,doparallel=T
  ,trace=1
  ,maximize=F
  ,pargroups=NULL
  ,pardepends=NULL #provide feature dependencies so a dependent is not considered if its parent is not selected yet. Helps avoid redundant exploration (e.g., in immunotar, a curve parameter should not be explored if its corresponding weight parameter is zero).
  ,usepargroups=F #whether to turn pars on/off in groups as defined in pargroups.
  ,o,o$control
);
if(is.null(o$parnames)){ o$parnames=paste0('par',forlength(par)); }

o$osuboptim=opt_set(list(
  singlepar=T #In forward-feature selection, whether to only optimize the single parameter that is being considered
  ),list_removefields(o,csv('suboptim,osuboptim,methodcontrols,control')),o$osuboptim);
osuboptim=o$osuboptim;
osuboptim$lower=var_pickfirstnotnull(osuboptim$lower,o$lower);
osuboptim$upper=var_pickfirstnotnull(osuboptim$upper,o$upper);
osuboptim$parsigns=var_pickfirstnotnull(osuboptim$parsigns,o$parsigns);


isforward=o$direction=='forward';
isreverse=!isforward;
stopfif(o$onebyone__&&!isforward,'onebyone optimization is only defined for forward selection.')
stopfif(o$onebyone__&&!osuboptim$singlepar,'onebyone optimization is only defined for suboptim option singlepar=T.')


abstol=var_pickfirstnotnull(o$control$abstol,o$abstol,0); #abstol can be negative for reverse-elimination when a small decline in performance may be acceptable in favor of fewer features.
stopfif(abstol<0 && isforward, 'In forward selection, abstol must be positive. Using more features with a sacrifice in accuracy does not make sense.');


stats=data.frame(); #we'll save the stats in a table.

# ---
if(o$usepargroups){
  stopfif(is.null(o$pargroups),'You must provide pargroups if you want to usepargroups=T');
  pargroups=as.list(o$pargroups);
}
else{
  pargroups=as.list(forlength(par));
}

# ---- Identify locked params and set ISselected for any preselected entries.
o$lockzeropars=var_pickfirstnotnull(o$lockzeropars,isreverse);
warnfif(!o$lockzeropars&&!o$suboptim,"Not locking zeropars (i.e., exploration of their addition or removal) only makes sense if you also use suboptim=T");

ISlocked=rep(FALSE,length(par));
ISselected=rep(FALSE,length(par)); #the features added so far are indicated here (forward selection). For reverse elimination, the removed features are indicated here.

stopfif(o$lockzeropars&&o$locknonzeropars,'You should not set both o$lockzeropars && o$locknonzeropars.');
if(o$lockzeropars){ ISlocked[par==0] = TRUE; if(isreverse){ ISselected[par==0]=TRUE; }}
if(o$locknonzeropars){ ISlocked[par!=0] = TRUE; if(isforward){ ISselected[par!=0]=TRUE; }}
if(o$selectnonzeropars){  ISselected[par!=0]=TRUE; stopfif(!isforward,'selectnonzeropars only makes sense for forward-selection.'); }
#we don't have a selectzeropars (or removezeropars) counterpart for reverse-elimination; b/c lockzeropars accomplishes that job for reverse-elimination.


# --- Remove pargroups that are already selected (for inclusion/exclusion)
for(ig in forlength(pargroups)){
  inds=pargroups[[ig]];
  inds=inds[!ISselected[inds]]; #if a feature is already selected for inclusion(forward)/removal(reverse), we won't test it for inclusion/removal
  if(isforward){
    inds[ISlocked[inds] & (par[inds]==0)] = NULL; #if a feature is locked at zero, we will never include it (b/c its "selection" won't change it anything othe tthan zero).
  }
  else{ #isreverse
    inds[ISlocked[inds] & (par[inds]!=0)] = NULL; 
  }
  pargroups[[ig]] = inds;
}
ISempty=unlist(lapply(pargroups,isempty));
if(any(ISempty)){ pargroups=pargroups[!ISempty]; }



# -------- Base evaluation
parorig=par;
if(isforward&&o$onebyone__){ } #nothing to do. leave par=parorig;
else if(isforward){ par=rep(0,length(par)); par[ISselected]=parorig[ISselected]; }
else{  par[ISselected]=0; }

bestpar=par; besteval = fn(bestpar, ...);
stat=list(iter=0,  parindex=NA,parname='BASEEVAL__',parvalue=NA,starteval=besteval,eval=besteval,impact=NA,changeaccepted=TRUE);
stat=list_merge(stat, list_make(o$parnames,bestpar) );
stats=plyr::rbind.fill(stats,as.data.frame(stat));

bestbesteval = besteval; #we keep this, to avoid drift due to a negative abstol during reverse-elimination.
if(o$trace){ cat(paste0('Baseeval=  [',besteval,']\n')); }


#--------------------- while iterations
iter=0; maxiter=length(pargroups); maxit=var_pickfirstnotnull(o$control$maxit,o$maxit);
while(!isempty(pargroups)){
  iter=iter+1; msgfif(o$trace, 'iter %d/%d,  bestbesteval:[[ %f ]]...', iter,maxiter,bestbesteval);
  if(!is.null(maxit)&&iter>maxit){ break; }
  

  pariter=bestpar;
  gress=list();

  activepargroups=rep(TRUE,length(pargroups));
  if(!isempty(o$pardepends)){
    for(ig in forlength(pargroups)){
      inds=pargroups[[ig]];
      Iremove=rep(FALSE,length(inds));
      for(iind in forlength(inds)){
        ind=inds[[iind]];
        depinds=o$pardepends[[ind]];
        depinds=base::setdiff(depinds,inds); #if they are within the current pargroup, they are subject to change, so don't check if they are/willbe zero.
        if(length(depinds)){
          if(isforward && any(!ISselected[depinds])){ Iremove[[iind]]=TRUE; }
          else if(isreverse && any(ISselected[depinds])){ Iremove[[iind]]=TRUE; }
        }
      }
      if(any(Iremove)){ 
        inds=inds[!Iremove]; 
        }
      if(!length(inds)){ activepargroups[[ig]]=FALSE; gress[[ig]]=list(value=NA); }
    }
  }

  activepargroups=which(activepargroups);
  if(!length(activepargroups)){ break; }

  #------------ foreach pargroup
  sfs_evalithfeature=function(iga,...){
  #for(iga in forlength(activepargroups)){
    ig=activepargroups[[iga]];
    inds=pargroups[[ig]];

    parg=pariter;
    if(isforward&&o$onebyone__){ } #nothing to do. leave parg=pariter;
    else if(isforward){ parg[inds]=parorig[inds]; } #pariter & parg have zeros for all candidates. copy the original for evaluation (or as starting point for suboptim)
    else{ parg[inds]=0; } #isreverse.


    Ioptim=c();
    if(o$suboptim){
      ISoptim=rep(FALSE,length(par));
      if(isforward){
        ISoptim[inds]=TRUE;
        if(!osuboptim$singlepar){ ISoptim[ISselected & (!ISlocked)]=TRUE; }
      }
      else{ #isreverse
        ISoptim[!ISselected & (!ISlocked)]=TRUE;
        ISoptim[inds]=FALSE;
      }
      Ioptim=which(ISoptim);
    }
    #warnf('optim_sfs(): REMEMBER TO REMOVE THIS LINE'); browser();
    if(!length(Ioptim)){
      value = fn(parg,...);
    }
    else{
      osub=osuboptim;
      if(!is.null(o$parnames)){ osub$parnames=o$parnames[Ioptim]; }
      if(length(osub$lower)>1){ osub$lower=osub$lower[Ioptim]; }
      if(length(osub$upper)>1){ osub$upper=osub$upper[Ioptim]; }
      if(!is.null(osub$parsigns)){ osub$parsigns=osub$parsigns[Ioptim]; }
      if(length(Ioptim)==1&&osub$method=='Nelder-Mead'){ osub$method='Brent'; } #for single-parameter optimization, optim() recommends Brent instead of Nelder-Mead.
      subpar=parg[Ioptim];
      #browser();
      if(var_tobool(osub$jitter)){
        subpar=jitter(subpar,amount=osub$jitter);
        if(!is.null(osub$lower)||!is.null(osub$upper)){
          if(length(osub$lower)==1){ subpar[subpar<osub$lower]=osub$lower; }
          else if(length(osub$lower)>1){ I=subpar<osub$lower; subpar[I]=osub$lower[I]; }
          if(length(osub$upper)==1){ subpar[subpar>osub$upper]=osub$upper; }
          else if(length(osub$upper)>1){ I=subpar>osub$upper; subpar[I]=osub$upper[I]; }
        }
      }
      fnsub=function(subpar_,...){ superpar=parg; superpar[Ioptim]=subpar_; return(fn(superpar,...)); };
      if(!exists('myoptim')){ source_disabled__('optim.r'); }
      #if(any(par>o$upper|par<o$lower)){ browser(); }
      subres=myoptim(subpar,fnsub,osub);
      value=subres$value;
      parg[Ioptim]=subres$par;
    }
    msgfif(o$trace, 'iter %d/%d, par %d/%d, eval: %f    %s',iter,maxiter,ig,length(pargroups),value,str_csv(o$parnames[inds]));
    gres=list(value=value,par=parg);
    #the following are alternative lines for straight for-loop vs. parallel/forloop function run. The for loop is more useful for debugging.
    #gress[[ig]]=gres; } #for loop
    return(gres);  }; gress[activepargroups] = list_map(forlength(activepargroups),sfs_evalithfeature,list(doparallel=o$doparallel),...);
     #return(gres);  }; for(iga in forlength(activepargroups)){ gress[[activepargroups[[iga]]]]=sfs_evalithfeature(iga,...); }
  
  
  evals=unlist(lists_extractfield(gress,'value'));
  if(all(is.na(evals))){ accept=F; }
  else{
    if(o$maximize){ bestig=which.max(evals); }
    else{ bestig=which.min(evals); }
    eval=evals[[bestig]];
    #accepting compares with bestbesteval, but updates besteval
    accept=(isforward && optim_isbettereval(eval,bestbesteval,o$maximize,abstol)) || (isreverse && optim_isequalorbettereval(eval,bestbesteval,o$maximize,abstol));
  }
  
  for(ig in forlength(pargroups)){
    inds=pargroups[[ig]];
    gres=gress[[ig]];
    if(is.na(gres$value)){ next; }
    stat=list(iter=iter,parindex=str_csv(inds),parname=str_csv(o$parnames[inds]),parvalue=str_csv(gres$par[inds]),starteval=besteval,eval=gres$value, impact=var_pick(o$maximize,1,-1)*var_pick(isreverse,-1,1)*(gres$value-besteval));
    stat[['changeaccepted']]=accept && ig==bestig;
    stat=list_merge(stat, list_make(o$parnames,gres$par) );
    stats=plyr::rbind.fill(stats,as.data.frame(stat));
  }
  inds=pargroups[[bestig]];
  gres=gress[[bestig]];
  if(accept){
    msgfif(o$trace,'iter %d/%d, parind %s, ACCEPTED: [[ %f ]] <== %f  (impact:%f)  %s (parvalue %f <-- %f)',iter,maxiter,str_csv(inds),gres$value,besteval,var_pick(o$maximize,1,-1)*var_pick(isreverse,-1,1)*(gres$value-besteval), str_csv(o$parnames[inds]), str_csv(gres$par[inds]), str_csv(pariter[inds]));
    if(any(gres$par>o$upper|gres$par<o$lower)){ browser(); }
    
    bestpar=gres$par; besteval=gres$value;
    
    acceptbestbest=(isforward && optim_isbettereval(besteval,bestbesteval,o$maximize,abstol)) || (isreverse && optim_isequalorbettereval(besteval,bestbesteval,o$maximize,abstol));
    if(acceptbestbest){ bestbesteval=besteval; }

    if(o$onebyone__){ next; } #don't change ISselected or pargroups; we'll check everything one-by-one again in the next iteration.
    ISselected[inds]=TRUE;
    pargroups[bestig]=NULL; #this removes the item from pargroups.
    #also remove any other pargroup if any of its entries are already in ISselected.
    Iremove=rep(F,length(pargroups));
    for(ig in forlength(pargroups)){
      if(any(ISselected[ pargroups[[ig]] ])){ Iremove[[ig]]=TRUE; }
    }
    if(any(Iremove)){ pargroups[Iremove]=NULL; }
  }
  else{ #!accept.
    msgfif(o$trace,'iter %d/%d, parind %s, REJECTED: [[ %f ]] <== %f (impact:%f)   %s (parvalue %f <-- %f)',iter,maxiter,str_csv(inds),eval,besteval,var_pick(o$maximize,1,-1)*var_pick(isreverse,-1,1)*(eval-besteval), str_csv(o$parnames[inds]), str_csv(gres$par[inds]), str_csv(pariter[inds]));
    break; } 
}#while~true
res=list(par=bestpar, value=besteval, stats=stats);
return(res);
}
