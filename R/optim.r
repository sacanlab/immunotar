source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2023 by Ahmet Sacan

#is eval an improvement over oldeval by at least abstol?
optim_isbettereval=function(eval,oldeval,maximize,abstol=0){
  return((maximize&& eval>oldeval+abstol) || ((!maximize)&& eval<oldeval-abstol));
}
optim_isequaleval=function(eval,oldeval,maximize,abstol=0){
  return((maximize&& eval==oldeval+abstol) || ((!maximize)&& eval==oldeval-abstol));
}
optim_isequalorbettereval=function(eval,oldeval,maximize,abstol=0){
  return((maximize&& eval>=oldeval+abstol) || ((!maximize)&& eval<=oldeval-abstol));
}

###############################################################
#random restart + 
optim_rand=function(par,fn,o,...){
randscale = var_pickfirstnotnull(o$control$randscale,o$randscale, 1);
res=list(par=par, value=fn(par, ...));
maxabspar=max(abs(par));
addpar=runif(n=length(par),min=0,max=maxabspar) - maxabspar/2;
par = par+addpar*randscale;
if(enforcesign){
  par[o$parsigns>0]=abs(par[o$parsigns>0]);
  par[o$parsigns<0]=-abs(par[o$parsigns<0]);
}
#TODO: we can implement this more systematically (reuse/factor-out the similar logic in checkzeros.)
#use rand+Nelder-Mead methodcontrols where available.
o$methodcontrols[['Nelder-Mead']]=opt_set(o$methodcontrols[['Nelder-Mead']],o$control,o$methodcontrols[['rand+Nelder-Mead']]);
#remove these parameters, to avoid warning from Nelder-Mead
o$methodcontrols[['Nelder-Mead']]$randscale=NULL; o$methodcontrols[['Nelder-Mead']]$enforcesign=NULL;
o$control$randscale=NULL; o$control$enforcesign=NULL;
#msgf('Starting from randomized par: %s',par)
#msgf('Eval at randomized par: %f',fn(par,...))
res2=myoptim(par,fn, opt_set(o,method='Nelder-Mead'), ... )
if(optim_isbettereval(res2$value,res$value,o$maximize)){ res=res2; }
return(res);  
}
###############################################################
optim_checkzeros=function(par,fn,o,...){
stats=data.frame(); #we'll save the stats in a table.
besteval = fn(par, ...);
stat=list(parindex=NA,parname='BASEEVAL__',starteval=besteval,zeroeval=NA,optimeval=NA,zeroaccepted=NA,impact=NA);
stats=plyr::rbind.fill(stats,as.data.frame(stat));
bestbesteval = besteval; #we keep this, to avoid drift due to abstol
if(o$trace){ cat(paste0('Baseeval=  [',besteval,']\n')); }
abstol=0;
if(!is.null(o$control)&&!is.null(o$control$abstol)){
  abstol=o$control$abstol
  stopfif(abstol<0,'abstol must be given as a positive value.');
}
#todo: add support for pargroups=c(1,1,1,2,2,2,...) etc. so we can turn off a group of pars at a time. When not given, we can fall back to normal individual behavior by setting pargroups=1:length(par).
for(i in 1:length(par)){
  if(par[i]!=0){
    if(o$trace){ msgf('Testing checkzero for parameter %d of %d %s...',i,length(par),var_pick(is.null(o$parnames),'',paste0(' (',o$parnames[i],')'))); }
    
    par2=par; par2[i]=0;
    eval = fn(par2,...); #this will be overridden below if o$control$optim=T.
    stat=list(parindex=i,parname=var_pick(is.null(o$parnames),'',o$parnames[i]),starteval=besteval,zeroeval=eval,optimeval=NA,zeroaccepted=FALSE,impact=var_pick(o$maximize,besteval-eval,eval-besteval));

    if(var_pickfirstnotnull(o$control$suboptim,F)&&any(par2!=0)){ #optim for each set0?
      osuboptim=var_pickfirstnotnull(o$control$osuboptim,list(method="Nelder-Mead",control=list(maxit=10)));
      Inonzero=par2!=0;
      subres=myoptim(par2[Inonzero],function(par2sub,...){
          par3=par2; par3[Inonzero]=par2sub;
          return(fn(par3,...)); },opt_set(list_removefields(o,'methodcontrols','control'),osuboptim),...);
      eval=subres$value;
      par2[Inonzero]=subres$par;
      stat[['optimeval']]=eval;
      stat[['optimimpact']]=var_pick(o$maximize,stat[['starteval']]-eval,eval-stat[['starteval']]);
    }

    #if there is improvement in eval, keep.
    if(optim_isequalorbettereval(eval,bestbesteval,o$maximize,abstol)){
      if(o$trace){
        msgf('Changing par[%d]%s: %f -->0 results in equal or better eval: %f --> %f (abstol=%f, maximize=%d)',i,var_pick(is.null(o$parnames),'',o$parnames[i]),par[i],besteval,eval,abstol,o$maximize);
      }
      par=par2;
      besteval=eval;
      stat$zeroaccepted=TRUE;
      if(o$maximize){ bestbesteval=max(bestbesteval,besteval); }
      else{ bestbesteval=min(bestbesteval,besteval); }
    }
    stats=plyr::rbind.fill(stats,as.data.frame(stat));
  }
}
res=list(par=par, value=besteval, stats=stats);
return(res);
}

###############################################################
optim_ga=function(par,fn,o,...){
installpackageifmissing('GA')
library(GA)
lower = var_pickfirstnotnull(o$control$lower,o$lower);
upper = var_pickfirstnotnull(o$control$upper,o$upper);
popSize = var_pickfirstnotnull(o$control$popSize,o$popSize,10);
maxit = var_pickfirstnotnull(o$control$maxiter,o$control$maxit,o$maxiter,o$maxit,10);
stalerun = var_pickfirstnotnull(o$control$stalerun,o$stalerun,3);#this is a convergence criteria: the maximum #of consecutive iters with no improvement 
parallel = var_pickfirstnotnull(o$control$parallel,o$parallel,F); #I wasn't happy with the lag in parallel when i tried. so, leaving it false by default.
includeinitial = var_pickfirstnotnull(o$control$includeinitial,o$includeinitial,F); #whether to include the initial par in the first population. Not Recommended for small populations, b/c the initial par may dominate and the population may not get a chance to evolve beyond it.
pmutation = var_pickfirstnotnull(o$control$pmutation,o$pmutation,GA::ga_pmutation); #when I tried 0.1,0.2,etc., it did not improve the population as much.
suggestions = var_pickfirstnotnull(o$control$suggestions,o$suggestions,NULL);
jittersuggestions = var_pickfirstnotnull(o$control$jittersuggestions,o$jittersuggestions,0);

if(includeinitial){ suggestions=rbind(suggestions,par); }
loadpopulation=var_pickfirstnotnull(o$control$loadpopulation,o$loadpopulation,F);
savepopulation=var_pickfirstnotnull(o$control$savepopulation,o$savepopulation,F);

if(var_tobool(loadpopulation)&&!is.character(loadpopulation)){
    stopfif(!var_tobool(savepopulation)||!is.character(savepopulation),"loadpopulation must be a filename, preferably ending with fst.")
    loadpopulation=savepopulation;
}
if(var_tobool(savepopulation)&&!is.character(savepopulation)){
    stopfif(!var_tobool(loadpopulation)||!is.character(loadpopulation),"savepopulation must be a filename, preferably ending with fst.")
    savepopulation=loadpopulation;
}
if(var_tobool(loadpopulation)){
  if(io_isfile(loadpopulation)){
    pop=as.matrix(data_readfile(loadpopulation));
    suggestions=rbind(suggestions,pop);
  }
  else{ warnf('Loadpopulation file [%s] does not exist. Continuing without it...',loadpopulation); }
}
if(!isempty(suggestions)){
  if(isempty(popSize)){ popSize=50; }
  if(nrow(suggestions)>popSize){
    warnf('myoptim(): ga: Number of suggestions [%d] larger than popSize [%d]. Only keeping top performers. Increase popSize if you want to keep them all.',nrow(suggestions),popSize);
    suggestions=as.matrix(suggestions);
    sugscores=unlist(list_map(1:nrow(suggestions), function(x){ fn(suggestions[x,],...); }),list(doParallel=parallel));
    if(o$maximize) {sugscores=-sugscores;}
    suggestions=suggestions[order(sugscores)[1:popSize],];
  }
}
if(var_tobool(jittersuggestions)&&!isempty(suggestions)){
  suggestions=jitter(suggestions,amount=jittersuggestions);
}


#msgf(list(lower=lower,upper=upper))
if(!exists('myga')){ source('ga.R'); } #ahmet: I downloaded a copy of ga.R so I can debug why parallel cluster is not working.
g=myga(type = "real-valued",fitness=function(x){ ret=fn(x,...); return(var_pick(o$maximize,1,-1)*ret); }
  ,lower=lower,upper=upper
  ,popSize = popSize
  ,maxiter = maxit
  ,run = stalerun
  ,monitor = var_pick(o$trace,gaMonitor,F)
  ,parallel = parallel
  ,pmutation = pmutation
  ,suggestions =suggestions
  ,optim = var_pickfirstnotnull(o$control$optim,F)
  #this is the default osuboptim listed on https://search.r-project.org/CRAN/refmans/GA/html/ga.html
  ,optimArgs=var_pickfirstnotnull(o$control$osuboptim, list(method="L-BFGS-B",poptim = 0.05,pressel=0.5,control = list(fnscale=-1,maxit=100))
  ))
res=list(par=g@solution[1,],value=g@fitnessValue, ga=g);
if(o$trace&&nrow(g@summary)>1){ 
  tryCatch({ GA::plot(g); }, error=function(e){ warnf('Got eror while trying to plot GA:'); print(e); });
}
if(var_pickfirstnotnull(o$noworsethaninit,T)){
  value=fn(par, ...);
  if(optim_isbettereval(value,res$value,o$maximize)){ res=list(par=par,value=value, ga=g,gapar=res$par,gavalue=res$value); }
}
if(var_tobool(savepopulation)){ data_writefile(as.data.frame(g@population),savepopulation);   }
  return(res);
}

###############################################################
#return true if the method requires lower/upper args, or if it is a method that supports it when lower/upper args are given.
optim_isboxconstraintmethod=function(method,lower=NULL,upper=NULL){
  return(str_incsv('L-BFGS-B,Brent',method) || (str_incsv('Nelder-Mead',method)&&(!is.null(lower)||!is.null(upper))));
}
#just a function that negates the result of fn. Used as a way to maximize (rather than minimize fn). (control$fnscale=-1 may be a better choice for this.)
###############################################################
#provide the options for myoptim() in o. provide fn arguments as the rest (...)
myoptim=function(par,fn,o=NULL,...){
  source_disabled__('util.r');
  stopfif(isempty(par),'Empty parameter vector. nothing to optimize.');
  o=opt_set(
    method=NULL #optimization method. checkzeros|sfs|null=Nelder-Mead|SANN|BFGS|CG|-BFGS-B. Can be a list of methods if you want to perform them in serial.
    ,maximize=NULL #defaults to o$control$fnscale<0.
    ,mineval=-Inf #used with enforcesign. Providing better bounds on the min/max possible evaluation is recommended (when known) to help optimization behave more gracefully.
    ,maxeval=Inf
    ,trace=0 #for printing progress, use trace=1.
    ,parnames=NULL #used for trace printing.
    ,methodcontrols=NULL #method-specific control options. e.g., provide maxit for SAAN but reltol for Nelder-Mead
    ,enforcesign=F #whether to enforce +/- sign of the parameters. Implemented by intercepting fn and returning inf/-inf when there is a violation.
    ,parsigns=NULL #used with enforcesign. When not provided, we obtain it from the current par signs.
    ,parcost=NULL #if you want to penalize for a par usage (to maximize the number of 0's in par), provide here the penalty per non-zero par. parcost is assumed to be the total across all par's. Use parcosteach to specify per-par.
    ,parcosteach=NULL #Single number or vector of cost values for each par.
    ,checkfilestop=F #if true, we check for a file called myoptim_stop, when present, we make the evaluation function return worst eval, without calling the actual fn.
    ,noworsethaninit=T #for some methods (ga), should we compare with the initial parameter's performance and return the higher of that vs. optimized params? will default to True for the last method we are running.
  ,o);

  if(is.null(o$maximize)) o$maximize=!is.null(o$control)&&!is.null(o$control$fnscale)&&o$control$fnscale<0;
  if(o$maximize && (is.null(o$control)||is.null(o$control$fnscale))){
    o$control$fnscale=-1;
  }
  isfnscalemaximize=o$control$fnscale<0;
  stopfif(xor(o$maximize,isfnscalemaximize),"Conflicting maximize [%d] and control$fnscale [%d] options.",o$maximize,o$control$fnscale);
  stopfif(any(is.na(par)),'One or more of the initial parameter values is NA.');
  

  if(length(o$method)>1){
    methods=o$method;
    for(i in 1:length(methods)){
      o$method=methods[[i]];
      msgfif(o$trace,'============= [[ method %d of %d: %s ]] ============',i,length(methods),o$method);
      res=myoptim(par,fn,o,...);
      par=res$par;
    }
    return(res);
  }
  if(o$trace){ tictoc::tic(); }

  # if any of these options are given in o$..., copy them into o$control$...
  ocontrolnames=c('trace','reltol','abstol','maxit','lower','upper');
  #reltol - a value by which the next iteration changes, default set to 1e-8. Set to 0.1
  #maxit - the maximum number of iteration to be completed 
  #abstol - 
  #trace - print information during iterations
  o$control = list_merge(list_selectfields(o,ocontrolnames),o$control);
  if(!is.null(o$methodcontrols)){
    if(o$method %in% names(o$methodcontrols)){
      o$control = list_merge(o$control, o$methodcontrols[[o$method]])
    }
  }
  

  fnorig=fn; fnmodified=F;

  #----------------- Minimize parameter-cost by penalizing parameter-count and magnitude.
  parcost = var_pickfirstnotnull(o$control$parcost,o$parcost);
  parcosteach = var_pickfirstnotnull(o$control$parcosteach,o$parcosteach);
  if(!is.null(parcost)&&!is.null(parcosteach)){ stopf('You must provide only one of parcost (total cost for all pars) or parcosteach.'); }
  if(!is.null(parcost)){ parcosteach = parcost / length(par); }
  if(!is.null(parcosteach)&&!all(parcosteach==0)){
    if(!o$maximize){ parcosteach=-parcosteach; }
    #msgf('parcosteach: %f',parcosteach);
    oldfn2=fn; fnmodified=T;
    fn=function(par,...){
      return( oldfn2(par,...) - sum(parcosteach*(par!=0)) );
    }
  }


  #----------------- Check filestop
  checkfilestop = var_pickfirstnotnull(o$control$checkfilestop,o$checkfilestop);
  if(o$checkfilestop){
    oldfn3=fn; fnmodified=T;
    fn=function(par,...){
      if(file.exists('myoptim_stop')){
        return(var_pick(o$maximize,o$mineval,o$maxeval));
      }
      return(oldfn3(par,...));
    }
  }

  #----------------- enforcesign by returning Inf/-Inf for violations. (Not recommended since it's not continuous; you are better off using a box-constrained optimization method)
  enforcesign = var_pickfirstnotnull(o$control$enforcesign,o$enforcesign, F);
  if(var_tobool(enforcesign)&&is.null(o$parsigns)){
    #o$parsigns=sign(par); no need to keep sign, just use the actual numbers?
    o$parsigns=sign(par);
  }
  #if the method can internally support box-constraints, don't use an enforcesign wrapper function.
  if(var_tobool(enforcesign) && optim_isboxconstraintmethod(o$method,TRUE,TRUE)){
    oldfn1=fn; fnmodified=T;
    fn=function(par,...){
      if(any(par*o$parsigns < 0)){
        #print(data.frame(par=par,signpar=sign(par),enforcedsigns=o$parsigns))
        #warnf('myoptim() eval intervention: Returning inf b/c of enforcesign.')
        return(var_pick(o$maximize,o$mineval,o$maxeval));
      }
      ret=oldfn1(par,...);
      return(ret);
    }
  }

  if(is.null(o$parnames)){ o$parnames=paste0('par',1:length(par)); }

  #------------------------- Setting up Lower/Upper (when they are given, or when a method (e.g., ga) requires it)
  lower = var_pickfirstnotnull(o$control$lower,o$lower);
  upper = var_pickfirstnotnull(o$control$upper,o$upper);
  if(!is.null(lower)||!is.null(upper) || optim_isboxconstraintmethod(o$method,lower,upper)){
    minpar=min(par);
    maxpar=max(par);
    parrange=maxpar-minpar; parrange=var_pick(parrange,parrange,max(abs(maxpar),1)); #if parrange=0, default to 1.
    if(is.null(lower)){ lower=minpar-parrange; }
    if(is.null(upper)){ upper=maxpar+parrange; }
    if(length(lower)==1&&length(par)>1){ lower=rep(lower,length(par)); }
    if(length(upper)==1&&length(par)>1){ upper=rep(upper,length(par)); }
    if(enforcesign){ #when enforcesign=T, o$parsigns becomes available (checked and default'ed above)
      lower[(o$parsigns>0)&(lower<0)]=0;
      lower[(o$parsigns<0)&(lower>0)]=-parrange;
      upper[(o$parsigns>0)&(upper<0)]=parrange;
      upper[(o$parsigns<0)&(upper>0)]=0;
      warnfif(any((upper-lower)==0),'Using parsigns resulted in some parameters to be bounded at [0,0] they will not be modified during the optimization.');
      #print(data.frame(par=par,parsign=o$parsigns,lower=lower,upper=upper))
      #stop();
    }
  }
  if(!is.null(lower)&&is.null(upper)){ upper=Inf; }
  if(is.null(lower)&&!is.null(upper)){ lower=-Inf; }
  if(!is.null(lower)&&length(lower)==1&&length(lower)<length(par)){ lower=rep(lower,length(par)); }
  if(!is.null(upper)&&length(upper)==1&&length(upper)<length(par)){ upper=rep(upper,length(par)); }
  #msgf(list(lower,upper));
  if(!is.null(lower)){
    I=par<lower;
    if(any(I)){
      warnf('Parameter(s) %s had initial value(s) %s, which is lower than the lower bound %s. Changing them to lower bound before running optimization ...',str_csv(o$parnames[I]),str_csv(par[I]),str_csv(lower[I]))
      par[I]=lower[I];
    }
  }
  if(!is.null(upper)){
    I=par>upper;
    if(any(I)){
      warnf('Parameter(s) %s had initial value(s) %s, which is higher than the upper bound %s. Changing them to upper bound before running optimization ...',str_csv(o$parnames[I]),str_csv(par[I]),str_csv(upper[I]))
      par[I]=upper[I];
    }
  }


  #-------------------- Method selector
  if(o$method=='none'){
    #print(list_make(o$parnames,par))
    res=list(par=par); #the value is filled in below where we check if fnmodified. 
  }
  else if(o$method=='checkzeros'){
    res=optim_checkzeros(par,fn,o,...);
  }
  else if(o$method=='sfs'){
    if(!exists('optim_sfs')){ source_disabled__('optim_sfs.r'); }
    o$control$lower=lower; o$control$upper=upper;
    res=optim_sfs(par,fn,o,...)
  }
  else if(o$method=='onebyone'){
    if(!exists('optim_onebyone')){ source_disabled__('optim_sfs.r'); }
    o$control$lower=lower; o$control$upper=upper;
    res=optim_onebyone(par,fn,o,...)
  }
  else if(o$method=='parranges'){
    o$control$lower=lower; o$control$upper=upper;
    res=optim_parranges(par,fn,o,...);
  }
  else if(o$method=='rand'){
    o$control$lower=lower; o$control$upper=upper;
    res=optim_rand(par,fn,o,...);
  }
  ## GA logic is implemented here
  else if(tolower(o$method)=='ga'){
    o$control$lower=lower; o$control$upper=upper;
    res=optim_ga(par,fn,o,...);
  }
  #
  else if(str_incsv('L-BFGS-B',o$method)){
    msgf('Using the box-constrained method %s with lower=%s and upper=%s',o$method,str_csv(lower),str_csv(upper));
    installpackageifmissing('optimx');
    res=optimx::optimx(par,fn,method=o$method, control=list_removefields(o$control,'lower','upper'),lower=lower,upper=upper, ...);
    #optimx gives a dataframe with p1, p2,etc. for pars. Converting it to a list format as optim() does.
    stopfif(nrow(res)!=1,'Expecting the result to be a single row, but got: %s',res);
    res=as.list(res);
    res$par=unlist(res[grepl('^p\\d+$',names(res))]);
  }
  else if(o$method=='Nelder-Mead'&&(!is.null(lower)||!is.null(upper))){
    msgf('Using the box-constrained method %s with lower=%s and upper=%s',o$method,str_csv(lower),str_csv(upper));
    installpackageifmissing('lme4');
    o$control=list_renamefields(o$control,'trace','verbose', 'reltol','FtolRel', 'abstol','FtolAbs');
    o$control=list_removefields(o$control,'fnscale');
    oldfn=fn;
    fn=function(par){ return(var_pick(o$maximize,-1,1)*oldfn(par,...)); }
    res=lme4::Nelder_Mead(fn,par,control=list_removefields(o$control,'lower','upper'),lower=lower,upper=upper)
    res=list_renamefields(res,'fval','value');
  }
  else{
    if(!is.null(lower)&&!is.null(upper)){ res=optim(par,fn, method=o$method, control=list_removefields(o$control,'lower','upper'),lower=lower,upper=upper,...); }
    else{ res=optim(par,fn, method=o$method, control=list_removefields(o$control,'lower','upper'),...); }
  }

  #if we modified fn, we need to re-evaluate with the original fn.
  if(fnmodified||o$method=='none'){
    #msgf('Rerunning with the original eval fn...');
    res$value=fnorig(res$par, ...);
  }

  
  if(o$trace){ msgf('============= [[ %s: Optimized score: %f ]] ============ (time: %s)',o$method,res$value,tictoc_pretty(print=F)); }
  return(res)
}


###############################################################
#Determine the ranges of parameter values that do not affect the evaluation. If we discover better evaluation, we'll update the param.
optim_parranges=function(par,fn,o,...){
  o=opt_set(
    maximize=F #default to o$control$fnscale<0.
    ,parnames=NULL
    ,stepsize='1%' #can be a number or a text containing '%', which becomes a fraction of each parameter value -- if parameter is zero, we use fraction out of 1.0.
    ,maxsteps=100 #maximum number of steps in either direction.
    ,repeatifimproved=T #if we discover an improvement, repeat scanning all parameters again.
    ,maxit=10 #if repeatifimproved, how many repetitions at most?
    ,trace=0 #whether to print progress messages.
    ,lower=NULL
    ,upper=NULL
    ,abstol=0
    ,abstol_accept=NULL #minimum improvement to accept an eval better and change par's for it. defaults to abstol. Must be positive.
    ,abstol_walk=NULL ##what level of improvement is considered keep-walking? you can set this negative if you want to extend even if the performance is degrading.  defaults to abstol.
    ,centerinrange=T #if true, we center a parameter within the range (Only applies to params that do not improve the eval). even when false, if we change a parameter to a better eval, we move that param to centerinrange.
  ,o,...);
  o=opt_set(o,o$control);

  lower=o$lower; upper=o$upper;
  if(!is.null(lower)&&length(lower)==1&&length(lower)<length(par)){ lower=rep(lower,length(par)); }
  if(!is.null(upper)&&length(upper)==1&&length(upper)<length(par)){ upper=rep(upper,length(par)); }


  d = as.data.frame(matrix(NaN,nrow=length(par),ncol=8));
  colnames(d)=c('baseeval','neweval','improvement','baseval','newval','minval','maxval','valrange')
  if(!is.null(o$parnames)){ rownames(d)=o$parnames; }
  else{ o$parnames = 1:length(par); }
  parorig=par;
  abstol=o$abstol;
  abstol_accept=var_pickfirstnotnull(o$abstol_accept,abstol); stopfif(abstol_accept<0,'abstol_accept must be positive.');
  abstol_walk=var_pickfirstnotnull(o$abstol_walk,abstol);

  iter=0;
  while(T){  #we'll break if !improved or iter>maxit, whichever comes first.
    if(iter>o$maxit){ break; }
    iter=iter+1;
    
    improved=F;
    for(pi in 1:length(par)){
      baseval=par[pi];
      baseeval = fn(par,...);
      stepsize=o$stepsize;
      if(is.character(stepsize)){
        if(!endsWith(stepsize,'%')){ stop('When stepsize is a text, it should have the percent sign, e.g., "1%"'); }
        stepsize=as.numeric(gsub(' *%$','',stepsize))*0.01;
        if(baseval!=0){ stepsize=abs(baseval)*stepsize; }
      }
      minval=baseval;
      maxval=baseval;
      neweval=baseeval;
      testedvals=c(baseval);
      testedevals=c(baseeval);
      par2=par;
      for(dir in c(+1,-1)){ #up or down direction
        for(s in 1:o$maxsteps){
          par2[pi] = baseval + dir*s*stepsize;
          if(!is.null(lower)&&par2[pi]<lower[pi]){ if(dir==-1){break;}; next; }
          if(!is.null(upper)&&par2[pi]>upper[pi]){ if(dir==1){break;}; next; }

          eval = fn(par2,...);
          testedvals = c(testedvals,par2[pi]);
          testedevals = c(testedevals,eval);
          if((o$maximize && eval > neweval+abstol_accept) || (!o$maximize && eval < neweval-abstol_accept)){
            if(o$trace){ cat(paste0('Discovered better eval by changing par[',pi,']=par[',o$parnames[pi],']: ',minval,'-->',par2[pi],'. better eval: ',eval,'\n')); }
            minval=par2[pi]; maxval=par2[pi];
            par[pi]=par2[pi];
            neweval=eval;
            improved=T;
          }
          else if((o$maximize && eval >= neweval+abstol_walk) || (!o$maximize && eval <= neweval-abstol_walk)){  #else: eval is equal (or equivalent) to neweval.
            if(dir==1){ maxval=par2[pi]; } else{ minval=par2[pi]; }
          }
          else{  #if((o$maximize && eval < neweval) || (!o$maximize && eval > neweval)){
            break;
          }
        }#foreach step
        if(dir==1 && minval!=baseval){ break; }
      }#for up/down direction
      if(o$centerinrange || par[pi]!=parorig[pi]){
        goodvals = sort(testedvals[testedevals == neweval]);
        newval = goodvals[ceiling(length(goodvals)/2)];
        if(!length(newval)){ print(list(baseval=baseval,minval=minval,maxval=maxval,newval=newval,baseeval=baseeval,neweval=neweval,testedvals=testedvals,goodvals=goodvals)); }
        par[pi]=newval;
      }
      else{
        newval=par[pi];
      }
      if(is.na(d[pi,'baseval'])){ d[pi,c('baseval','baseeval')]=c(baseval,baseeval); }
      d[pi,c('neweval','improvement','newval','minval','maxval')] = c(neweval,var_pick(o$maximize,1,-1)*(neweval-baseeval),newval,minval,maxval,maxval-minval);
    }#foreach par

    if(!o$repeatifimproved || !improved){ break; }
  }#while(true)

  return(list(value=neweval, par=par, stats=d));
}



#stk__=dbg_nicestack(1); message(sprintf('optim.r sourced from: %s',stk__));
