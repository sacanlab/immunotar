#% Copyright (C) 2023 by Ahmet Sacan

#just a function that negates the result of fn. Used as a way to maximize (rather than minimize fn). (control$fnscale=-1 may be a better choice for this.)
###############################################################

myoptim=function(par,fn,o=list(),method=NULL,trace=NULL,...){
#  source('util.r');
  o = list_merge(list(
    method=NULL #optimization method. checkzeros|null=Nelder-Mead|SANN|BFGS|CG|-BFGS-B. Can be a list of methods if you want to perform them in serial.
    ,maximize=NULL #default to o$control$fnscale<0.
    ,trace=0 #for printing progress, use trace=1.
    ,parnames=NULL #used for trace printing.
    ,methodcontrols=NULL #method-specific control options. e.g., provide maxit for SAAN but reltol for Nelder-Mead
  ),o,list(...));
  if(!is.null(method)) o$method=method;
  if(!is.null(trace)) o$trace=trace;


  if(is.null(o$maximize)) o$maximize=!is.null(o$control)&&!is.null(o$control$fnscale)&&o$control$fnscale<0;

  if(length(o$method)>1){
    for(method in o$method){
      o$method=method;
      res=myoptim(par,fn,o,...);
      par=res$par;
    }
    return(res);
  }
  if(!is.null(o$method)&&o$method=='checkzeros'){
    besteval = fn(par, ...);
    if(o$trace){ cat(paste0('Baseeval=  [',besteval,']\n')); }
    for(i in 1:length(par)){
      if(par[i]!=0){
        par2=par; par2[i]=0;
        eval = fn(par2,...);
       if(o$maximize&&eval>=besteval || !o$maximize&&eval<=besteval){
          if(o$trace){
            if(is.null(o$parnames)){ cat(paste0('Changing par[',i,']: ',par[i],'-->0 results in equal or better eval: ',eval,'\n')); }
            else{ cat(paste0('Changing par[',i,']=par[',o$parnames[i],']: ',par[i],'-->0 results in equal or better eval: ',eval,'\n')); }
          }
          besteval=eval;
          par=par2;
        }
      }
    }
    return(list(par=par, value=besteval))
  }
  else{
    if(o$maximize && (is.null(o$control)||is.null(o$control$fnscale))){
      o$control$fnscale=-1;
    }
    # if any of these options are given in o$..., copy them into o$control$...
    ocontrolnames=c('trace','reltol','abstol','maxit');
    #reltol - a value by which the next iteration changes, default set to 1e-8. Set to 0.1
    #maxit - the maximum number of iteration to be completed 
    #abstol - 
    #trace - follow the tracing 
    o$control = list_merge(list_selectfields(o,ocontrolnames),o$control);
    if(!is.null(o$methodcontrols)){
      if(o$method %in% names(o$methodcontrols)){
        o$control = list_merge(o$control, o$methodcontrols[[o$method]])
      }
      # if o$methodcontrols[o$method] is available, use:
      # o$control = list_merge(o$control, o$methodcontrols[o$method]); 
    }
    return(optim(par,fn, method=o$method, control=o$control, ...))
  }
}

###############################################################
#Determine the ranges of parameter values that do not affect the evaluation. If we discover better evaluation, we'll update the param.
myoptim_getparranges=function(par,fn,o=list(),...){
#  source('util.r');
  o = list_merge(list(
    maximize=F #default to o$control$fnscale<0.
    ,parnames=NULL
    ,stepsize='1%' #can be a number or a text containing '%', which becomes a fraction of each parameter value -- if parameter is zero, we use fraction out of 1.0.
    ,maxsteps=100 #maximum number of steps in either direction.
    ,repeatifimproved=T #if we discover an improvement, repeat scanning all parameters again.
    ,trace=0 #whether to print progress messages.
  ),o,list(...));

  d = as.data.frame(matrix(NaN,nrow=length(par),ncol=6));
  colnames(d)=c('baseval','baseeval','newval','neweval','minval','maxval')
  if(!is.null(o$parnames)){ rownames(d)=o$parnames; }
  else{ o$parnames = 1:length(par); }

  while(T){  #we'll break if !improved
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
      par2=par;
      for(dir in c(+1,-1)){ #up or down direction
        for(s in 1:o$maxsteps){
          par2[pi] = baseval + dir*s*stepsize;
          testedvals = c(testedvals,par2[pi]);
          eval = fn(par2,...);
          if((o$maximize && eval > neweval) || (!o$maximize && eval < neweval)){
            if(o$trace){ cat(paste0('Discovered better eval by changing par[',pi,']=par[',o$parnames[pi],']: ',minval,'-->',par2[pi],'. better eval: ',eval,'\n')); }
            minval=par2[pi]; maxval=par2[pi];
            par[pi]=par2[pi];
            neweval=eval;
            improved=T;
          }
          else if((o$maximize && eval < neweval) || (!o$maximize && eval > neweval)){
            break;
          }
          else{  #else: eval is equal to neweval.
            if(dir==1){ maxval=par2[pi]; } else{ minval=par2[pi]; }
          }
        }#foreach step
        if(dir==1 && minval!=baseval){ break; }
      }#for up/down direction
      goodvals = sort(testedvals[testedvals >= minval & testedvals <= maxval]);
      newval = goodvals[ceiling(length(goodvals)/2)];
      if(!length(newval)){ print(list(baseval=baseval,minval=minval,maxval=maxval,newval=newval,baseeval=baseeval,neweval=neweval,testedvals=testedvals,goodvals=goodvals)); }
      if(is.na(d[pi,'baseval'])){ d[pi,c('baseval','baseeval')]=c(baseval,baseeval); }
      d[pi,c('newval','neweval','minval','maxval')] = c(newval,neweval,minval,maxval);
    }#foreach par

    if(!o$repeatifimproved || !improved){ break; }
  }#while(true)
  return(d);
}
