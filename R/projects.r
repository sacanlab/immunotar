#% Copyright (C) 2023 by Ahmet Sacan
#Functions for running and multiple projects to study or optimize the weights.
#source('util.r')
#source('myoptim.r')
#source('project.r')

#this is a helper function for projects_run_ to run for a single project.
# Extracted here into a separate function so we can more easily run in parallel.

projects_run_single=function(p,requireknowntargets=T,runonce=F,...){
#  source('project.r')
  if(requireknowntargets && is.null(p['knowntargets'])){ stop('The projects you provide need to have knowntargets'); }
  if(!runonce || !('score' %in% names(p))){
    p=project_runfull(p, outfile=NULL,...);
  }
  return(p);
}

##use this to run each project once, to ensure their data is prepared.

#' projects_run is used to generate the IMMUNOTAR score for  each protein similar to project_run however, this is when you have multiple different cancer expression datasets. These datasets would have to be specified separately in the yaml file or in the project structure within R. This function will run through each project separately and project a structure with the outputs for each project. 
#' To make this function run faster, it uses parallel computing. You can disable this by setting doparallel=F. You can select the number of cores using the option numcores= OR the default is 2 less than the total cores on your computer. 

#' @export
projects_run=function(ps,requireknowntargets=T,runonce=F,doparallel=T,...){
  if(is.namedlist(ps)){ ps=list(ps); }
  #print(paste0('projects_run::21:args: ',list(...)))
  ps = list_map(ps,projects_run_single,requireknowntargets=requireknowntargets,runonce=runonce,doparallel=doparallel,...);
  return(ps);
}

#use this to run each project once, to ensure their data is prepared.

projects_runonce=function(ps,requireknowntargets=T,doparallel=T,...){  
  return(projects_run(ps,requireknowntargets,runonce=T,doparallel=doparallel,...));
}

projects_prepare=function(ps,doparallel=T,...){
  if(is.namedlist(ps)){ ps=list(ps); }
  ps = list_map(ps,project_prepare,doparallel=doparallel,...);
  return(ps);
}


#projects may have different columns. use this to get a merged list of weights.
#you must call projects_runonce() before calling this function.

projects_getweightsandcurves=function(ps,o=list()){
  o = list_merge(list(
    weights=NULL
    ,curves=NULL
  ),o);
  if(is.null(o$weights)||is.null(o$curves)){
    for(pi in 1:length(ps)){
      if(pi==1){	
        weights = as.data.frame(ps[[pi]]$weights,check.names = FALSE);
        curves = as.data.frame(ps[[pi]]$curves,check.names = FALSE);
      }
      else{
        weights = plyr::rbind.fill(weights,as.data.frame(ps[[pi]]$weights,check.names = FALSE));
        curves = plyr::rbind.fill(curves,as.data.frame(ps[[pi]]$curves,check.names = FALSE));
      }
    }
    #colwise(mean) does not ignore NAs so we switched to colMeans
    #if(is.null(o$weights)){ o$weights = as.list(plyr::colwise(mean)(weights)); }
    #if(is.null(o$curves)){ o$curves = as.list(plyr::colwise(mean)(curves)); }
    
    if(is.null(o$weights)){ o$weights = as.list(colMeans(weights, na.rm = T)); }
    if(is.null(o$curves)){ o$curves = as.list(colMeans(curves, na.rm = T)); }
    
  }
  return(o[c('weights','curves')]);
}

###############################################################
# This function is for use in optim(). v is a numerical vector representing Iweights subset of the weights and Icurves subset of the curves being optimized.

projects_evalweightandcurvevector=function(v, ps, weights,Iweights,curves,Icurves, enforceweightsign=F,stopwhenbestpossibleisfound=F,...){
  if(stopwhenbestpossibleisfound && .GlobalEnv$projects_evalweightandcurvevector_foundbestpossible){ return(-Inf); }
  #if(enforceweightsign&&!identical(sign(as.numeric(weights[Iweights])), sign(v[1:length(Iweights)]))){ return(-Inf); }
  if(!is.null(enforceweightsign)&&enforceweightsign&&any(as.numeric(weights[Iweights])*v[1:length(Iweights)] < 0)){ return(-Inf); }
  
  weights[Iweights]=v[1:length(Iweights)];
  curves[Icurves]=v[(length(Iweights)+1):length(v)];

  psevals = rep(0,length(ps));
  
  for(pi in 1:length(ps)){
    p=ps[[pi]];
    if(length(curves)) p$curveddata=NULL;
  }
  
  ps = projects_run(ps,...,getfull=T,weights=weights,curves=curves);
  
  for(pi in 1:length(ps)){
    psevals[pi]=ps[[pi]]$rankeval;
  }
  
  out=mean(psevals, na.rm = T);
  
  if(stopwhenbestpossibleisfound && out==1.0 && !is.na(out)){
    cat('Reached best possible evaluation.\n');
    .GlobalEnv$projects_evalweightandcurvevector_foundbestpossible=T;
  }
  return(out);
}
###############################################################

projects_optimsetup=function(ps,o=list(),...){
  o = list_merge(list(
    Iweights=NULL #give names or indices of the weights to optimize. if not given, we optimize all weights.
    ,Icurves=list() #give names or indices of curves to scan. set to NULL to optimize all curves.
    ,trace=0 #for printing progress, use trace=1.
    ,weights=NULL
    ,curves=NULL
  ),o, list(...));
  
  #print(paste0('projects_optimsetup::97:args: ',list(...)))
  
  ps=projects_runonce(ps,...);

  weightsandcurves=projects_getweightsandcurves(ps,o);
  weights=weightsandcurves$weights;
  curves=weightsandcurves$curves;
  Iweights = data_Icols(as.data.frame(weights),o$Iweights);
  Icurves = data_Icols(as.data.frame(curves),o$Icurves);
  par=c(as.numeric(weights[Iweights]),as.numeric(curves[Icurves]));
  o$parnames = c(strs_withprefix(names(weights)[Iweights],'weight:'), strs_withprefix(names(curves)[Icurves],'curve:'));
  o$maximize = T;

  return(list(ps=ps,o=o, weights=weights,curves=curves,Iweights=Iweights,Icurves=Icurves,par=par, ...));
}

###############################################################
#' projects_optimweightandcurves is used to optimize the weights and curves inputed by the user by maximizing the MAP algorithm score. 
#' The use can select what attribute weights and curves need optimizing which can be inputted by listing the column names in the Iweights or Icurves parameters 
#' Several optimization methods can be used and they include: checkzeros|null|Nelder-Mead|SANN|BFGS|CG|L-BFGS-B. One or several optimization methods can be used in a single run. 
#' The optimization will start at the original weights used. The user can maintain the sign of the weights by setting enforceweightsign=T, this is the default setting.
#' To log the optimized weights in an excel file, the user can feed that using the logbestfile= parameter. 
#' 
#' 
#' @export
projects_optimweightsandcurves=function(ps,o=list(),...){
  r=projects_optimsetup(ps,o,...); ps=r$ps; o=r$o; weights=r$weights; curves=r$curves; Iweights=r$Iweights; Icurves=r$Icurves; par=r$par;
  o=list_merge(list(
    method='SANN' #optimization method. checkzeros|null|Nelder-Mead|SANN|BFGS|CG|L-BFGS-B. Can be a list of methods if you want to perform them in serial.
    ,enforceweightsign=T # use enforcesign=T so that when a weight that doesn't match the expected sign is given, we return -Inf.
    ,logbestfile=NULL 
    ),o,list(...));

  if(length(o$method)>1){
    for(method in o$method){
      res=projects_optimweightsandcurves(ps,list_merge(o,list(method=method)),logbestfile=o$logbestfile);
      o$weights=res$weights;
      o$curves=res$curves;
    }
    return(res);
  }

  if(identical(o$method,'checkzeros')){
    res=myoptim(par,projects_evalweightandcurvevector, o, ps=ps, weights=weights,Iweights=Iweights, enforceweightsign=o$enforceweightsign, curves=curves,Icurves=Icurves);
  }
  else{
    .GlobalEnv$projects_evalweightandcurvevector_foundbestpossible=F;
    res=myoptim(par,projects_evalweightandcurvevector, o, ps=ps, weights=weights,Iweights=Iweights, enforceweightsign=o$enforceweightsign, curves=curves,Icurves=Icurves,stopwhenbestpossibleisfound=T, ...);
  }
  
  weights[Iweights]=res$par[1:length(Iweights)];
  curves[Icurves]=res$par[(length(Iweights)+1):length(res$par)];
  res$weights=weights;
  res$curves=curves;
  
  if(!is.null(o$logbestfile)){
    try(xlsxlogfile(res, o$logbestfile), silent = T)
    try(updateoptimizedyaml(res, 'optimizedyaml.yml'))
  }
  else{return(res)}
  
  return(res);

}

###############################################################
#Scan weights & return value ranges that don't affect the eval.

projects_getparranges=function(ps,o=list(),...){
  r=projects_optimsetup(ps,o,...); ps=r$ps; o=r$o; weights=r$weights; curves=r$curves; Iweights=r$Iweights; Icurves=r$Icurves; par=r$par;

  return(myoptim_getparranges(par,projects_evalweightandcurvevector, o, ps=ps, weights=weights,Iweights=Iweights, enforceweightsign=o$enforceweightsign, curves=curves,Icurves=Icurves));
}

###############################################################
#Scan weights and return the rank evaluations.
#ps can be a single project or list of multiple projects.
projects_scanweights=function(ps,o=list(), ...){
  o = list_merge(list(
    Iweights=NULL #give names or indices of the weights to scan. if not given, we scan all weights.
    ,weightscan = seq(-10,10,length.out=5) #provide weight values to use.
    ,doplot=T
    ,weights=NULL
    ,curves=NULL
  ),o);
  o=list_merge(o,list(...));
  ps=projects_runonce(ps,...);
  weightsandcurves=projects_getweightsandcurves(ps,o); weights=weightsandcurves$weights; curves=weightsandcurves$curves;
  Iweights = data_Icols(as.data.frame(weights),o$Iweights);
  Iweightnames = names(weights)[Iweights];

weightscan = o$weightscan;
weightscanevals = as.data.frame(matrix(0, length(weightscan), length(Iweights)));
colnames(weightscanevals)=Iweightnames;

evalhistory = data.frame(matrix(nrow=0,ncol=length(weights)+length(ps)+1));
colnames(evalhistory) = c(names(weights),paste0('rankeval',1:length(ps)),'rankeval');

#psevals is used to store evaluation of each ps for a single weight value.
psevals = rep(0,length(ps));


for(coli in 1:length(Iweights)){
	tmpweights = weights;
	colname=Iweightnames[coli];
	for(wi in 1:length(weightscan)){
		tmpweights[[colname]]=weightscan[wi];
		evalhistory[nrow(evalhistory)+1,1:length(weights)] = tmpweights;

		for(pi in 1:length(ps)){
			p = project_run(ps[[pi]],getfull=T,weights=tmpweights);
			psevals[pi]=p$rankeval;
			evalhistory[nrow(evalhistory), paste0('rankeval',pi)] = p$rankeval;
		}
		rankeval = mean(psevals);
		weightscanevals[wi,colname] =	rankeval;
		evalhistory[nrow(evalhistory),'rankeval']=rankeval;
	}
}

if(o$doplot){
  matplot(weightscan,weightscanevals,type='l',lwd=2)
  legend('topright',Iweightnames,col=1:length(Iweights),lty=1:length(Iweights),inset=c(-0.2,0));
}
return(list(evalhistory=evalhistory,weightscan=weightscan, weightscanevals=weightscanevals));
}
