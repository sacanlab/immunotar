source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2023 by Ahmet Sacan
#Functions for running and multiple projects to study or optimize the weights.
source_disabled__('util.r')
source_disabled__('project.r')

#this function works with projects_setupcluster() to use numeric ids to represent a project.
projects_getglobalproject=function(pid){
	if(!is.numeric(pid)){ stopf('You must use this function only for numeric-id, e.g., check if(is.numeric(p)) before calling.') }
	if(is.null(.GlobalEnv$zoz.parallel.projects)){ stopf('.GlobalEnv$zoz.parallel.projects is not available. You must call projects_setupcluster() to set up that global variable.') }
	if(pid>length(.GlobalEnv$zoz.parallel.projects)){ stopf('.GlobalEnv$zoz.parallel.projects has %d projects, but projectid=%d is being requested. The global projects list seems to be out of sync with what you are currently working on. Call call projects_setupcluster() to fix that') }
	return(.GlobalEnv$zoz.parallel.projects[[pid]]);	
}

#this is a helper function for projects_run_ to run for a single project.
# Extracted here into a separate function so we can more easily run in parallel.
projects_run_single=function(p,...){
	if(!exists('project_run')){ source_disabled__('project.r'); }
	if(!exists('projects_getglobalproject')){ source_disabled__('projects.r'); }
	o=opt_set(requireknownpositives=T,runonce=F,...)
	if(o$requireknownpositives && is.null(p['knownpositives'])){ stop('The projects you provide need to have knownpositives'); }
	if(is.numeric(p)){ p=projects_getglobalproject(p);  }
	if(!o$runonce || !project_wasrunbefore(p)){
		p=project_run(p, outfile=NULL,...,getfull=T);
	}
	return(p);
}

###############################################################
#' projects_run is used to generate the IMMUNOTAR score for  each protein similar to project_run however, this is when you have multiple different cancer expression datasets. These datasets would have to be specified separately in the yaml file or in the project structure within R. This function will run through each project separately and project a structure with the outputs for each project. 
#' To make this function run faster, it uses parallel computing. You can disable this by setting doparallel=F. Use config('numcores',XXX) to set the number-of-cores; if not set, it will default to 2 less than the total cores on your computer. 
#' doparallel argument is passed on to list_map()
#' @export
projects_run=function(ps,...){
	o=opt_set(...);
	if(is.namedlist(ps)){ ps=list(ps); }
	ps = list_map(ps,projects_run_single,o,o);
	return(ps);
}

###############################################################
##use this to run each project once, to ensure their data is prepared.
projects_runonce=function(ps,...){
	return(projects_run(ps,...,runonce=T));
}
###############################################################
projects_rerunifwasrunbefore=function(ps,...){
	o=opt_set(...);
	if(!exists('project_rerunifwasrunbefore')){ source_disabled__('project.r'); }
	if(is.namedlist(ps)){ ps=list(ps); }
	ps = list_map(ps,project_rerunifwasrunbefore,o,o)
	return(ps);
}
###############################################################
#' doparallel argument passed on to list_map()
projects_prepare=function(ps,...){
	o=opt_set(...);
	if(!exists('project_prepare')){ source_disabled__('project.r'); }
	if(is.namedlist(ps)){ ps=list(ps); }
	ps = list_map(ps,project_prepare,o,o)
	return(ps);
}
###############################################################
projects_reimportdefaultparams=function(ps,...){
	o=opt_set(...);
	if(!exists('project_reimportdefaultparams')){ source_disabled__('project.r'); }
	if(is.namedlist(ps)){ ps=list(ps); }
	ps = list_map(ps,project_reimportdefaultparams,o,o)
	return(ps);
}
###############################################################
projects_fillweightsigns=function(ps,...){
	o=opt_set(...);
	if(!exists('project_fillweightsigns')){ source_disabled__('project.r'); }
	if(is.namedlist(ps)){ ps=list(ps); }
	ps = list_map(ps,project_fillweightsigns,o,...)
	return(ps);
}
###############################################################

#projects may have different columns. use this to get a merged list of weights.
#you must call projects_runonce() before calling this function.

projects_getweightsandcurves=function(ps,o=list()){
	o = list_merge(list(
		weights=NULL
		,weightsigns=NULL
		,curves=NULL
	),o);
	if(!isempty(o[['weights']])&&!isempty(o$weightsigns)&&!isempty(o$curves)){ return(o[c('weights','weightsigns','curves')]); }
	
	ps=projects_fillweightsigns(ps,runonce=T,reimportdefaultparams=F);
	
	weights=data.frame(); weightsigns=data.frame(); curves=data.frame();
	for(pi in 1:length(ps)){
		p=ps[[pi]];
		stopfif(is.null(p[['weights']])||is.null(p[['weightsigns']]),'One or more of weights/weightsigns not available in the projects. You must call projects_runonce() before calling this function.');
		if(isempty(o[['weights']])){ weights = plyr::rbind.fill(weights,as.data.frame(p[['weights']],check.names = FALSE)); }
		if(isempty(o$weightsigns)){ weightsigns = plyr::rbind.fill(weightsigns,as.data.frame(p$weightsigns,check.names = FALSE)); }
		if(isempty(o$curves)&&!is.null(p$curves)){ curves = plyr::rbind.fill(curves,as.data.frame(p$curves,check.names = FALSE)); }
	}
	#colwise(mean) does not ignore NAs so we switched to colMeans
	#if(is.null(o[['weights']])){ o[['weights']] = as.list(plyr::colwise(mean)(weights)); }
	#if(is.null(o$curves)){ o$curves = as.list(plyr::colwise(mean)(curves)); }
	
	if(isempty(o[['weights']])){ o[['weights']] = as.list(colMeans(weights, na.rm = T)); }
	if(isempty(o$curves)&&nrow(curves)>0){ o$curves = as.list(colMeans(curves, na.rm = T)); }
	if(isempty(o$weightsigns)){
		o$weightsigns=list();
		for(ci in 1:ncol(weightsigns)){
			if(all(sign(weightsigns[,ci])==sign(weightsigns[[1,ci]]))){ o$weightsigns[[colnames(weightsigns)[[ci]]]]=weightsigns[[1,ci]]; }
			else{ o$weightsigns[[colnames(weightsigns)[[ci]]]]=0; } #when different projects have different signs, set the sign to 0.
		}
	}
	return(o[c('weights','weightsigns','curves')]);
}

###############################################################

projects_optimsetup=function(ps,...){
	o=opt_set(
		Iweights=NULL #give names or indices of the weights to optimize. if not given, we optimize all weights.
		,Icurves=list() #give names or indices of curves to scan. set to NULL to optimize all curves.
		,trace=0 #for printing progress, use trace=1.
		,weights=NULL #you can provide previous starting point for weights & curves here.
		,curves=NULL
		,enforceweightsigns=F # hard-enforce weightsigns by returning -Inf as evaluation when a weight sign does not match the weightsign. Not recommended, b/c -inf may not be okay with some optimization methods (e.g., Nelder-Mead); use softenforceweightsigns instead.
		,softenforceweightsigns=T #each undesired weightsign will cause between -1..-2 (proportional to magnitude) penalty in the evalution score. 
		,hardzeroweights=NULL #give a list of features (e.g., 'opentargetsurface_isopentargetsurface') that you want to hard-set at zero and exclude from optimization. We use prefix-matching for these items.
,lockzeroweights=F #whether to keep the existing zero-weights (ie., exclude them from further optimization/inclusion)
,lockzerocurves=F
,lockweights=NULL #names of the weights to lock (prevent from changing)
		,...);
	stopfif(!is.null(o$numcores),'numcores argument is now obsolete. Use config("numcores",X) to set up the number of cores.');
	
	#keep only the projects that have known targets (the projects that do not have known targets cannot be optimized on.)
	Ikeep=rep(F,length(ps));
	for(i in 1:length(ps)){
		Ikeep[[i]]=!isempty(ps[[i]]$knownpositives);
	}
	if(!all(Ikeep)){
		msgf('Removing %d of %d projects, because they do not have known target to optimize with.',unlist(sum(!Ikeep)),length(ps));
		ps=ps[Ikeep];
	}
	if(!length(ps)){ stop('No projects (left) to optimize with. Make sure you provide projects with known targets.'); }
	
	#if o$curves=NULL and o$Icurves=NULL, set o$curves=list() to force project_run->data_score to fill in curves list.
	if(is.null(o$curves)&&(is.null(o$Icurves)||!isempty(o$Icurves))){ o$curves=list(); }
	if(!isempty(o$curves)&&any(is.na(o$curves))){ o$curves[is.na(o$curves)]=0; }
	if(!is.null(o$curves)&&is.null(ps[[1]]$curves)){ ps=projects_run(ps,...,o,getcurves=T); }
	else{ ps=projects_runonce(ps,...,o,getcurves=T); }
	
	#remove reporting, it would consume unnecessary time.
	for(i in 1:length(ps)){    ps[[i]]=list_removefields(ps[[i]],'reportforgenes');  }
	stopfif(isempty(ps[[1]][['weights']]),'projects_runonce should have made p$weights available, but it is not available.')
	dbg_warnfif((o$enforceweightsigns||o$softenforceweightsigns)&&is.null(ps[[1]]$weightsigns),'If you wish to enforcewightsign, it is recommended that you call projects_fillweightsigns yourself, before using optimization, so you can choose whether to reimportdefaultparams for the weightsign determination. reimportdefaultparams=F is used here, so the signs will be based on the current weight values.')
	ps=projects_fillweightsigns(ps,runonce=T,reimportdefaultparams=F);
	
	weightsandcurves=projects_getweightsandcurves(ps,o);
	weights=weightsandcurves[['weights']];
	weightsigns=weightsandcurves$weightsigns;
	curves=weightsandcurves$curves;
	
	
	Iweights = data_Icols(as.data.frame(weights),o$Iweights);
	Icurves = data_Icols(as.data.frame(curves),o$Icurves);
	
	if(!isempty(o$hardzeroweights)){
		hardzeroweights=csv(o$hardzeroweights);
		I=rep(FALSE,length(weights));
		for(i in 1:length(hardzeroweights)){
			I=I | grepl(paste0('^',hardzeroweights[[i]]),names(weights));
		}
		if(any(I)){
			I=which(I);
			weights[I]=0;
			Iweights=base::setdiff(Iweights,I);
		}
	}
	if(!isempty(o$lockweights)){
		I= match(o$lockweights, names(weights));
		stopfif(any(is.na(I)),'Unknown lockweights: %s',str_csv(o$lockweights[is.na(I)]));
		Iweights=base::setdiff(Iweights, I);
	}
	if(o$lockzeroweights){
		I= weights==0;
		if(any(I[Iweights])){
			msgf('projects_optimsetup: lockzeroweights=T: Removing %d of %d weight parameters that currently have a value 0. You need to set lockzeroweights=F if you want to optimize them as well.',sum(I[Iweights]),length(Iweights));
			Iweights=base::setdiff(Iweights, which(I));
		}
	}
	if(o$lockzerocurves){
	  I= curves==0;
	  if(any(I[Icurves])){
	    msgf('projects_optimsetup: lockzerocurves=T: Removing %d of %d curve parameters that currently have a value 0. You need to set lockzerocurves=F if you want to optimize them as well.',sum(I[Icurves]),length(Icurves));
	    Icurves=base::setdiff(Icurves, which(I));
	  }
	}
	#check for any Icurves that we can exclude because their corresponding weights are kept at zero.
	if(length(Icurves)>0){
		Iremove=weights==0;
		Iremove[Iweights]=F; #keep a weight if it is being optimized (it may become non-zero during optimization).
		Iremove=which(Iremove);
		Iremove=Icurves %in% Iremove;
		if(any(Iremove)){
			msgf('projects_optimsetup: Removing %d of %d curve parameters, because their corresponding weight is zero and that weight is not currently being optimized. (Changing those curves will not have any effect.)',sum(Iremove),length(Icurves));
			Icurves=Icurves[!Iremove];
		}
	}
	#print(Iweights); print(Icurves); stop(); 
	
	
	par=c(as.numeric(weights[Iweights]),as.numeric(curves[Icurves]));
	o$parsigns=c(as.numeric(weightsigns[Iweights]),rep(0,length(Icurves)));
	o$parnames = c(strs_withprefix(names(weights)[Iweights],'weight:'), strs_withprefix(names(curves)[Icurves],'curve:'));
	o$maximize = T;
	o$mineval = -10*length(Iweights); #instead of 0, this considers the softenforceweightsigns penalties we may have. multiplying by 2 is enough, but let's multiply by 10 to be sure.
	o$maxeval = 1;
	
	#return(list(ps=ps,o=o,weights=weights,curves=curves,Iweights=Iweights,Icurves=Icurves,par=par, ...))
	return(list(ps=ps,o=o,weights=weights,weightsigns=weightsigns,curves=curves,Iweights=Iweights,Icurves=Icurves,par=par))
	
	#return(list(ps=ps,o=o, list(weights=weights,curves=curves,Iweights=Iweights,Icurves=Icurves,par=par), list(...)));
}
###############################################################
#This function is for direct calling, when you want to evaluate a set of weights and curves, without having to go in projects_optimweightsandcurves()
projects_evalweightsandcurves=function(ps,weights=NULL,curves=NULL, enforceweightsigns=F,softenforceweightsigns=T,...){
	r=projects_optimsetup(ps,weights=weights,curves=curves,enforceweightsigns=enforceweightsigns,softenforceweightsigns=softenforceweightsigns,...);
	ps=r$ps; o=r$o; weights=r[['weights']]; weightsigns=r$weightsigns; curves=r$curves; Iweights=r$Iweights; Icurves=r$Icurves; par=r$par;
	res=projects_evalweightandcurvevector(par, o, ps=ps, weights=weights,weightsigns=weightsigns,Iweights=Iweights,curves=curves,Icurves=Icurves,enforceweightsigns=o$enforceweightsigns,softenforceweightsigns=o$softenforceweightsigns,doparallel=o$doparallel);
	return(res);
}
###############################################################
# This function is for use in optim(). v is a numerical vector representing Iweights subset of the weights and Icurves subset of the curves being optimized.
# we explicitly list the input arguments and avoid opt_set() for speed.
projects_evalweightandcurvevector=function(v, ps, weights,weightsigns,Iweights,curves,Icurves, enforceweightsigns=F,softenforceweightsigns=T,...){
	if(!is.null(enforceweightsigns)&&enforceweightsigns&&any(as.numeric(weightsigns[Iweights])*v[1:length(Iweights)] < 0)){
		return(-Inf); }
	
	if(length(Iweights)>0){ weights[Iweights]=v[1:length(Iweights)]; }
	if(length(Icurves)>0){ curves[Icurves]=v[(length(Iweights)+1):length(v)]; }
	
	psevals = projects_run(ps,...,getfull=T,weights=weights,curves=curves,getrankeval=T);
	out=mean(unlist(psevals), na.rm = T);
	
	#penalize between -1..-2 (proportional to magnitude) for each weightsign mismatch
	if(!is.null(softenforceweightsigns)&&softenforceweightsigns){
		I=as.numeric(weightsigns[Iweights]) * v[1:length(Iweights)] < 0;
		if(any(I)){
			#print(t(plyr::rbind.fill(as.data.frame(weightsigns[Iweights]),as.data.frame(t(list_make(names(weightsigns),v[1:length(Iweights)]))))))
			penalty=abs(v[1:length(Iweights)][I]/100); #proportional penalty helps with continuous adjustment back to desirable signage.
			penalty[penalty>1]=1;
			penalty = sum(I) + sum(penalty);
			#msgf('eval: %f, penalty: %f, v: %s',out,penalty,str_csv(v))
			out = out - penalty;
		}
	}
	
	return(out);
}
###############################################################
#' projects_optimweightandcurves is used to optimize the weights and curves inputted by the user by maximizing the MAP algorithm score. 
#' The use can select what attribute weights and curves need optimizing which can be inputted by listing the column names in the Iweights or Icurves parameters 
#' Several optimization methods can be used and they include: checkzeros|null|Nelder-Mead|SANN|BFGS|CG|L-BFGS-B. One or several optimization methods can be used in a single run. 
#' The optimization will start at the original weights used. The user can hard-maintain the sign of the weights by setting enforceweightsigns=T (not recommended), or more gracefully favor the maintainance of the signs by setting softenforceweightsigns=T (this is default).
#' To log the optimized weights in an excel file, the user can feed that using the optimlogfile= parameter. 
#' 
#' 
#' @export
projects_optimweightsandcurves=function(ps,...){
	if(!exists('myoptim')){ source_disabled__('optim.r'); }
	
	if(nargs()==2 && var_tobool(list(...)[1]$recursion__)){
		o=list(...)[1];
		par=o$par;
		weights=o$weights;
		weightsigns=o$weightsigns;
		curves=o$curves;
		Iweights=o$Iweights;
		Icurves=o$Icurves;
	}
	else{    
		o=opt_set(
			method='SANN' #optimization method. checkzeros|null|Nelder-Mead|SANN|BFGS|CG|L-BFGS-B. Can be a list of methods if you want to perform them in serial.
			,enforceweightsigns=F # use enforceweightsigns=T so that when a weight that doesn't match the expected sign is given, we return -Inf. Not recommended; use softenforceweightsigns to more gracefully penalize unexpected weight signs.
			,softenforceweightsigns=T
			,optimlogfile=T  #provide true or a filename. if true, project_addoptimlog() will use a default log file 'project_optimization.log.xlsx'
			,optimizedparamsfile=NULL #provide true or a filename. if true, project_updateoptimizedparams() will use a default file 'project_optimizedparams.yml'.
			,devmode=F #when true, we use set maxiterations for a quick-run. Useful for code-testing; not for production purposes.
			,setupcluster=NULL
			,recursion__=F #when running multiple methods, calling each method set this to true.
			,usenumericids__=F #used internally to let recursive calls know to use numeric project ids (kept in a global variable adn sent to parallel workers, to speed up processing.)
			,noworsethaninit=NULL #for some methods (ga), should we compare with the initial parameter's performance and return the higher of that vs. optimized params? will default to True for the last method we are running.
			,optimizelogfile=NULL #whether to load the optimlog and apply optimization to each entry and save it back in the optim.
			,...);
		r=projects_optimsetup(ps,o); ps=r$ps; o=r$o; weights=r[['weights']]; weightsigns=r$weightsigns; curves=r$curves; Iweights=r$Iweights; Icurves=r$Icurves; par=r$par;
		stopfif(isempty(par),"Empty parameter vector. Nothing to optimize.")
	}
	
	o$method=csv(o$method);
	if(!var_tobool(o$recursion__)){#only needs to be done in the first entry-call when multiple methods are being executed.
		if(o$devmode){
			msgf('Devel-mode is ON. Setting maxit=3, abstol=0.1, reltol=0.5 for a quick-run.');
			o$maxit=3; o$reltol=0.5; o$abstol=0.1;
			o$methodcontrols$SANN$maxit=o$maxit;
			o$methodcontrols[['Nelder-Mead']]$maxit=o$maxit; #I don't think this has any effect.
			o$methodcontrols[['Nelder-Mead']]$abstol=o$abstol;
			o$methodcontrols[['Nelder-Mead']]$reltol=o$reltol;
		}
		
		#setup a cluster if needed
		numtasks=length(ps);
		if(is.null(o$setupcluster)&&var_tobool(o$doparallel)){ o$setupcluster=TRUE; }
		if(!isempty(o$method)&&any(o$method=='ga')&&var_tobool(o$methodcontrols$ga$parallel)){ o$setupcluster=TRUE; numtasks=NULL; } #this will cause startcluster() to decide, based on config() or available cores.
		if(var_tobool(o$setupcluster)){
			clust=projects_setupcluster(ps,numtasks=numtasks,doexportps=T);
			o$usenumericids__=T;
			if(var_tobool(o$methodcontrols$ga$parallel)&&is.logical(o$methodcontrols$ga$parallel)){ o$methodcontrols$ga$parallel=clust; }
		}
	}
	
	if(var_tobool(o$optimizelogfile)){
		logfile=project_optimlogfile(o);
		log = project_getoptimlogpopulation(optimlogfile=logfile,getdetailed=T,o,weights=weights,curves=curves,Iweights=Iweights,Icurves=Icurves);
		o$optimlogfile = io_addfilenameprefix(logfile,time_date(format='.tmp.%Y%m%d.%H%M%S.'));
		
		#run once normally for the current weights and weights (from ps weights or input argument)
		bestres=projects_optimweightsandcurves(ps,opt_set(o,par=par,weights=weights,weightsigns=weightsigns,curves=curves,Iweights=Iweights,Icurves=Icurves,recursion__=T,optimizelogfile=F));
		
		if(nrow(log$data)>0){ for(i in 1:nrow(log$data)){
			msgfif(o$trace,'--------------- [[ optimizing optimlog %d of %d: %s ]] %s ---------------',i,nrow(log$data),str_csv(o$method),var_pick(o$optimcomment,o$optimcomment,''));
			optimcomment=paste0(log$data[[i,'optimcomment']],sprintf('::reoptim oldrow=%d ::',i),var_pick(o$optimcomment,o$optimcomment,''));
			wlog=as.list(log$weights[i,]); clog=as.list(log$curves[i,]);
			wlog=type.convert(wlog, as.is=T); clog=type.convert(clog, as.is=T)
			weights2=weights; curves2=curves;
			weights2[names(wlog)]=wlog; curves2[names(clog)]=clog;
			res=projects_optimweightsandcurves(ps,opt_set(o,recursion__=T,par=par,weightsigns=weightsigns,Iweights=Iweights,Icurves=Icurves,optimcomment=optimcomment,optimizelogfile=F,weights=weights2,curves=curves2));
			if(res$value>bestres$value){
				bestres=res;
				weights=res$weights;
				curves=res$curves;
			}
		}}
		file.rename(logfile,io_addfilenamesuffix(o$optimlogfile,'.DELETE'));
		file.rename(o$optimlogfile,logfile);
		return(bestres);
	}
	
	
	
	#we run each method individually, so we can save each
	if(length(o$method)>1){
		methods=o$method;
		for(i in 1:length(methods)){
			if(is.null(o$noworsethaninit)){ o$noworsethaninit=i==length(methods); }
			o$method=methods[[i]];
			msgfif(o$trace,'============= [[ method %d of %d: %s ]] ============ %s',i,length(methods),o$method,var_pick(o$optimcomment,o$optimcomment,''));
			res=projects_optimweightsandcurves(ps,opt_set(o,par=par,weights=weights,weightsigns=weightsigns,curves=curves,Iweights=Iweights,Icurves=Icurves,recursion__=T));
			weights=res$weights;
			curves=res$curves;
		}
		return(res);
	}
	
	## We now have an individual o$method below this line.
	if(o$usenumericids__){  ps=as.list(seq(1,length(ps))); }
	if(!isempty(o$method)&&o$method=='ga'&&var_tobool(o$methodcontrols$ga$parallel)){
		dbg_warnfif(var_tobool(o$doparallel),'Setting doparallel=F within GA(parallel=T) to avoid nested-parallellism, b/c the population will likely need all the workers and no cores will be left to run nested parallel jobs.');
		#o$doparallel=F;
	}
	
	savepopulation=var_pickfirstnotnull(o$methodcontrols[['ga']]$savepopulation,o$savepopulation);
	if(identical(o$method,'ga')&&var_tobool(savepopulation)&&!is.character(savepopulation)){
		o$methodcontrols[['ga']]$savepopulation=io_name(sys_cachedir(),paste0(basename(project_optimizedparamsfile(o)),var_pick(Iweights,'.weights',''),var_pick(Icurves,'.curves',''),'.',sys_computername(),'.gapop.fst'));
	}
	loadlogpopulation=var_pickfirstnotnull(o$methodcontrols[[o$method]]$loadlogpopulation,o$loadlogpopulation);
	if(identical(o$method,'ga')&&var_tobool(loadlogpopulation)){
		pop=project_getoptimlogpopulation(o,weights=weights,Iweights=Iweights,curves=curves,Icurves=Icurves);
		if(!isempty(pop)){
			assertf(ncol(pop)==length(par));
			o$methodcontrols[['ga']]$suggestions=rbind(o$methodcontrols[['ga']]$suggestions,pop);
		}
	}
	 #whether to create pargroups for weights & curves so the a weight and its corresponding curve can be optimized simultaneously.
	if(identical(o$method,'sfs')&&var_tobool(var_pickfirstnotnull(o$methodcontrols$sfs$groupweightsandcurves,o$groupweightsandcurves))){
		o$pargroups=arr_groupby(1:length(o$parnames), gsub('^(weight|curve):','',o$parnames));
		o$usepargroups=T;
	}
	if(identical(o$method,'sfs')){
		o$pardepends=list();
		curveparinds=which(grepl('^curve:',o$parnames));
		for(cind in curveparinds){
			curvename=o$parnames[[cind]];
			wind=which(o$parnames == paste0('weight:', gsub('^curve:','',curvename)));
			if(length(wind)){ o$pardepends[[cind]]=wind; }
		}
	}
	
	#res=myoptim(par,projects_evalweightandcurvevector, o, ps=ps, weights=weights,weightsigns=weightsigns,Iweights=Iweights,curves=curves,Icurves=Icurves,enforceweightsigns=o$enforceweightsigns,softenforceweightsigns=o$softenforceweightsigns,doparallel=o$doparallel); 
	fn=function(par){
	  if(!exists('projects_evalweightandcurvevector')){ source_disabled__('projects.r'); }
	  return(projects_evalweightandcurvevector(par, ps=ps, weights=weights,weightsigns=weightsigns,Iweights=Iweights,curves=curves,Icurves=Icurves,enforceweightsigns=o$enforceweightsigns,softenforceweightsigns=o$softenforceweightsigns,doparallel=o$doparallel)); }
	res=myoptim(par,fn, o);
	
	weights[Iweights]=res$par[1:length(Iweights)];
	curves[Icurves]=res$par[(length(Iweights)+1):length(res$par)];
	res$weights=weights;
	if(!isempty(curves)){ res$curves=curves; }
	
	project_optimbackup(res); #Not necessary, but let's just keep a time'd backup of the results in sys_cachedir().
	
	if(!is.null(o$optimizedparamsfile)){
    project_updateoptimizedparams(res, o);
    if(identical(o$method,'sfs')){
      yamlfile=project_optimizedparamsfile(o);
      statsfile=io_changefileext(yamlfile,paste0('.sfs.',var_pick(identical(o$methodcontrols$sfs$direction,'reverse'),'reverse.','forward.'),time_date(format='%Y%m%d.%H%M%S'),'.xlsx'));
      data_writefile(res$stats,statsfile)
	  projects_sfs_summarizeimpact(statsfile,overwrite=T)
	  projects_impactfile_groupbydatasource(statsfile,overwrite=T);
    }
    else if(identical(o$method,'parranges')){
      yamlfile=project_optimizedparamsfile(o);
      statsfile=io_changefileext(yamlfile,paste0('.parranges.',time_date(format='%Y%m%d.%H%M%S'),'.xlsx'));
      data_writefile(res$stats,statsfile)
    }
  }
	else{ warnf('You did not provide the optimizedparamsfile option. If you want to save the parameters found from this optimization, set optimizedparamsfile to TRUE or a file name.'); }
	if(!is.null(o$optimlogfile)){ project_addoptimlog(res, optimlogfile = o$logfile, o);   }
	
	return(res);
}

###############################################################
# Summarize the impact from a single SFS run. Saves the summmary in the same file as the impact worksheet.
projects_sfs_summarizeimpact=function(file,overwrite=F){
if(!overwrite&&io_isfile(file)&&xls_issheet(file,'impact')){
	msgf('projects_sfs_summarizeimpact: impact worksheet already present in file [ %s ]. Use overwrite=T to regenerate.',file);
	return(data_readfile(file,rowNames=F,sheet='impact'));
}
d=data_readfile(file);
d=d[!(d$parname %in% 'BASEEVAL__'),]; d=type.convert(d, as.is =TRUE)
d$changeaccepted=as.numeric(d$changeaccepted)
d=data_combineduplicaterows(d,func=max,idcolumn='parname')
d$pargroupname = gsub('_.*','',d$parname);
I=csv('pargroupname,parname,impact,iter,changeaccepted'); I[!(I %in% colnames(d))]=NULL;
d=d[,I];
d=d[order(-d$impact),]; rownames(d)=NULL;
data_writefile(d,file,sheet='impact');
return(d);
}
###############################################################
#takes the impact worksheet and generates groupedimpact worksheet (grouped by datasource)
projects_impactfile_groupbydatasource=function(file,overwrite=F){
if(!overwrite&&io_isfile(file)&&xls_issheet(file,'groupedimpact')){
	msgf('projects_sfs_summarizeimpact: groupedimpact worksheet already present in file [ %s ]. Use overwrite=T to regenerate.',file);
	return(data_readfile(file,rowNames=F,sheet='groupedimpact'));
}
d=data_readfile(file,rowNames=F,sheet='impact');
d$datasource = gsub('^(weight|curve):','',d$pargroupname);
d=data_combineduplicaterows(d,func=max,idcolumn='datasource')
d=d[order(-d$impact),]; rownames(d)=NULL;
data_writefile(d,file,sheet='groupedimpact');
return(d);
}

###############################################################
#Combine SFS Impact analysis from multiple runs (whose results are kept in data/sfs.forward/*.xlsx), create the combined impact.summary.xlsx
projects_sfs_summarizeimpact_frommultirun=function(folder='sfs.forward',grouped=F,overwrite=F){
folder=getdatafile(folder);
file=io_name(folder,'impact.summary.xlsx');
if(!grouped&&!overwrite&&io_isfile(file)&&xls_issheet(file,'impact')){
	msgf('projects_sfs_summarizeimpact: impact worksheet already present in file [ %s ]. Use overwrite=T to regenerate.');
	return(data_readfile(file,rowNames=F,sheet='impact'));
}
if(grouped&&!overwrite&&io_isfile(file)&&xls_issheet(file,'groupedimpact')){
	msgf('projects_sfs_summarizeimpact: groupedimpact worksheet already present in file [ %s ]. Use overwrite=T to regenerate.');
	return(data_readfile(file,rowNames=F,sheet='groupedimpact'));
}

files = fs::dir_ls(folder, glob='*project_optimizedparams*.sfs.*.xlsx')
stopfif(isempty(files),'No matching files in folder [ %s ]',folder);
d=data.frame();
for(fi in forlength(files)){
  f=files[[fi]];
  if('impact' %in% xls_sheetnames(f)){
    d2=data_readfile(f,sheet='impact',rowNames = F)
    d=plyr::rbind.fill(d,as.data.frame(d2));
  }
}
d=data_combineduplicaterows(d,func=max,idcolumn='parname')
d=d[order(-d$impact),]; rownames(d)=NULL
d$pargroupname = gsub('_.*','',d$parname);
d=data_reordercols(d,csv('pargroupname,parname,impact'));
data_writefile(d,file,sheet='impact');

dg=projects_impactfile_groupbydatasource(file,overwrite=T);

return(var_pick(grouped,dg,d));
}

###############################################################
#Scan weights and return the rank evaluations.
#ps can be a single project or list of multiple projects.
projects_scanweights=function(ps, ...){
	o=opt_set(
		Iweights=NULL #give names or indices of the weights to scan. if not given, we scan all weights.
		,weightscan = seq(-10,10,length.out=5) #provide weight values to use.
		,doplot=T
		,weights=NULL
		,curves=NULL
		,...);
	warnf('TODO: projects_scanweights(): I should probably call projects_optimsetup to set these up.');
	ps=projects_runonce(ps,...);
	weightsandcurves=projects_getweightsandcurves(ps,o); weights=weightsandcurves[['weights']]; curves=weightsandcurves$curves;
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

###############################################################
#collect the 'name' 
projects_getnames=function(ps){
	if(is.null(names)){
		pnames=paste0('project',1:length(ps));
		for(i in 1:length(ps)){
			pname=ps[[i]][['name']];
			if(!is.null(pname)){ pnames[[i]]=pname; }
		}
	}
	return(pnames)
}
###############################################################
#summarize the gene scores across multiple projects.
#names: project names. will default to list of p$name if available. otherwise 'projectN'
#by Rawan
#' @export
projects_summarizeresults=function(ps,pnames=NULL){
	map=pancancer_diseasemap()
	for(i in 1:length(ps)){
		pheno=map[[which(names(map) == ps[[i]]$disease)]]$short
		d=ps[[i]]$datawithscore
		d=data.frame(genes=rownames(d))
		d=o[,pheno]=d$score
		if(i == 1){
			res=d
		}else{
			res=merge(res, d, by='genes')
		}
	}
	rownames(res)=res$genes
	res$avg.score=rowMeans(res[,2:ncol(res)])
	res=res[order(res$avg.score, decreasing = T),]
	return(res)
}


###############################################################
#by Rawan
projects_heatmap=function(ps,o=list(),...){
	#options:
	#plotfeatures=list() #which features to include in the heatmap?
	
}

###############################################################
#options are passed into project_rankimpact()
#Primary caller: projects_rankimpact_plot() (results to be shown in pancancer_optimize.rmd and/or pancancer_analyze.rmd)
#The caller can take the result and create a bar plot showing mean and std of each attribute.
projects_rankimpact=function(ps,...){
	o=opt_set(
		devmode=F
		,doparallel=T
		,...) #devmode affects the filename we store the results as.
	#saving it in datadir, b/c we don't want to lose these results.
	
	#ocache=cache_opts(cachedir=sys_datadir(),cachefile=getdatafile(paste0('projects_rankimpact',var_pick(o$devmode,'.devel',''),'.fst')), ...)
	ocache=cache_opts(cachedir=sys_datadir(),fileext='fst',...)
	
	if(!ocache$recache){
		msgf('projects_rankimpact(): File [%s] already exists. You need to either remove it or use recache=T to recreate it. Reading and returning the file contents...',ocache$cachefile);
		return(cache_load(ocache));
	}
	
	
	ps=projects_runonce(ps);
	
	#Rawan: This was giving me an error, had to edit it to remove the (o, modifylist(o, list(...))) to just the modifylist(o,list(...)) and now it works correctly because its not feeding the o list twice. 
	pimpacts=list_map(ps,project_rankimpact,modifyList(o,list(doparallel=F)),o)  #if we doparallel here,let's not doparallel in project_rankimpact
	#TODO: check if the following works:
	#pimpacts=list_map(ps,project_rankimpact,o, modifyList(o,list(doparallel=F)))  #if we doparallel here,let's not doparallel in project_rankimpact
	impacts=data.frame();
	for(pimpact in pimpacts){
		#Rawan edit: needed to make the pimpact a dataframe because this was a list and it was erroring
		impacts=plyr::rbind.fill(impacts,as.data.frame(pimpact))
	}
	msgf('projects_rankimpact(): Writing results to file [%s]...',ocache$cachefile);
	cache_save(impacts,ocache);
	yaml::write_yaml(o,paste0(ocache$cachefile,'.yml'));
	return(impacts);
}


###############################################################
#The caller setting doexportps=True is responsible for changing its own ps list to numbers.
projects_setupcluster=function(ps,numtasks=NULL,doexportps=F,doexportfuncs=T){
	clust=parallel_startglobalcluster(numtasks=var_pickfirstnotnull(numtasks,length(ps)));
	doexportfuncs=var_pickfirstnotnull(doexportfuncs); #is.null(.GlobalEnv$zoz.parallel.exported$projectfuncs));
	if(doexportps){
		if(!is.numeric(ps)){ .GlobalEnv[['zoz.parallel.projects']]=ps; }
		parallel_registerexports(csv('zoz.parallel.projects,zoz.config,zoz.config.coded'),clust);
	}	
	if(doexportfuncs){
		msgf('projects_setupcluster: Exporting dependency functions. This may take a minute...')
		#run a simple project, to ensure all functions are loaded into .GlobalEnv, then export them to the cluster.
		p=ps[[1]];
		if(is.numeric(p)){ p=projects_getglobalproject(p); }
		p=project_minimizefordevmode(p);
		p=list_removefields(p,'reportforgenes');
		p=project_resetruncounter(p);
		project_run(p);
		
		deps=func_depends(csv('projects_evalweightandcurvevector')); #this is a memcached list.
		.GlobalEnv$installpackageifmissing=.GlobalEnv$installpackageifmissing_github=.GlobalEnv$installpackageifmissing_bioc=function(...){invisible()} #turn these costly functions on. We assume all needed packages are already installed in the workers.
		parallel_registerexports(deps,clust);
	}
	return(invisible(clust));
}

###############################################################
#use this for a single project p or list of projects ps.
#we implement this for project*s* and not for a single project, so we can load the yaml file only once.
projects_cache_updateifneeded=function(ps,ocache,...){
	o=opt_set(
		optimizedparamsfile=T
		,doparallel=T
		,...
	)
	if(!var_tobool(o$optimizedparamsfile)){ return(ps); }
	stopfif(is.namedlist(ps),'This function should only be called with a list of projects (we expect the cache to also contain a list of project. For single project and its cache use the project_ (singular) function instead.');
	importfile=project_optimizedparamsfile(yamlfile=o$importfile,...);
	
	modified=F
	for(i in forlength(ps)){
		p=project_cache_updateifneeded(ps[[i]],ocache,o,dosave=F,dorerunifneeded=F);
		if(var_tobool(attr(p,'modified'))){ modified=T; ps[[i]]=p; }
	}
	if(modified){
		ps=projects_rerunifwasrunbefore(ps,doparallel=o$doparallel);
		msgf('projects_cache_updateifneeded: The importfile [%s] was updated since last cachefile, Projects have been updated. Saving new cachefile...',importfile);
		cache_save(ps,ocache);
	}
	return(ps);
}



###############################################################
#Used in pancancer_optimize.rmd
projects_optimrecipe=function(ps,...){
	tictoc::tic();
	o=opt_set(
		numrepeats=1
		,freshstart=F
		,subsets=NULL #csv of weights,curves,weights+curves. Defaults to all three on freshstart, but just weights+curves for not-freshstart. if Iweights or Icurves is not null, we don't use these subsets.
		,comment=''
		,...
	)
	OO=list_removefields(o,csv('numrepeats,freshstart,subsets'));
	if(is.null(o$subsets)){	o$subsets=var_pick(o$freshstart,'weights,curves,weights+curves','weights+curves'); }
	o$subsets=csv(o$subsets);
	optimres=list_selectfields(o,c('weights','curves'));
	
	for(i in seq(1,o$numrepeats)){
		if(o$freshstart){
			optimres=list(); ps=projects_reimportdefaultparams(ps); ps=projects_fillweightsigns(ps); ps=projects_runonce(ps); .GlobalEnv$projects_optimrecipe_freshstarted=T;
		}
		else if(i==1 && var_tobool(.GlobalEnv$projects_optimrecipe_freshstarted)){ optimres=list(); ps=projects_runonce(ps); }
		for(subi in forlength(o$subsets)){
			subset=o$subsets[[subi]];
			msgf('----------------- [[ Repetition %d of %d  - Subset %d of %d: %s ]] ----------------',i,o$numrepeats, subi,length(o$subsets),subset);
			subOO=OO;
			stopfif(!str_incsv('weights,curves,weights+curves',subset),'Invalid subset [ %s ]',subset);
			if(str_incsv('weights,weights+curves',subset)){ if(isempty(subOO$Iweights)){ subOO['Iweights']=list(NULL); } }
			else{ subOO[['Iweights']]=list(); }
			if(str_incsv('curves,weights+curves',subset)){ if(isempty(subOO$Icurves)){ subOO['Icurves']=list(NULL);} }
			else{ subOO[['Icurves']]=list(); }
			
			optimres = projects_optimweightsandcurves(ps,subOO,list_selectfields(optimres,c('weights','curves')),optimcomment=paste0(var_pick(o$freshstart,'freshstart,',''),subset,str_ensureprefix(o$comment,','))) ;
		}
	}
	tictoc_pretty()
	return(invisible(optimres));
}
###############################################################
#a convenience function that uses the parent environment to grab the variables.
#Used in pancancer_optimize_ahmet.rmd
projects_optimrecipe__=function(optimres=NULL,...){
	env=parent.env(environment());
	optimres=projects_optimrecipe(env[['ps']],env[['OO']],comment=env[['cmnt']],weights=optimres$weights,curves=optimres$curves,...);
	return(invisible(optimres));
}



###############################################################
#combines the optimizedparamsfile present in the datafolder into project_optimizedparams_comparison.xlsx file.
projects_compareoptimizedparamsfiles=function(...){
o=opt_set(
	doplot=T
	,...
	);
file=io_name(sys_datadir(),'project_optimizedparams_comparison.xlsx');
files = fs::dir_ls(sys_datadir(), glob='*project_optimizedparams*.yml');
files=files[!grepl('\\.devel\\.',files)];
ocache=cache_opts(o,depends=files,cachefile=file);
if(ocache$recache){
d=data.frame();
for(fi in forlength(files)){
  f=files[[fi]];
  y=yaml::read_yaml(f);
  colweight=list_withprefixnames(y$colweight,'weight:');
  colcurve =list_withprefixnames(y$colcurve,'curve:');
  y=list_merge(y,colweight);
  y=list_merge(y,colcurve);
  y=list_removefields(y,'colweight','colcurve');
  d=plyr::rbind.fill(d,as.data.frame(y,check.names=F));
}
rownames(d)=gsub('project_optimizedparams.','',io_filenamenoext(files));
#remove columns that are all zeros.
Inum=data_numericcols(d);
dnum=d[,Inum]; dnum[is.na(dnum)]=0; d[,Inum]=dnum;
Iremove = Inum[ colSums(d[Inum]!=0 ) == 0];
d[,Iremove]=NULL;

d=d[order(-d$optimscore),];
d=data_reordercols(d,csv('optimscore,numnonzeroparams,numnonzeroweights,numnonzerocurves'))

data_writefile(d,file);
}#recache
else{
	msgf('File %s already exists. Use recache=T to regenerate it.',file);
	d=data_readfile(file);
}
if(o$doplot){
dwc=d[,grepl('^(weight|curve):',colnames(d))]
#colnames(dwc)=project_shortenfeaturenames(colnames(dwc)) #TODO: project_shortenfeaturenames() has an error for this.
colnames(dwc)=gsub(paste0('_(',paste0(vec_rescaletypes(),collapse='|'),')'),'',colnames(dwc))
colnames(dwc)=gsub('^weight:','w:',colnames(dwc)); colnames(dwc)=gsub('^curve:','c:',colnames(dwc));
installpackageifmissing_bioc('ComplexHeatmap')
plt=ComplexHeatmap::pheatmap(as.matrix(dwc),fontsize=8,show_rownames=T,show_colnames=T,scale='column')
print(plt)
}
return(d);
}

###############################################################
#stk__=dbg_nicestack(1); message(sprintf('projects.r sourced from: %s',stk__));


###############################################################
#collect the 'name' 
projects_getnames=function(ps){
  if(is.null(names(ps))){
    pnames=paste0('project',1:length(ps));
    for(i in 1:length(ps)){
      pname=ps[[i]][['disease']];
      if(!is.null(pname)){ pnames[[i]]=pname; }
    }
  }
  return(pnames)
}

###############################################################
#summarize the gene scores across multiple projects.
#names: project names. will default to list of p$name if available. otherwise 'projectN'
#by Rawan

#' @export
projects_summarizeresults=function(ps,pnames=NULL){
  map=pancancer_diseasemap()
  for(i in 1:length(ps)){
    if(is.null(pnames)){
      pheno=ps[[i]]$disease
    }else{pheno=pnames[i]}
    d=ps[[i]]$datawithscore
    df=data.frame(genes=rownames(d))
    df[,pheno]=d$score
    if(i == 1){
      res=df
    }else{
      res=merge(res, df, by='genes')
    }
  }
  rownames(res)=res$genes
  res$genes=NULL
  res$Average_score=rowMeans(res[,2:ncol(res)])
  res=res[order(res$Average_score, decreasing = T),]
  return(res)
}


###############################################################
#Create a summarized heatmap across projects 
#by Rawan

#' @export
projects_heatmap_scores=function(ps,rows=NULL, levels=NULL, legendtitle='Gene\nscore\n '){
  s=projects_summarizeresults(ps)
  
  if(is.null(rows)){
    dbg_warnf('No specific genes given to plot, using the top 10 average scoring genes
                across all projects')
    rows=1:10
  }
  
  avg=s[rows,"Average_score", drop=F]
  s$Average_score=NULL
  plot=s[rows,]
  p.known=matrix(nrow=nrow(plot), ncol=ncol(plot))
  
  for(i in 1:ncol(plot)){
    knowntar.disease=paste0(ps[[i]]$disease, '_',ps[[i]]$knownpositives__)
    c=paste0(colnames(plot)[i], '_', rownames(plot))
    r.ind=which(c %in% knowntar.disease)
    p.known[r.ind,i]='*'
  }
  
  p.known[which(is.na(p.known))]=''
  rownames(p.known)=rownames(plot)
  colnames(p.known)=colnames(plot)
  map=pancancer_diseasemap()
  
  for(i in 1:ncol(plot)){
    colnames(plot)[i]=map[[which(names(map) == colnames(plot)[i])]]$short
    colnames(p.known)[i]=map[[which(names(map) == colnames(p.known)[i])]]$short
  }
  
  plot=plot[,levels]
  p.known=p.known[,levels]
  plot=cbind(plot,avg)
  p.known=cbind(p.known,'')
  col = RColorBrewer::brewer.pal(name = "Blues", n = 5)
  ComplexHeatmap::pheatmap(plot,fontsize=14, show_rownames=T, 
                           show_colnames = T, treeheight_row = 0,
                           treeheight_col = 0, cluster_cols = F, cluster_rows = F, 
                           color = col, na_col = 'grey', 
                           border_color ='white', 
                           cellwidth = 25, cellheight =15, 
                           display_numbers = as.matrix(p.known),
                           fontsize_number = 15, fontfamily = 'Times', number_color = 'black', 
                      
                           heatmap_legend_param = list(title = legendtitle, 
                                                       title_gp= grid::gpar(fontsize = 15,fontfamily='Times'),
                                                       labels_gp = grid::gpar(fontsize = 15, fontfamily='Times')))
  
}



###############################################################
#by Rawan
#options are passed into projects_rankimpact()
#Primary caller: pancancer_optimize.rmd and/or pancancer_analyze.rmd

#' @export
projects_rankimpact_plot=function(d,...){
  if(!is.data.frame(d)){
    d=projects_rankimpact(d,...);
  }
  Iremove=colSums(d!=0)==0;
  if(any(Iremove)){ d=d[,!Iremove,drop=F];}
  
  if(!exists('unique_strings')){ source_disabled__('project.r')}
  #colnames(d)=gsub(paste0('_',ps[[1]]$rescale), '', colnames(d))
  colnames(d)=strmat_unique(stringr::str_split_fixed(colnames(d), pattern = '_',n = Inf), nc=1)
  plot.d=reshape::melt(d)
  
  #Trying to wrap the names of the columns but still very long 
  #plot.d$variabln=str_replace_all(plot.d$variable, paste0("(.{15})"), "\\1\n")
  
  ggplot2::ggplot(plot.d, ggplot2::aes(x=reorder(variable, abs(value), FUN=median, decreasing=T), y=abs(value))) + 
    ggplot2::geom_boxplot() + 
    #ggplot2::geom_jitter(shape=16, position=ggplot2::position_jitter(0.1)) + 
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, lineheight = 30)) + 
    ggplot2::xlab('') +
    ggplot2::ylab('Impact MAP')
  
  
}

# Get a data column from each project, the data from them concatenated with rbind.
projects_collectdatacol_old=function(ps, col){
  d=data.frame();
  for(p in ps){
    if(isempty(d) || grepl('^(expr|depmap)_',col)){
      
      #These are not needed. drop=F will make sure we get a data.frame from indexing.
      #Ensures that we reserve the name of the column using assign 
      #assign(paste(col), p$datawithscore[,col] )
      #Ensures that the column name in pd will be the col name given 
      #pd=db=data.frame(mget(col))
      pd = p$datawithscore[,col,drop=F];
      
      dthera=theratardb_disease2genesymbols(p$theratardisease,asframe=T, minstatusscore = 0)
      
      
      
      pd$higheststatus_=0;
      rownames(pd)=rownames(p$datawithscore)
      I=match(rownames(pd), dthera$genesymbol);
      pd$higheststatus_[!is.na(I)]=dthera[I[!is.na(I)],]$higheststatus_;
    }
    else{
      dthera=theratardb_disease2genesymbols(p$theratardisease,asframe=T, minstatusscore = 0)
      I=match(dthera$genesymbol, rownames(d))
      pd=d[dthera$genesymbol[!is.na(I)],]
      pd$higheststatus_=dthera$higheststatus_[!is.na(I)]
    }
    d=rbind(d,pd)
  }
  return(d)	
}

###############################################################
# Get a data column from each project, the data from them concatenated with rbind.
projects_collectdatacol=function(ps, col){
  d=data.frame();
  for(p in ps){
    if(isempty(d) || grepl('^(expr|depmap)_',col)){
      pd = p$datawithscore[,col,drop=F];
      pd$higheststatus_=0; #add a column of zeros, then change only the knownscore rows to their higheststatus values.
      pd[p$knownscoreinds,'higheststatus_'] = p$knownscores__[,'score'];
    }
    else{
      #only select the knownscore rows (the other rows are identical across different projects)
      pd = p$datawithscore[p$knownscoreinds, col, drop=F ];
      pd$higheststatus_=p$knownscores__[,'score'];
    }
    d=rbind(d,pd)
  }
  return(d)	
}
###############################################################
#stk__=dbg_nicestack(1); message(sprintf('projects.r sourced from: %s',stk__));
