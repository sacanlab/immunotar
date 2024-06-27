source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2022 by Ahmet Sacan
# The only two functions you need to call from this file are: project_run(), which loads 
# the settings from configuration file (see data/demo_config.yml) and runs through the entire
# analysis; and project_load(), which only loads the configuration. If you use project_load(), 
# you may examine or make changes to the loaded project configuration in your code and can then
# pass the configuration on to project_run() to complete any remaining tasks.

###############################################################
#traverse the project configuration. if any list has a '...file' field, consider its value to be a file path and try to resolve it.
source_disabled__('util.r')
source_disabled__('config.r')

project_resolvefiles=function(o,basedir){
  if(is.list(o)){
    if(!is.null(names(o))){
      for(name in names(o)){
        if(endsWith(name,'file') && is.character(o[[name]])){
          file=o[[name]];
          if(file.exists(file)){ next; }
          file=config_autofill(file);
          o[[name]]=io_resolvepath(file,basedir);
        }
        else{
          o[[name]]=project_resolvefiles(o[[name]],basedir);
        }
      }
    }
    else{
      o=lapply(o,function(x)project_resolvefiles(x,basedir));
    }
  }
  return(o);
}

###############################################################


# After loading, if dosummarize, p['data'] becomes available.
# If !dosummarize, single dataset is kept in o['dataset']; multiple datasets are kept in o['datasets'].

project_loaddatasets=function(p,dosummarize=T, dorescale=T, ...){
  if(!exists('dataset_load')){ source_disabled__('dataset.r'); }

  if('data' %in% names(p)){
    # call dataset_load() so data preparation (enrich/handlenan/rescale) can be done.
    p=dataset_load(modifyList(p,list(dosummarize=F)));
    return(p);
  }

  if(!('dataset' %in% names(p)) && !('datasets' %in% names(p))){
    warnf('No dataset/datasets are configured for the project. Using all human uniprot genes as the starting set. You may explicitly use dataset="__ALLHUMANGENES__" or dataset="__ALLHUMANSURFACEGENES__" to avoid this warning.');
    p$dataset="__ALLHUMANGENES__"
  }
  if('dataset' %in% names(p)){
    if('datasets' %in% names(p)){ stop('configuration must not include both "dataset" and "datasets" options. Use "datasets" if you want to specify multiple datasets.'); }
    p$dataset=dataset_load(p$dataset,dosummarize=dosummarize);
  }
  else if('datasets' %in% names(p)){
    for(i in 1:length(p$datasets)){
      dataset=p$datasets[[i]];
      if('datafile' %in% names(dataset)){ dataset=modifyList(list(summaryprefix=tools::file_path_sans_ext(basename(dataset$datafile)),'_'),dataset); }
      else{ dataset=modifyList(list(summaryprefix=paste0('dataset',as.character(i)),'_'),dataset); }
      dataset = dataset_load(modifyList(dataset,list(dosummarize=dosummarize, dorescale=dorescale)));
      p$datasets[[i]]=dataset;
    }
  }

  if(dosummarize){
    if('dataset' %in% names(p)){
      p$data=p$dataset$data;
    }
    else if('datasets' %in% names(p)){
      for(i in 1:length(p$datasets)){
        if(i==1){ p$data=p$datasets[[i]]$data; }
        else{
          source_disabled__('enrich.r')
          p$data=enrich_merge(p$data,p$datasets[[i]]$data,by.y=0,all.y=T)
        }
      }
    }
  }

  if(var_tobool(p$devmode)){
    warnf('Development-mode is ON (use only for code-testing, not for production). Keeping any knownpositives and only the first 10 genes.');
    p=project_minimizefordevmode(p);
  }

  return(p);
}
project_minimizefordevmode=function(p,numknownpositives=5,numothergenes=5){
  stopfif(isempty(p[['data']]),'Call this function after a project has been prepared.');
    keepgenes=c();
    if(numknownpositives>0 && !isempty(p$knownpositives)){ 
      keepgenes=base::intersect(rownames(p$data),csv(p$knownpositives));
      keepgenes=keepgenes[1:min(numknownpositives,length(keepgenes))];
    }
    if(numothergenes>0){ 
      keepmore=setdiff(rownames(p$data),keepgenes);
      if(!isempty(keepmore)){
        keepmore=keepmore[1:min(length(keepmore),numothergenes)];
        keepgenes=c(keepgenes,keepmore);
       }
    }
    Ikeep=rownames(p$data) %in% keepgenes;
    if(!all(Ikeep)){
      p$data=p$data[Ikeep, ,drop=F];
      if(!is.null(p$curveddata)){ p$curveddata=p$curveddata[Ikeep, ,drop=F]; } 
      if(!is.null(p$weighteddata)){ p$weighteddata=p$weighteddata[Ikeep, ,drop=F]; } 
      if(!is.null(p$score)){ p$score=p$score[Ikeep, ,drop=F]; }
      #the ones above should be in line with p$data, but others below may not be, so we need to locate keepgenes in them.
      if(!is.null(p$datawithscore)){ p$datawithscore=p$datawithscore[rownames(p$datawithscore) %in% keepgenes, ]; }
      p=project_resetruncounter(p);
    }
    return(p);
}

###############################################################
#import a parameter file. Used in project_load() when there are p$importfiles entries.
#we use modifyList/list_merge based on recursive=T/F
project_importfile=function(p,file,dir=NULL,recursive=T){
  path=io_which(file,c(dir,p$dir,'{datadir}')); #resolve the location of the import file, expecting it to be in the same folder as yamlfile as default.
  
  if(file == 'project_optimizedparams.yml' && !file.exists(path)){
    warnf('Optimized importfile [%s] cannot be found or does not exist. Skipping that import...', file)
    return(p);
  }
  stopfif(isempty(path)||!io_isfile(path), "importfile [%s] cannot be found or does not exist.",file);
  p$importedfiles=c(p$importedfiles,path);
  y=yaml::read_yaml(path);
  if(recursive){ if(!isempty(y)){ p=modifyList(p,y); } }
  else{ p=list_merge(p,y); }
  return(p);
}
###############################################################

#import defaultparams file.
project_reimportdefaultparams=function(p,dir=NULL){
  if(!exists('project_load')){ source('project.r'); } #this is weird and recursive, but it is to make any parallel workers happy.
  p=project_resetruncounter(p);
  #removing weights,curves,score which are no longer valid for the new parameters; they should be recalculated when the project is re-run.
  p=list_removefields(p,'weights','weightsigns','curves','score','colweight','colcurve','didprepare');
  p=project_importfile(p,'project_defaultparams.yml',dir,recursive=F); #without recursive=F, p$colweight would be merged instead of reset to defaults.
  return(p);
}

###############################################################
#sets p$defaultweightsigns from p$defaultweights. Will call project_runonce if p$defaultweights is not available.
project_filldefaultweightsigns=function(p,...){ #keep "...", when we run this in paralllel, we need to have a sink for additional arguments.
  if(!exists('project_reimportdefaultparams')){ source('project.r'); } #this is weird and recursive, but it is to make any parallel workers happy.
  o=opt_set(list(runonce=NULL,warn=T),...);
  runonce=o$runonce;
  warn=o$warn;

  if(var_tobool(runonce)&&!is.null(p[['defaultweightsigns']])){ return(p); }
  warnfif(warn && is.null(runonce) && !is.null(p[['defaultweightsigns']]),'p$defaultweightsigns is already available. Recalculating them now. Call this function only if p$defaultweightsigns is not available or set warn=F or runonce=T/F to avoid this warning.');
  q=project_reimportdefaultparams(p);
  if(is.null(q[['weights']])||!project_wasrunbefore(q)){ q=project_runonce(q) }
  p$defaultweights=q$weights;
  #p$defaultweightsigns = lapply(q[['weights']],sign);
  #let's store the weights as is as weightsigns so we can debug them better. The math we apply later don't require them to be -1,+1.
  p$defaultweightsigns = q[['weights']];
  return(p);
}

###############################################################
#sets p$weightsigns from p$weights. Will call project_runonce if p$weights is not available.
project_fillweightsigns=function(p,...){ #keep "...", when we run this in paralllel, we need to have a sink for additional arguments.
  if(!exists('project_reimportdefaultparams')){ source('project.r'); } #this is weird and recursive, but it is to make any parallel workers happy.
  o=opt_set(list(usedefaultweightsigns=T,reimportdefaultparams=NULL,runonce=NULL,warn=T),...);
  usedefaultweightsigns=o$usedefaultweightsigns;

  runonce=o$runonce;
  warn=o$warn;
  reimportdefaultparams=o$reimportdefaultparams;
  #browser();
  
  stopfif(!is.null(reimportdefaultparams), 'reimportdefaultparams argument (which had a default FALSE) is no longer used. Use usedefaultweightsigns and runonce=F if you want to reimportdefaultparams.');

  if(usedefaultweightsigns&&var_tobool(runonce)&&!is.null(p[['defaultweightsigns']])){
      if(!var_equals(p[['weightsigns']],p[['defaultweightsigns']])){ p[['weightsigns']]=p[['defaultweightsigns']]; }
      return(p);
  }
  else if(var_tobool(runonce)&&!is.null(p[['weightsigns']])){ return(p); }
  warnfif(warn && is.null(runonce) && !is.null(p[['weightsigns']]),'p$weightsigns is already available. Recalculating them now. Call this function only if p$weightsigns is not available or set warn=F or runonce=T/F to avoid this warning.');
  if(usedefaultweightsigns){
    p=project_filldefaultweightsigns(p,runonce=runonce,warn=warn); 
    p$weightsigns=p$defaultweightsigns;
    return(p);
  }  
  if(is.null(p[['weights']])||!project_wasrunbefore(p)){ p=project_runonce(p); }
  #p$weightsigns = lapply(p[['weights']],sign);
  #let's store the weights as is as weightsigns so we can debug them better. The math we apply later don't require them to be -1,+1.
  p$weightsigns = p[['weights']];
  return(p);
}
###############################################################

#' project_load is used when the user is wanting to load the cancer expression datasets into R. The user is able to summarize and re-scale the expression data-sets based on their input prior to running it through the program for target scoring. Using this function you are able to load more than a single data-set to summarize or analyze. 
#' The input to this function is a yaml file which contains the path of the expression dataset file and all the needed configurations for summarizing and re-scaling. The input could also be an R generated project structure object which contains the prior information 
#' 
#' 
#' dosummarize = T - can calculate any or all the following parameters 
#' 
#'    1. numhighexpressed -  how many samples have average or above average expression of protein
#'    2. numexpressed -  how many samples have any expression of protein 
#'    3. mean - calculate mean expression of every protein across all samples 
#'    4. median - calculate median expression of every protein across all samples 
#'  dorescale = T - can rescale numbers using any of the following methods: 
#'  
#'    1. percentile 
#'    2. log2
#'    3. z-score
#'    4. tpm
#'    5. rangemap0100 - assign numbers in vector (v) to values from 0-100 - (v - minv)/(maxv-minv) * 100)
#'    The summarized data will be in p['data']
#'    
#'    If dosummarize = F - the data will not be summarized and you can find  the names of the datasets in p['datasets']

# Call project_load() if you want to load the project and possibly make changes to parameters 
# or do manual analysis, before you call project_run()
# First argument is multi-purpose. It can be a yamlfile, or a project structure.

project_load=function(yamlfile,loaddatasets=T,dosummarize=T, dorescale=T){
  installpackageifmissing('yaml');
  if(is.character(yamlfile)){
  	yamlfile=io_which(yamlfile);
    p = yaml::yaml.load_file(yamlfile)
    p$dir = dirname(yamlfile);
    p = project_resolvefiles(p,dirname(yamlfile));
    warnfif(var_tobool(p$runcounter__),'project_load(): Newly loaded project from [%s] already has a runcounter__ set. This will prevent project_runonce() not to run the project. You may remove the runcounter__ from yaml file or in your code.',yamlfile);
  }
  else{ p=yamlfile; }

  if(!var_tobool(p$didimportfiles)){
    #array_shift optimizedparams and then defaultparams, so they appear in the order defaults,optimized.
    if(is.null(p$importdefaultparams)){
      catf("importdefaultparams is ON by default. Set the project configuration entry importdefaultparams=FALSE if you do not want to import them.")
      #do the import at the beginning, so the project parameters are loaded on top of it.
      p=list_merge(list(importdefaultparams=T),p);
    }
    if(is.null(p$importoptimizedparams)){
      catf("importoptimizedparams is ON by default. Set the project configuration entry importoptimizedparams=FALSE if you do not want to import them.")
      p=list_merge(list(importoptimizedparams=T),p);
    }

    q=list();
    for(name in names(p)){ #we go through one-by-one so the config.importfile options can take place where they appear, so configs appearing after that are not overridden.
      q[[name]]=p[[name]]; #store, even if it's an importfile field, so we can later know what was imported.
    	importfiles=NULL;
      if(name=='importfile'||name=='importfiles'){ importfiles=p[[name]]; }
      else if(name=='importdefaultparams'&&var_tobool(p$importdefaultparams)){ importfiles='project_defaultparams.yml'; }
      else if(name=='importoptimizedparams'&&var_tobool(p$importoptimizedparams)){ importfiles=config('project_optimizedparamsfile',default='project_optimizedparams.yml'); }
      #else{ q[[name]]=p[[name]]; }
      if(isempty(importfiles)){ next; }
      for(importfile in importfiles){ #it can be a list of multiple files
        q=project_importfile(q,importfile);
      }
    }
    p=q;
  	p$didimportfiles=TRUE;
  }

  if(dosummarize&&!loaddatasets){ warnf('Will not summarize, because loaddatasets is not requested.'); }
  if(loaddatasets){ p=project_loaddatasets(p,dosummarize, dorescale)}

  return(p);
}

###############################################################
project_loadcached=function(p,loaddatasets=T,dosummarize=T, dorescale=T,...){
  ocache=cache_opts(project=p,loaddatasets=T,dosummarize=T, dorescale=T,...);
  if(!ocache$recache){
    msgf('Loading project from cachefile [%s]  ...',ocache$cachefile);
    p=cache_load(ocache);
    p=project_cache_updateifneeded(p,ocache);
  }
  else{
    p=project_load(p,loaddatasets=loaddatasets,dosummarize=dosummarize, dorescale=dorescale)
    cache_save(p,ocache);
  }
  return(p);
}
###############################################################

#' project_run is used to generate the IMMUNOTAR score for  each protein in the cancer expression dataset. The input into this function is either your yaml file which contains the path to the expression data-set and the configurations needed for running the algorithm or a project 
#' If project_load is run, you can use the output of project_load, if not, project_load will be run through this.
#' project_run will enrich with all the datatypes selected by the user through the yaml file or the project structure. 
#'    Options include - gtex,evodevo_pediatric,healthyprot,compartments_sp,cirfess_spc,uniprot,opentarget,opentargetsurface,theratarsurface
#' After enriching the data column can be re-scaled based on user input and missing values or NA's in the dataset can be handled based on user input. 
#' The user can also provide weights to each enrichment column using the yaml file or the project structure
#' The proteins will then be scored based on the expression and enrichment columns and the weight vector provided. 
#' If you would like the output of the function to be a project structure with all the information that the algorithm utilizes such as the original data, the weighted data, the scored data and the MAP score of the function, you would have to specify that by adding getfull=T. 
#' If getfull=F - the output of this function will a dataframe with the scores of each protein
#' 
#' 

#load and enrich theproject. Makes p$data available.
project_prepare=function(p,...){
  if(!exists('project_load')){ source('project.r'); } #this is weird and recursive, but it is to make any parallel workers happy.
  if(!exists('dataset_load')){ source_disabled__('dataset.r'); }
  if(!exists('opt_set')){ source_disabled__('util.r'); }
  o=opt_set(...)

#setting this true will create a smaller data (10 genes + knownpositives). this is used to speed up code-testing.

  p=project_load(p,loaddatasets=T,dosummarize=T, dorescale=F);
  p=list_merge(p,list_removefields(o,c('weights','weightsigns','curves')));
  p=list_mergenonempty(p,list_selectfields(o,c('weights','weightsigns','curves'))); #only merge these if they are not empty.
  if(!isempty(o$curves)){ p=list_removefields(p,'curveddata'); }

  #This step causes the double-rescaling for the second time in the dataset. 
  #NOTE: Fixed the issue by not rescaling the data fed separately than the enriched data. There is no need for it when running project run.
  
  #This step will do the enriching with all the databases.
  p=dataset_load(modifyList(p,list(dosummarize=F))); 

  #knownpositives & knownnegatives may not all be present in the data. Remove those absent and create auxilliary indices to speed up rankeval().
  if(!is.null(p[['knownpositives']])) {
      p[['knownpositiveinds']] = match(p[['knownpositives']], rownames(p$data)); #save in p to speed-up stat_rankeval
      Ifound=!(is.na(p[['knownpositiveinds']]));
      msgfif(!all(Ifound),'project_prepare: The following knownpositives are not in the data matrix: %s ....',str_csv(p$knownpositives[!Ifound]));
      p[['knownpositiveinds']] = p[['knownpositiveinds']][Ifound];
      p[['knownpositives__']]=p[['knownpositives']][Ifound];
      p[['numknownpositives__']]=length(p[['knownpositives__']])
      if(!is.null(p[['knownpositivescores']])){ p[['knownpositivescores__']]=p[['knownpositivescores']][Ifound,]; }
  }
  if(!is.null(p[['knownnegatives']])) {
      p[['knownnegativeinds']]=match(p[['knownnegatives']], rownames(p$data)); 
      Ifound=!(is.na(p[['knownnegativeinds']]));
      msgfif(!all(Ifound),'project_prepare: The following knownnegatives are not in the data matrix: %s ....',str_csv(p$knownnegatives[!Ifound]));
      p[['knownnegativeinds']] = p[['knownnegativeinds']][Ifound];
      p[['numknownnegatives__']]=length(p[['knownegatives__']])
      p[['knownnegatives__']]=p[['knownnegatives']][Ifound];
  }
  if(!is.null(p[['knownscores']])){
  	p[['knownscoreinds']]=match(rownames(p[['knownscores']]), rownames(p$data)); 
  	Ifound=!(is.na(p[['knownscoreinds']]));
  	#msgfif(!all(Ifound),'project_prepare: The following knownnegatives are not in the data matrix: %s ....',str_csv(p$knownnegatives[!Ifound]));
  	p[['knownscoreinds']] = p[['knownscoreinds']][Ifound];
  	p[['knownscores__']]=p[['knownscores']][Ifound,,drop=F];
  }


  p$didprepare=T;
  #we are treating project as if it is a dataset, b/c it contains $data and has the same handlenan, rescale options available.
  return(p);  
}

###############################################################
# p can be an already loaded configuration, or a configuration filename.
project_run=function(p,...){
  o = opt_set(
    getfull=T #if True, we return the entire project structure. When false, we only return the score vector.
    ,getrankeval=F
  	,...);
  #list-prepare is expensive, let's avoid it if possible.
  if(!is.list(p)||is.null(p$didprepare)||!p$didprepare){    p=project_prepare(p,o);  }
  else{
    p=list_merge(p,list_removefields(o,c('weights','weightsigns','curves')));
    p=list_mergenonempty(p,list_selectfields(o,c('weights','weightsigns','curves'))); #only merge these if they are not empty.
  }
	d=p$data;
	
  if(!exists('data_score')){ source_disabled__('data_score.r'); }
  if(!exists('stat_rankeval')){ source_disabled__('stat_rankeval.r'); }

  result=data_score(d,p);
  
  if(o$getrankeval||o$getfull){ #efficiency shortcut when only the rankeval is requested. used in projects_evalweightandcurvevector().
    #knownpositiveinds and knownnegativeinds are setup from knownpositives/knownnegatives in project_prepare()
    rankevalpos=NA;
  	if(!isempty(p[['knownpositiveinds']])){
      if(!is.null(p[['knownpositivescores__']])){  rankevalpos = stat_rankeval(result[['score']], p[['knownpositiveinds']], method='wmap', positiveweights=p[['knownpositivescores__']]);    }
      else{ rankevalpos = stat_rankeval(result[['score']], p[['knownpositiveinds']], method='map');  }
  	}
    rankevalneg=NA;
  	if(!isempty(p[['knownnegativeinds']])){
  		rankevalneg = stat_rankeval(result[['score']], p[['knownnegativeinds']]);
  	}
    if(is.na(rankevalpos)&&!is.na(rankevalneg)){ rankeval=-rankevalneg; }
    if(!is.na(rankevalpos)&&is.na(rankevalneg)){ rankeval=rankevalpos; }
    else{
     #when both pos&neg are available, the result is a weighted combination.
     rankeval = (rankevalpos*length(p[['knownpositiveinds']]) - rankevalneg*length(p[['knownnegativeinds']]))/max(length(p[['knownpositiveinds']]),length(p[['knownnegativeinds']]))
    }
    if(o$getrankeval){ return(rankeval); }
  }
  if(!o$getfull){ return(result); }
  p$rankeval=rankeval;
  
    
  #modifyList works recursively when some values themselves are lists, which has undesired outcomes. Use list_merge() instead when recursive behavior would be a problem.
  #proj=modifyList(p, result);
  p=list_merge(p, result);

  #NOTE: (Rawan) Let's not use exists(), but use !is.null() instead.
  #"Using exists() on a list does work, but I believe that R internally coerces it to an environment before checking for an object of that name, which is inefficient and can result in errors if there are any unnamed elements. "
  #https://stackoverflow.com/questions/7719741/how-to-test-if-list-element-exists  
  #if(exists('knownpositives', p)){
  
  
  if(!is.null(p[['reportforgenes']])&&!isempty(p[['reportforgenes']])){
    genesreport=c()
    for(g in csv(p$reportforgenes)){
      genesreport=rbind(genesreport, c(g, round(genereport(p$datawithscore[,'score',drop=F], g), 1)))
    }
    colnames(genesreport)=c("gene", "percentile_rank")
    p[['reportforgenes']]=genesreport
  }
  p$runcounter__=var_pickfirstnotnull(p$runcounter__,0)+1;
  return(p);  
}

###############################################################
project_runsimple=function(p,...){
  return(project_run(p,...,getfull=F))
}
###############################################################
# Run and get full/detailed output.
project_runfull=function(p,...){
  return(project_run(p,...,getfull=T));
}

###############################################################
project_wasrunbefore=function(p){
  return(is.list(p)&&var_tobool(p$runcounter__));
}
###############################################################
project_resetruncounter=function(p){
  if(var_tobool(p$runcounter__)){  p$runcounter__=0; }
  return(p);
}
###############################################################
# Only run if we have not run it before.
project_runonce=function(p,...){
  if(is.list(p)&&var_tobool(p$runcounter__)){ return(p); }
  #runcounter__ should be the only indicator to test whether the project was run before. Don't use score.
	#if(('score' %in% names(p))&&!is.null(p[['score']])){ return(p); }
  return(project_runfull(p,...));
}
###############################################################
# after you update a project (eg its weights), calling this function will ensure that the project is rerun to update the results.
#If the project has not been run before, no updates should be necessary so a project_run is avoided here.
project_rerunifwasrunbefore=function(p,...){
  if(!exists('project_wasrunbefore')){ source_disabled__('project.r'); }
  if(!project_wasrunbefore(p)){ return(p); }
  #msgf('Rerunning the project, b/c it was run before.');
  return(project_runfull(p));
}

###############################################################
project_optimizedparamsfile=function(...){
  o=opt_set(devmode=F,optimizedparamsfile=NULL,...);
  if(!isempty(o$optimizedparamsfile)&&o$optimizedparamsfile=='__DEFAULT__'){ o$optimizedparamsfile='project_optimizedparams.yml'; }
  yamlfile=o$optimizedparamsfile;
  if(is.logical(yamlfile)&&!yamlfile){ return(); }
  if(is.null(yamlfile)||is.logical(yamlfile)){
    yamlfile=config('project_optimizedparamsfile',default='project_optimizedparams.yml');
    if(o$devmode && !grepl('.devel.',basename(yamlfile))){
      oldyamlfile=yamlfile;
      yamlfile=io_addfilenamesuffix(yamlfile,'.devel');
      if(!io_isfile(yamlfile)){ file.copy(oldyamlfile,yamlfile); }
    }
    if(yamlfile=='project_optimizedparams.yml'||yamlfile=='project_optimizedparams.devel.yml'){ msgf('project_optimizedparamsfile(): Using %s ...',yamlfile);  }
    else{ 
      stopfif(config('project_optimizedparamsfile_stoponconfiguse',default=T),'project_optimizedparamsfile(): Using config(optimizedparamsfile): is currently considered error, so we donot accidentally use an optimizedparams file when a different one should be used. Set config("project_optimizedparamsfile_stoponconfiguse",F) if you want to ignore this error and proceed at your own risk.',yamlfile);
    }
  }
  yamlfile=getdatafile(yamlfile);
  #stopfif(!grepl('noopentarget',yamlfile),'Unexpected yamlfile %s',yamlfile);
  #if the file did not exist before, copy over from the default file.
  if(io_isnotfileorempty(yamlfile)){
    defaultyamlfile=getdatafile('project_optimizedparams.yml');
    if(io_isfileandnotempty(defaultyamlfile)){
      msgf('project_optimizedparamsfile(): Optimizedparamsfile [%s] does not exist. Creating it as a copy from [%s], but wiping the score so it can be recalculated when we run a new optimization.',yamlfile,defaultyamlfile);
       y=yaml::read_yaml(defaultyamlfile);
       y['optimscore']=NULL;
       io_mkfiledirif(yamlfile);
       yaml::write_yaml(y,yamlfile)
    }
  }
  return(yamlfile);
}
###############################################################
project_optimlogfile=function(...){
  o=opt_set(devmode=F,optimlogfile=NULL,...);
  stopfif('logfile' %in% names(o),'You must provide o$optimlogfile, not o$optimfile as an argument.');
  logfile=o$optimlogfile;
  if(is.null(logfile)){ logfile=config('project_optimlogfile'); }
  if(!is.null(logfile)&&is.logical(logfile) && !logfile){ return(); }
  if(is.null(logfile) || is.logical(logfile)){
    logfile=io_changefileext(project_optimizedparamsfile(o),'.log.xlsx');
    if(o$devmode && !grepl('.devel.',basename(logfile))){
      oldlogfile=logfile;
      logfile=io_addfilenamesuffix(logfile,'.devel');
      if(!io_isfile(logfile)){ file.copy(oldlogfile,logfile); }
    }
  }
  logfile=getdatafile(logfile);
  #if the file did not exist before, copy over from the default file.
  if(io_isnotfileorempty(logfile)){
    defaultlogfile=io_changefileext(project_optimizedparamsfile(optimizedparamsfile='__DEFAULT__'),'.log.xlsx');
    if(io_isfileandnotempty(defaultlogfile)){
      msgf('project_optimlogfile(): Optimizedlogfile [%s] does not exist. Creating it as a copy from [%s], but wiping the scores b/c they would not be valid if a new project is being used.',logfile,defaultlogfile);
      d=data_readfile(defaultlogfile);
      d[,'optimscore']=NA;
      data_writefile(d,logfile);
    }
  }
  return(logfile);
}

###############################################################
#Update optimized params (stored in YAML file) if we now have a better score.
#by Rawan, Ahmet
#if r=NULL, we read & return the yamlfile.
project_updateoptimizedparams=function(res=NULL, ...){
  o=opt_set(forceupdate=F,...);
  #if all curves are zero, also store it in a nocurve file; only when we are doing auto-filenaming.
  #Update: Curves do matter and we no longer care to store a nocurves version.
  if(FALSE && (is.null(yamlfile)||is.logical(yamlfile)&&yamlfile)&&!is.null(r$curves)&&all(r$curves==0)){
    yamlfile=project_optimizedparamsfile(o);
    if(!isempty(yamlfile)){
      project_updateoptimizedparams(r,..., optimizedparamsfile=io_addfilenamesuffix(yamlfile,'.nocurves')); }
  }

  yamlfile=project_optimizedparamsfile(o);
  if(isempty(yamlfile)){ return(NULL); }
  if(file.exists(yamlfile)){ y=yaml::read_yaml(yamlfile); }
  else{ y=list(); }
  if(is.null(res)){ return(y); }

  #change the names in res, so we can use similar names.
  r=res;
  r=list_renamefields(r,list(weights='colweight',curves='colcurve',value='optimscore'));
  r$colweight=project_scaleweights(r$colweight);
  
  r$numnonzeroweights=sum(unlist(r$colweight)!=0)
  r$numnonzerocurves=sum(unlist(r$colcurve)!=0 & unlist(r$colweight)!=0)
  r$numnonzeroparams=r$numnonzeroweights + r$numnonzerocurves;
  y$numnonzeroweights=sum(unlist(y$colweight)!=0);
  y$numnonzerocurves=sum(unlist(y$colcurve)!=0 & unlist(y$colweight)!=0);
  y$numnonzeroparams=y$numnonzeroweights + y$numnonzerocurves;

  update=o$forceupdate; saveacopy=F;
  if(is.null(y$optimscore)){ update=T; }
  

  #check if r$optimscore is better than y$optimscore by at least abstol, accounting for #features used.
  if(!update){
    isbetter_noabstol = optim_isbettereval(r$optimscore,y$optimscore,maximize=T,abstol=0); #defined so we can print a warning message.

    abstol=var_pickfirstnotnull(o[['updateoptimizedparams_abstol']],0);
    abstolperparam=var_pickfirstnotnull(o[['updateoptimizedparams_abstolperparam']],0);
    #if(abstolperparam){ abstol = (r$numnonzeroparams-y$numnonzeroparams)*abstolperparam; }
    #only consider #weights as penalized; allow to use curves freely.
    if(abstolperparam){ abstol = (r$numnonzeroweights-y$numnonzeroweights)*abstolperparam; }
    if(r$numnonzeroparams<y$numnonzeroparams){ update=optim_isequalorbettereval(r$optimscore,y$optimscore,maximize=T,abstol=abstol); }
    else{ update=optim_isbettereval(r$optimscore,y$optimscore,maximize=T,abstol=abstol); }

    if(!update && isbetter_noabstol){
      saveacopy=io_addfilenamesuffix(yamlfile,time_date(format='.better.butrejected.%Y%m%d.%H%M%S'));
      warnf('The new optim eval %f is better than the one in the yamlfile %f, but was rejected based on abstol=%f. I will NOT update the YAML file, but am saving a copy in: %s',r$optimscore,y$optimscore,abstol, saveacopy);
    }
  }

  if(update||var_tobool(saveacopy)){
    rrr=opt_set(list_selectfields(r,csv('optimscore,numnonzeroparams,numnonzeroweights,numnonzerocurves')),optimdate=time_date(),optimcomputer=sys_computername(), optimmethod=o$method, optimcomment=o$optimcomment);
    if(!var_tobool(o$freshstart)){
      if(o$method=='none'){ rrr$optimmethod=y$optimmethod; }
      else{ rrr$optimmethod=paste0(y$optimmethod,';',o$method); }
    }
    z=list_merge(rrr,y,rrr, list_selectfields(r,csv('colweight,colcurve'))); #rrr is listed first so we get the parameters listed in the order we'd like to see them.
    if(update){ y=z; yaml::write_yaml(z, yamlfile); }
    if(var_tobool(saveacopy)){ yaml::write_yaml(z, saveacopy); }
  }
  return(y);
}

project_getname=function(p){
  return(var_pickfirstnotnull(p[['projectname']],p[['disease']],'noname'));
}
project_getshortname=function(p){
  return(var_pickfirstnotnull(p[['projectshortname']],p[['projectname']],p[['disease']],'noname'));
}
###############################################################
#Save log excel file and best weights in YAML file
#by Rawan, Ahmet

project_addoptimlog=function(res=NULL, ...){
  o=opt_set(devmode=F,removeredundancy=T,...);
  logfile=project_optimlogfile(o);
  if(file.exists(logfile)){ d=data_readfile(logfile,rowNames=F);  }
  else{ warnf('optimlog file [%s] does not exist (yet).',logfile); d=data.frame(); }
  if(is.null(res)){return(d);}
  res$weights=project_scaleweights(res$weights);

  r=list(optimscore=res$value, optimdate=time_date(), optimcomputer=sys_computername(), optimmethod=o$method, optimcomment=o$optimcomment);
  r=c(r, list_make(strs_withprefix(names(res$weights),'weight:'),res$weights));
  r=c(r, list_make(strs_withprefix(names(res$curves),'curve:'),res$curves));
  d=plyr::rbind.fill(d, as.data.frame(t(r)))
  d[,grepl('weight:',colnames(d))]=project_scaleweights(d[,grepl('weight:',colnames(d))]);
  m=d[,grepl('weight:',colnames(d))|grepl('curve:',colnames(d))];
  m[is.na(m)]=0; d[,grepl('weight:',colnames(d))|grepl('curve:',colnames(d))]=m;
  d=d[!duplicated(d[,grepl('weight:',colnames(d))|grepl('curve:',colnames(d))]),];

  if(o$removeredundancy){
    d=project_optimlog_removeredundancy(optimlogfile=logfile,o,d__=d);
  }
  data_writefile(d, logfile,rowNames=F);
  d=type.convert(d,as.is=T);
  return(d) 
}

###############################################################
#get the optimlog entries as a matrix, for use in Genetic Algorithms.
project_getoptimlogpopulation=function(...){
	o=opt_set(weights=NULL,curves=NULL,Iweights=NULL,Icurves=NULL,d__=NULL,removeredundancy=T,getdetailed=F,...);
  if(is.null(o$d__)){  d=project_addoptimlog(res=NULL,o); }
  else{ d=o$d__; }
  if(o$removeredundancy){
    d=project_optimlog_removeredundancy(o,d__=d);
  }
	if(isempty(d)){
		weights=NULL; curves=NULL; pop=NULL;
	}
	else{
		d=as.data.frame(d);
	  wlog=d[,grepl('^weight:',colnames(d)),drop=F]; colnames(wlog)=gsub('^weight:','',colnames(wlog)); wlog=type.convert(wlog, as.is=T);
	  clog=d[,grepl('^curve:',colnames(d)),drop=F]; colnames(clog)=gsub('^curve:','',colnames(clog)); clog=type.convert(clog, as.is=T);
	  wlog=project_scaleweights(wlog);
	  weights=wlog; curves=clog;
	  if(!is.null(o$Iweights)){
	  	if(isempty(o$Iweights)){ wlog=c(); } #o$Iweights is list().
	  	else{
	  		stopfif(isempty(o$weights),'If you give Iweights, you must also provide the weights named list so I can resolve the names and put them in the same order.');
	  		#browser()
	  		selectcols=names(o$weights)[o$Iweights];
	  		wlog[,selectcols[!(selectcols%in%colnames(wlog))]]=0; #add any non-existent columns and set to NA. (which is later modified to 0)
	  		wlog=wlog[,selectcols,drop=F];
	  		}
	  }
	  else if(!is.null(o$weights)){ wlog=wlog[,names(o$weights)]; }
	  if(!is.null(o$Icurves)){
	  	if(isempty(o$Icurves)){ clog=c(); } #o$Icurves is list().
	  	else{
		  	stopfif(isempty(o$curves),'If you give Iweights, you must also provide the weights named list so I can resolve the names and put them in the same order.');
		  	clog=clog[,names(o$curves)[o$Icurves],drop=F];
	  	}
	  }
	  else if(!is.null(o$curves)){ clog=clog[,names(o$curves),drop=F]; }

    pop=c();
    if(!isempty(wlog)){ colnames(wlog)=paste0('weight:',colnames(wlog)); pop=cbind(pop,data.matrix(wlog)); }
    if(!isempty(clog)){ colnames(clog)=paste0('curve:',colnames(clog)); pop=cbind(pop,data.matrix(clog)); }
    pop[is.na(pop)]=0;  
	  #REMINDER: Do not remove any rows. Other functions may rely on d, wlog, clog, and pop to be in the same row-order.    
	}
  if(o$getdetailed){
    return(list(data=d, weights=weights, curves=curves, pop=pop));
  }
  return(pop);
}
###############################################################
project_optimlog_removeredundancy=function(...){
  o=opt_set(maxcorrelation=0.95,d__=NULL,...); #d__ will be passed to getoptimlogpopulation(). If d__ is given, we do not writefile()
  logfile=project_optimlogfile(o);
  log = project_getoptimlogpopulation(o,optimlogfile=logfile,getdetailed=T,weights=NULL,curves=NULL,Iweights=NULL,Icurves=NULL,removeredundancy=F); #need to not-select for any weights/curves, so the correlation is based on all.
  d=log$data;
  if(isempty(d)){ return(d); }
  cors=cor(as.matrix(t(log$pop)))
  Iremove=rep(FALSE,nrow(d));
  Ihasscore=rep(FALSE,nrow(d)); 
  for(i in 1:nrow(d)){ Ihasscore[[i]] = !isempty(d[[i,'optimscore']]) && !is.na(d[[i,'optimscore']]); }
    
  for(i in 1:nrow(d)){
    if(Iremove[[i]] || !Ihasscore[[i]]) { next; }
    I=cors[i,]>o$maxcorrelation;
    I[[i]]=F; #don't allow self-correlation-removal
    I[is.na(I)]=F; #if we have all-zeros in a row, its correlation with others is undefined, and >o$maxcorrelation will result in NA.
    I[Iremove]=F;
    I[!Ihasscore]=F;
    if(!any(I)){ next; }
    for(j in which(I)){
      #only consider them the same if they have the same parameters turned on/off
      if(d[j,'optimscore']>=d[i,'optimscore'] && all((log$pop[i,]==0)==(log$pop[j,]==0) )){ Iremove[[i]]=TRUE; }
    }
  }
  if(any(Iremove)){
    msgf('Removing [ %d of %d ] redundant log entries. from the log file [ %s ]',sum(Iremove),nrow(d),logfile);
    d=d[!Iremove,]
    if(is.null(o$d__)){
      warnf('Writing non-redundant entries back to log file [ %s ]',logfile);
      data_writefile(d,logfile);
    }
  }
  return(invisible(d));
}

###############################################################
#store/backup the result of an optimization (not necessary, but just in case)
project_optimbackup=function(res){
  res$ga=NULL; #we don't backup a genetic algorithm object
  file=io_name(sys_cachedir(),io_sanitizefilename(paste0('optim.',time_date(),'.yml')));
  yaml::write_yaml(res,file)
  return(file)
}

###############################################################
#private function to calculate the rankdiff of one gene and one data column (kept as separate function so we can use list_map() to do this on all genes.
#row number is what we apply the listmap to, which is why it is the first argument here.
project_rankimpact_rankdiff__=function(row,p,col,colmin,colmax,...){
	if(!exists('data_score')){ source_disabled__('data_score.r'); }
  #orig=p$data[[row,col]]; #backup of the original value.
	p$getfull=F;
	
  p$data[[row,col]]=colmin;
  scores=data_score(p$data,p); #run project_runsimple() (DO NOT USE project_runfull(), it is unnecessary here and will take more time.)
  rankmin = rank(scores)[row]; #get the rank of ri's score in the list of scores.
  
  #repeat with the max of ci'th column.
  p$data[[row,col]]=colmax;
  scores=data_score(p$data,p); #run project_runsimple() (DO NOT USE project_runfull(), it is unnecessary here and will take more time.)
  rankmax = rank(scores)[row];
  rankdiff=rankmax-rankmin; #the last thing in the loop is what gets accumulated.
	msgf('row %d: rankmin=%f, rankmax=%f',row,rankmin,rankmax)
  return(rankdiff);
}

###############################################################
#Calculate the impact of each column by estimating its effect on the rank of genes.
#Primary caller is projects_rankimpact().
project_rankimpact=function(p,...){
	if(!exists('project_rankimpact_rankdiff__')){ source('project.r'); } #this is weird and recursive, but it is to make any parallel workers happy.
	if(!exists('opt_set')){ source_disabled__('util.r'); }

  o=opt_set(
    numrandrows=2 #if non-zero, we run a test for every gene.
    #add any other options here
    ,doparallel=T
		,domean=F #get a matrix of all simulations or average them?
		,docolgroups=F #TODO: Ahmet calculate rank impact for feature groups such as all gtex 
  ,...)
  
  p=project_runonce(p);
  d=p$data;
  cnames=colnames(d);
  #Calculate the min and max of each column before the main for loop.
  colmins = apply(d,2,min,na.rm=T)
	colmaxs = apply(d,2,max,na.rm=T)

	if(o$numrandrows!=0){
    warnfif(o$numrandrows>nrow(d),"Requested numrandrows [%d] is greater than the number of rows [%d] in project data has. Ignoring numrandrows and using all data rows...",o$numrandrows,nrow(d))
    o$numrandrows=min(o$numrandrows,nrow(d)); }
  if(o$numrandrows==0||o$numrandrows==nrow(d)){ Irows=1:nrow(d); }
  else{  Irows=sample(nrow(d),o$numrandrows);  }
	
	if(!exists('data_score')){ source('data_score.r'); } #data_score will be needed in list_map function call.
	parallel_startglobalcluster(numtasks=length(Irows));
	
  impacts=data_new(nrow=length(Irows),colnames=colnames(d)); #store impact of each test
  for(ci in 1:length(cnames)){
    cname=cnames[[ci]];
    w = p[['weights']][[cname]];
    warnfif(is.null(w),"project_rankimpact(): Unexpected: The column [%s] does not have a weight associated with it.");
    if(w==0){ impacts[,ci]=0; next; } #if weight is 0, the impact is zero, no need to run tests.
		colmin=colmins[[ci]];
		colmax=colmaxs[[ci]];
    
    impacts[,ci] = unlist(list_map(Irows,project_rankimpact_rankdiff__,list(doparallel=o$doparallel), p=p,col=ci,colmin=colmin,colmax=colmax));
  }
  if(o$domean){ impacts=colMeans(impacts,na.rm=T)/nrow(d)*100;  }  #return as a percentile impact
  else{ impacts=impacts/nrow(d)*100; }
	return(impacts)
}

###############################################################
#optimization sometimes results in very large weights. I use this function to scale them to a reasonable level.
#set bysum=0/F, bymax=0/F to turn either off. bysum=T defaults to 100, bymax=T defaults to 3.
#We have switched to bymax=3 (previously 10), so we can user lower=upper=-3/3 for all weights & curves during optimization.
project_scaleweights=function(W, bysum=F, bymax=3){
  if(isempty(W)){ return(W); }
  if(is.list(W)&&!is.data.frame(W)){
    v = project_scaleweights(t(as.numeric(W)),bysum=bysum,bymax=bymax);
    return(list_make(names(W),as.list(v)));
  }
  W=type.convert(W, as.is =TRUE)
  for(i in 1:nrow(W)){ for(j in 1:ncol(W)){ if(isempty(W[[i,j]])){ W[[i,j]]=NA; }}}  
  Inan=is.na(W); 
  if(any(Inan)){ W[Inan]=0; }

  if(var_tobool(bysum)){
    if(identical(bysum,T)){ bysum=100; }
    scale = bysum/rowSums(abs(W) );
    W = W*scale;
  }
  if(var_tobool(bymax)){
    if(identical(bymax,T)){ bymax=3; }
    if(any(W>bymax)){
      scale = bymax/apply(abs(W),1,max);
      W = W * scale;
    }
  }
  #if(any(Inan)){ W[Inan]=NA; } #leave them as zero.
  return(W);
}

###############################################################
#We sometimes cache an individual project. This function checks if any of the importfiles used in the project have been modified and if so, updates the project (and the cache)
#There's a version of this function projects_cache_updateifneeded, used for cacheing  a collection of projects, where we do not cache them individually, but use this function to check and update. For collection of projects, projects_cache_updateifneeded is responsible for saving the cache.
project_cache_updateifneeded=function(p,ocache,...){
  o=opt_set(
    dosave=T
    ,dorerunifneeded=T
    ,...
  )
  attr(p,'modified')=FALSE;

  changedfiles=c();
  recachedata=F;
  #check if any of the datafile's have been modified
  datafiles=c(p$dataset$datafile,lists_extractfield(p$datasets,'datafile'));
  for(i in forlength(datafiles)){
    datafile=datafiles[[i]];
    if(!io_isfileandnotempty(datafile)){ warnf('Datafile [%s] does not exist. Not updating the project cache.',datafile); return(p); }
    if(!recachedata && io_isnewer(datafile,ocache$cachefile) ){ recachedata=T; changedfiles=c(changedfiles,datafile); }
  }


  recacheimports=F;
  if(var_tobool(p$didimportfiles)){
	  for(i in forlength(p$importedfiles)){
	    importfile=p$importedfiles[[i]];
	    importfile=getdatafile(importfile);
	    if(!io_isfileandnotempty(importfile)){ warnf('Importfile [%s] does not exist. Not updating the project cache.',importfile); return(p); }
	    if(!recacheimports && io_isnewer(importfile,ocache$cachefile) ){ recacheimports=T; changedfiles=c(changedfiles,importfile); }
	    if(!recacheimports && !io_isnewer(ocache$cachefile,time_numeric('2024-02-17 16:00:00')) ){ recacheimports=T; }
	  }
  }
  if(recachedata||recacheimports){
    p=list_removefields(p,csv('weights,curves,colcurve,colweight,curveddata,weigtheddata,score,datawithscore'));
    if(recachedata){
      p=list_removefields(p,csv('data'));
      p[grepl('^did',names(p))]=NULL;  #not ideal, but I am removing anything that starts with did (eg., 'didenrich');
    }
    p$didimportfiles=FALSE; p=project_prepare(p); #this will call project_load() which in turn will reload the imporfiles (and the data if needed.)
    if(o$dorerunifneeded){ p=project_rerunifwasrunbefore(p); }
    if(o$dosave){
      msgf('project_cache_updateifneeded: Re-loading the project data and/or parameters, because the following files have been modified:\n%s',changedfiles);
      cache_save(p,ocache);
    }
    attr(p,'modified')=TRUE;
  }
  return(p);
}

###############################################################
#put colnames into groups based on primary data source (we assume the prefix of a colname up to the first "_" represents the data source)
#this function returns a list where keys are the group names and values are the colnames belonging to that group.
project_groupcolnames=function(cols){
  gnames = gsub('_.*','',cols);
  return(arr_groupby(cols,gnames));
}
###############################################################
#select the data columns having the highest weights
#topk: using 0 disables this.
#minabsweight: absolute-value threshold. Using 0 selects any weight>0. Using a negative number will select all weights.
#bestingroup: whether to keep only one from each datasource.
project_selectcolsbyweight=function(p,minabsweight=0,bestingroup=F){
  p=project_runonce(p);
  w = p$weights;
  w = w[ abs(unlist(w)) > minabsweight ];
  if(bestingroup){
    namegroups =  project_groupcolnames(names(w));
    cols=c()
    for(gi in forlength(namegroups)){
      gnames=namegroups[[gi]];
      if(length(gnames)==1){ cols=c(cols,gnames); }
      else{
        cols=c(cols, gnames[[which.max(abs(unlist(w[gnames]))) ]] );
      }
    }
    w=w[cols];
  }
  return(names(w[order(-abs(unlist(w)))]));
}

#the contents to be moved into project.r

###############################################################
#create plot data structure for use with ggplot.
#if you feel the need to customize the plot, do so by adding options to the o list below.
project_rankplot=function(p, ...){
  if(!exists('opt_set')){ source_disabled__('util.r'); }
  o=opt_set(
    nudge_x=8 #x/y offset for label of known targets.
    ,nudge_y=10 #defaults to score-range/20
    ,withboxes=T #draw boxes around labels?
    ,knownpositives=p$knownpositives
    ,validatedpositives=p$validatedpositives
    ,validatednegatives=p$validatednegatives
    ,includequantile=F
    ,...)
  #datawithscore is already sorted by score.
  dplot=p$datawithscore[,colnames(p$datawithscore) %in% c('score'),drop=F];
  
  dplot$rank=1:nrow(dplot);
  knownpositives=csv(o$knownpositives);
  dplot$label=''; #we'll use these as text labels on the plot.
  I=which(rownames(dplot) %in% c(knownpositives, o$validatedpositives, o$validatednegatives));
  dplot[I,'label']=apply( cbind(rownames(dplot)[I]) , 1 , paste , collapse = "." );
  dplot$isknowntarget=FALSE;
  dplot$isknowntarget[I]=TRUE;
  
  if(!is.null(o$validatedpositives)){
    v=which(rownames(dplot) %in% o$validatedpositives)
    nv=which(rownames(dplot) %in% o$validatednegatives)
    dplot$isvalidtarget="a";
    dplot$isvalidtarget[v]='b';
    dplot$isvalidtarget[nv]='c'
  }
  
  
  dplot=dplot[order(dplot$isknowntarget, decreasing = F),]
  
  installpackageifmissing('ggplot2,ggrepel')
  library(ggplot2); library(ggrepel)
  
  
  g=ggplot(dplot, aes(x=rank, y=score)) + geom_point(aes(color=isknowntarget), size=3)+
    scale_color_manual(values=c('grey', 'black')) +
    theme_bw() + theme(legend.position = 'none') +
    theme(text=element_text(size=15, family='Arial')) + 
    ylab('IMMUNOTAR Score') + xlab('Protein Rank');
  if(o$includequantile){
    g=g+geom_hline(yintercept =as.numeric(quantile(dplot$score, 0.95)), linetype='dashed', color='purple') +
      geom_hline(yintercept =as.numeric(quantile(dplot$score)[2]), linetype='dashed') +
      geom_hline(yintercept = as.numeric(quantile(dplot$score)[3]), linetype='dashed') +
      geom_hline(yintercept = as.numeric(quantile(dplot$score)[4]), linetype='dashed')
  }
  
  if(is.null(o$nudge_y)){ o$nudge_y=diff(range(dplot$score,na.rm=T))/20; }
  if(!isempty(I)){
    g=g+geom_label_repel(aes(label=label, fill=dplot$isvalidtarget), max.overlaps = 1000, family='Arial', xlim = c(1, NA)) + scale_fill_manual(values=c('white', 'cadetblue2', 'tomato1')) 
  }
  
  return(g)
}


##############################################################################
#Creating unique strings
strmat_unique=function(x, n = nrow(x),nc=1,...) {
  if(nc==0){
    dbg_warnf('Not changing the names of the columns')
    return(x)
  }
  #x = matrix of words 
  #n = all strings 
  #nc = minimum string n to include
  ## join the first nc words
  installpackageifmissing('stringi,stringr')
  s <- stringi::stri_trim(apply(x[, 1:nc, drop = FALSE], 1, stringi::stri_join, collapse = " "))
  ## find non-duplicated word combinations, and store in column 1
  nodups <- !s %in% s[stringi::stri_duplicated(s)]
  x[nodups, 1] <- s[nodups]
  ## remove extra words from the matrix
  x[nodups, -1] <- ""
  ## if some strings are not unique, do it again, increasing nc by one
  if(any(x[, 2] != "")) {
    x <- strmat_unique(x = x, n = n, nc = nc + 1)
    ## otherwise, grab the unique sub-phrases from column 1    
  } else {
    x <- x[, 1]
  }
  x=gsub(' ', '_', x)
  return(x)
}    

##############################################################################
#New implementation by Rawan 
#Shorten the names of features 
project_shortenfeaturenames=function(cols,removescaling=T,...){
  if(removescaling){
    if(!exists('vec_rescaletypes')){ source_disabled__('vec_rescale.r');}
    cols=gsub(paste0('_(',paste0(vec_rescaletypes(),collapse='|'),')$') , '', cols)
  }
  #warnfif(!is.null(rescaletype),"rescaletype is obsolete. use removescaling=T/F.");
  #cols=gsub(paste0('_',rescaletype,'$'), '', cols);
  
  installpackageifmissing('stringr')
  word.mat = stringr::str_split_fixed(cols, pattern = '_',n = Inf)
  ## function to extract unique words
  cols.n=strmat_unique(word.mat, ...)
  return(cols.n);
}

########################################################################
#Selecting features with highest weights from each enrichment dataset 
project_selectheatmapfeatures=function(p, medianexp=F, ...){
  features=c()
  f.names=unique(gsub("_.*", "", names(p$weights)))
  for(i in f.names){
    f=names(which.max(abs(unlist(p$weights[grepl(i,names(p$weights))]))))
    features=c(features,f)
  }
  if(medianexp){
    features=features[-grepl('expr', features)]
    features=c(names(p$weights)[grepl('expr_median', names(p$weights))], features)
  }
  return(features)
}

###############################################################
#Heatmap of ImmunoTar features
# by Rawan
#rows: which rows to show. defaults to 1:10
#colnames: which columns to show. will default to some pre-selected columns that we like showing.
#withexprcol:  if true, we add expr_mean_* column to colnames.
#any additional arguments are passed into ComplexHeatmap::pheatmap
project_resultheatmap=function(p,rows=NULL,cols=NULL,withexprcol=F, medianexp=F, legendtitle='Feature\nValue\n', ...){
  o=opt_set(
    markgenesby=NULL #use one or more p fields. e.g., use 'knownpositives'
    ,imgfile=NULL #if given, we'll save the plot into that file.
    ,title=NULL
    ,cluster_r=F
    ,...
  )
  p=project_runonce(p);
  d=p$datawithscore;
  if(is.null(cols)){
    dbg_warnf('No specific features selected to plot, using features with the highest weights within each enrichment database')
    cols=project_selectheatmapfeatures(p, medianexp=medianexp, ...)
    #If we need it to be cancer specific add 
    if(!withexprcol){
      cols=cols[-grep('expr|depmap|GO', cols)]
    }
  }
  
  I=cols %in% colnames(d);
  if(!all(I)){
    warnf("project_resultheatmap(): Removing the following requested columns; they are not present in the data.",str_csv(cols[!I]));
    cols=cols[I];
  }
  stopfif(isempty(cols),"Empty list of colnames.")
  if(isempty(rows)){
    rows=1:min(nrow(d),10)
    dbg_warnf('Selecting the top 10 genes to display')
  }
  else if(is.character(rows)){
    rows=base::intersect(rows,rownames(d));
    if(isempty(rows)){
      dbg_warnf('None of the requested genes are in the data. Not creating the heatmap...');
      return(invisible(NULL)); }
  }
  
  #rangemap features, so they show up with similar colors as the rest.
  source_disabled__('vec_rescale.r')
  for(name in cols){
    d[,name]=vec_rescale(d[,name], type = 'rangemap0100')
  }
  d=d[rows, cols]
  colnames(d)=project_shortenfeaturenames(cols = colnames(d),...)
  installpackageifmissing_bioc('ComplexHeatmap')
  #installpackageifmissing('viridis')
  col = RColorBrewer::brewer.pal(name = "Purples", n = 5)
  if(!is.null(o$markgenesby)){
    d[,colnames(d)[grepl('opentargets', colnames(d))]]=NULL
  }
  #ht_opt=ComplexHeatmap::ht_opt()
  #ht_opt$heatmap_row_names_gp= grid::gpar(fontsize = 20)
  #ht_opt$heatmap_column_names_gp= grid::gpar(fontsize = 20)
  colnames(d)[which(colnames(d) == 'opentargetsurface')]='PMTLSurface'
  colnames(d)[which(colnames(d) == 'expr')]='cancer_expr'
  h=ComplexHeatmap::pheatmap(d,fontsize=10,show_rownames=T, show_colnames = T, treeheight_row = 0,
                             treeheight_col = 0, cluster_cols = F, cluster_rows = o$cluster_r, 
                             color = col, na_col = 'grey', border_color = 'white', 
                             cellwidth = 20, cellheight =10, fontsize_col = 10,
                             fontsize_row = 9, main = o$title,
                             heatmap_legend_param = list(title = legendtitle, 
                                                         title_gp= grid::gpar(fontsize = 10, y=10, x=10),
                                                         labels_gp = grid::gpar(fontsize = 10),
                                                         title_position = "topcenter", # Title position to avoid overlapping
                                                         labels_position = "left"
                                                        ))
  
  
  o$markgenesby=csv(o$markgenesby);
  for(byi in forlength(o$markgenesby)){
    by=o$markgenesby[[byi]];
    bygenes=csv(p[[by]]);
    I=which(rownames(d) %in% bygenes);
    if(!length(I)){ next; }
    pch=rep(NA,nrow(d));
    pch[I]=20; #this is just the marker style. google pch style.
    an=ComplexHeatmap::rowAnnotation(NEEDTOCHANGE=ComplexHeatmap::anno_simple(rep(0,nrow(d)),col=circlize::colorRamp2(c(0,1),c('white','white')),pch=pch, gp = grid::gpar(fontsize = 10, fontfamily='Arial'), pt_gp = grid::gpar(fontsize = 10,fontfamily='Arial')))
    if(by=='opentarget'){by='PMTL'}
    if(by=='curated'){by='Validated'}
    an@anno_list$NEEDTOCHANGE@label=by; #I couldn't change the name of the annot without this hack.
    h=h+an
  }
  if(!isempty(o$markgenesby)){ #adding right-annotations mess up the labels and we have to reannotate here.

    h=h+ComplexHeatmap::rowAnnotation(rn = ComplexHeatmap::anno_text(rownames(d), gp = grid::gpar(fontsize = 10, fontfamily='Arial')))  
   
    #,location = unit(0, "npc"), just = "left"))

  }
  
  
  fig_export(h,o);
  
  return(ComplexHeatmap::draw(h, heatmap_legend_side = "right", padding = grid::unit(c(2, 10, 2, 2), "pt")))
}


###############################################################
fig_export=function(h,...){
  o=opt_set(
    imgfile=NULL #if not given, we'll automatically decide (TODO)
    ,overwrite=NULL
    ,...
  )
  if(!isempty(o$imgfile)){
    if(!io_isrealpath(o$imgfile)){
      figdir=config('figdir',default='.');
      o$imgfile=io_name(figdir,o$imgfile)
    }
    fileext=io_fileext(o$imgfile);
    func=str2func(fileext); #Limitation: fileext will need to be one of the ones that is supported, e.g., .tiff, .png
    if(io_isfileandnotempty(o$imgfile)&&!var_tobool(o$overwrite)){ warnf('File [%s] already exists. Delete it if you want to overwrite, or use overwrite=T')}
    else{
      #ggplot2::ggsave(o$imgfile) #does not work.
      func(o$imgfile, res=600, width = 7, height = 7, units = 'in') 
      ComplexHeatmap::draw(h) #TODO: Fix cropping issues. ggplot2 probably does a better job 
      dev.off();
      #ggplot2::ggsave(o$imgfile,plot=plt) #does not work.
    }
  }
  return(o$imgfile);
}

project_heatmap_scores=function(ps,rows=NULL, levels=NULL, legendtitle='Gene\nscore\n '){
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
  ComplexHeatmap::pheatmap(plot,fontsize=12, show_rownames=T, 
                           show_colnames = T, treeheight_row = 0,
                           treeheight_col = 0, cluster_cols = F, cluster_rows = F, 
                           color = col, na_col = 'grey', 
                           border_color ='white', 
                           cellwidth = 25, cellheight =15, 
                           display_numbers = as.matrix(p.known),
                           fontsize_number = 15, fontfamily = 'Times', number_color = 'black', 
                           
                           heatmap_legend_param = list(title = legendtitle, 
                                                       title_gp= grid::gpar(fontsize = 12,fontfamily='Times'),
                                                       labels_gp = grid::gpar(fontsize = 12, fontfamily='Times')))
  
}



#stk__=dbg_nicestack(1); message(sprintf('project.r sourced from: %s',stk__));
