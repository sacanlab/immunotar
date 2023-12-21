#% Copyright (C) 2022 by Ahmet Sacan
# The only two functions you need to call from this file are: project_run(), which loads 
# the settings from configuration file (see data/demo_config.yml) and runs through the entire
# analysis; and project_load(), which only loads the configuration. If you use project_load(), 
# you may examine or make changes to the loaded project configuration in your code and can then
# pass the configuration on to project_run() to complete any remaining tasks.

###############################################################
#traverse the configuration. if any list has a '...file' field, consider its value to be a file path and try to resolve it.

project_resolvefiles=function(o,basedir){
  if(is.list(o)){
    if(!is.null(names(o))){
      for(name in names(o)){
        if(endsWith(name,'file') && is.character(o[[name]])){
          file=o[[name]];
          if(file.exists(file)){ next; }
#          source('config.r')
          file=config_autofill(file);
#          source('util.r')
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

#source('dataset.r')

###############################################################


# After loading, if dosummarize, proj['data'] becomes available.
# If !dosummarize, single dataset is kept in o['dataset']; multiple datasets are kept in o['datasets'].

project_loaddatasets=function(proj,dosummarize=T, dorescale=T, ...){
  if('data' %in% names(proj)){
    # call dataset_load() so data preparation (enrich/handlenan/rescale) can be done.
    proj=dataset_load(modifyList(proj,list(dosummarize=F)));
    return(proj);
  }

  if(!('dataset' %in% names(proj)) && !('datasets' %in% names(proj))){
    warning('No dataset/datasets are configured for the project. Using all human uniprot genes as the starting set. You may explicitly use dataset="__ALLHUMANGENES__" to avoid this warning.');
    proj$dataset="__ALLHUMANGENES__"
  }
  if('dataset' %in% names(proj)){
    if('datasets' %in% names(proj)){ stop('configuration must not include both "dataset" and "datasets" options. Use "datasets" if you want to specify multiple datasets.'); }
    proj$dataset=dataset_load(proj$dataset,dosummarize=dosummarize);
  }
  else if('datasets' %in% names(proj)){
    for(i in 1:length(proj$datasets)){
      dataset=proj$datasets[[i]];
      if('datafile' %in% names(dataset)){ dataset=modifyList(list(summaryprefix=tools::file_path_sans_ext(basename(dataset$datafile)),'_'),dataset); }
      else{ dataset=modifyList(list(summaryprefix=paste0('dataset',as.character(i)),'_'),dataset); }
      dataset = dataset_load(modifyList(dataset,list(dosummarize=dosummarize, dorescale=dorescale)));
      proj$datasets[[i]]=dataset;
    }
  }

  if(dosummarize){
    if('dataset' %in% names(proj)){
      proj$data=proj$dataset$data;
    }
    else if('datasets' %in% names(proj)){
      for(i in 1:length(proj$datasets)){
        if(i==1){ proj$data=proj$datasets[[i]]$data; }
        else{
#          source('enrich.r')
          proj$data=enrich_merge(proj$data,proj$datasets[[i]]$data,by.y=0,all.y=T)
        }
      }
    }
  }
  return(proj);
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
#'    The summarized data will be in proj['data']
#'    
#'    If dosummarize = F - the data will not be summarized and you can find  the names of the datasets in proj['datasets']

# Call project_load() if you want to load the project and possibly make changes to parameters 
# or do manual analysis, before you call project_run()
# First argument is multi-purpose. It can be a yamlfile, or a project structure.

project_load=function(yamlfile,loaddatasets=T,dosummarize=T, dorescale=T){
#  source('util.r'); 
  installpackageifmissing('yaml');
  if(is.character(yamlfile)){
    proj = yaml::yaml.load_file(yamlfile)
    proj = project_resolvefiles(proj,dirname(yamlfile));
  }
  else{ proj=yamlfile; }

  if(!is.null('importfile' %in% names(proj))){
    proj$importfiles=append(proj$importfiles, proj$importfile)
  }
  
  if(length(proj$importfiles) > 0){
    for(impor in proj$importfiles){
      importyaml=yaml::read_yaml(config_autofill(impor));
      proj=modifyList(importyaml,proj)
      
    }
  }
  if(dosummarize&&!loaddatasets){ warning('Will not summarize, because loaddatasets is not requested.'); }
  if(loaddatasets){ proj=project_loaddatasets(proj,dosummarize, dorescale)}
  return(proj);
}

###############################################################

#' project_run is used to generate the IMMUNOTAR score for  each protein in the cancer expression dataset. The input into this function is either your yaml file which contains the path to the expression data-set and the configurations needed for running the algorithm or a project 
#' If project_load is run, you can use the output of project_load, if not, project_load will be run through this.
#' project_run will enrich with all the datatypes selected by the user through the yaml file or the project structure. 
#'    Options include - gtex,evodevo_pediatric,healthyprot,compartments_sp,cirfess_spc,uniprot,opentarget,surfaceopentarget,theratarsurface
#' After enriching the data column can be re-scaled based on user input and missing values or NA's in the dataset can be handled based on user input. 
#' The user can also provide weights to each enrichment column using the yaml file or the project structure
#' The proteins will then be scored based on the expression and enrichment columns and the weight vector provided. 
#' If you would like the output of the function to be a project structure with all the information that the algorithm utilizes such as the original data, the weighted data, the scored data and the MAP score of the function, you would have to specify that by adding getfull=T. 
#' If getfull=F - the output of this function will a dataframe with the scores of each protein
#' 
#' 

#load and enrich theproject. Makes proj$data available.
project_prepare=function(proj,o=list(),...){
#  source('util.r')
  o=list_merge(o,list(...));

  proj=project_load(proj,loaddatasets=T,dosummarize=T, dorescale=F);
  proj=list_merge(proj,o);
  
  #This step causes the double rescaling for the second time in the dataset. 
  #NOTE: Fixed the issue by not rescaling the data fed separately than the enriched data. There is no need for it when running project run. 
  
  #This step will do the enriching with all the databases 
  proj=dataset_load(modifyList(proj,list(dosummarize=F))); 
  
  colnames(proj$data)=gsub('depmap_probability_.*','depdamp_probability',colnames(proj$data))
  if(!is.null(proj$weights)){
    names(proj$weights)=gsub('depmap_probability_.*','depdamp_probability',names(proj$weights))
  }
  if(!is.null(proj$curves)){
    names(proj$curves)=gsub('depmap_probability_.*','depdamp_probability',names(proj$curves))
  }
  
  #we are treating project as if it is a dataset, b/c it contains $data and has the same handlenan, rescale options available.
  return(proj);  
}

# proj can be an already loaded configuration, or a configuration filename.
project_run=function(proj,o=list(),...){
  o = modifyList(list(
    getfull=T #if True, we return the entire project structure. When false, we only return the score vector.
  ),o);
  
  proj=project_prepare(proj,o,...);
	d=proj$data;
	
#	source('data_score.r')
  result=data_score(d,proj);
  if(!o$getfull){ return(result); }
  
  #modifyList works recursively when some values themselves are lists, which has undesired outcomes. Use list_merge() instead when recursive behavior would be a problem.
  #proj=modifyList(proj, result);
  
  proj=list_merge(proj, result);

#  dscore=result$data[,'score',drop=F];
#  proj$score=dscore;


  #NOTE: (Rawan) Let's not use exists(), but use !is.null() instead.
  #"Using exists() on a list does work, but I believe that R internally coerces it to an environment before checking for an object of that name, which is inefficient and can result in errors if there are any unnamed elements. "
  #https://stackoverflow.com/questions/7719741/how-to-test-if-list-element-exists  
  #if(exists('knowntargets', proj)){
  
  if(!is.null(proj[['knowntargets']])){
#    source('stat_rankeval.r');
    #we assume datawithscore is already sorted by the score.
    proj$rankeval = stat_rankeval(proj$datawithscore[,'score',drop=F], proj$knowntargets);
  }
  
  if(!is.null(proj[['reportforgenes']])){
    genesreport=c()
    for(g in proj$reportforgenes){
      genesreport=rbind(genesreport, c(g, round(genereport(proj$datawithscore[,'score',drop=F], g), 1)))
    }
    colnames(genesreport)=c("gene", "percentile_rank")
    proj[['reportforgenes']]=genesreport
  }
  
  return(proj);  
}

project_runfull=function(proj,o=list(),...){
  return(project_run(proj,o,...,getfull=T));
}
