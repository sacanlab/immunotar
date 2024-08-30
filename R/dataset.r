source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2022 by Ahmet Sacan, Rawan Shraim.
# A "dataset" ("dset" is commonly used as a variable name) is a list('data'=>data.frame(), ...) and can contain additional bookkeeping fields, eg.., dset$didsummarize.

###############################################################
dataset_applyfunction_sample=function(dset){
  print('This is dummy sample for dataset_applyfunction. No changes are done to the dataset. Another example of a function you can apply is "dataset_keeponlysurfacegenes"')
  return(dset)
}

dataset_keeponlysurfacegenes=function(data){
  source_disabled__('biodb.r')
  return(getsurfacegenes(data));
}

###############################################################
#after loading, you can use dset$data.
# We use this function for multiple purposes: to load an experimental dataset (doenrich should be OFF), and to process the project data (dosummarize should be OFF).
# dir is used to resolve data file locations with io_which()
dataset_load=function(dset,basedir=NULL, ...){
  if(is.character(dset)){
    if(dset=="__ALLHUMANGENES__"||dset=="__ALLHUMANSURFACEGENES__"){
      source_disabled__('biodb.r');
      genes=uniprotdb_allgenesymbols(9606);
      dsetorig=dset;
      dset=list(data=data.frame(matrix(,nrow=length(genes),ncol=0),row.names=genes), dosummarize=F)
      if(dsetorig=="__ALLHUMANSURFACEGENES__"){
        dset$data=getsurfacegenes(dset$data);
      }
    }
    else if(dset=="____"){
    }
    else{
      dset=list(datafile=dset);
    }
  }
  source_disabled__('util.r')
  dset=list_merge(dset,list(...));
  #checks if dataset is already loaded into dset$data 
  if('data' %in% names(dset)){
    #do nothing.
  }
  else if('datafile' %in% names(dset)){    
    datafile=dset$datafile;
    datafile=io_which(datafile,c(dir,'{datadir}'))    
    cat(paste0('Reading datafile [',datafile,']...\n'));
  	source_disabled__('util.r')
    dset$data=data_readfile(datafile,rowNames=T,header=T); #todo: need to check how the io_readfile() behaves when datafile is not an xlsx file -- will it still support rowNames=T ?
  }
  else{
    print(dset)
    stop('Cannot load dataset. One of [datafile] or [data] fields needs to be specified. The dataset entry is printed above.');
  }

  if(var_tobool(dset$keeponlysurfacegenes) && is.null(dset$didkeeponlysurfacegenes)){
    dset$data=dataset_keeponlysurfacegenes(dset$data);
    dset$didkeeponlysurfacegenes=T;
  }
  if(!is.null(dset$dataset_applyfunction) && is.null(dset$diddataset_applyfunction)){
    func=str2func(dset$dataset_applyfunction);
    dset$data=func(dset$data);
    dset$diddataset_applyfunction=T;
  }
  
  if(!is.null(dset$doenrich) && !is.null(dset.dosummarize)){
    stop('dosummarize and doenrich should not be used together. You need to call this function with dosummarize to process an experimental data and use doenrich to process the project data.');
  }

  if((is.null(dset$doenrich)&&!is.null(dset$enrich) || !is.null(dset$doenrich)&&dset$doenrich)&&(is.null(dset$didenrich)||!dset$didenrich)  && !is.null(dset$enrich)){
		if(is.null(dset$enrich)) oenrich=list()
		else oenrich=dset$enrich;
		source_disabled__('data_enrich.r')
		cat('Enriching with additional information. This may take a while...\n')
		dset$data=data_enrich(dset$data,oenrich);
    dset$didenrich=T
		#Rawan added this 04.21.24
		dset$dataraw=dset$data;
	}

  if((is.null(dset$dohandlenan)&&!is.null(dset$handlenan) || !is.null(dset$dohandlenan)&&dset$dohandlenan) && (is.null(dset$didhandlenan)||!dset$didhandlenan)){
    source_disabled__('data_handlenan.r')
    dset$data=data_handlenan(dset$data,modifyList(list(cols='__NUMERIC__'),dset));
    dset$didhandlenan=T
  }
  if((is.null(dset$dorescale)&&!is.null(dset$rescale) || !is.null(dset$dorescale)&&dset$dorescale) && (is.null(dset$didrescale)||!dset$didrescale)){
    source_disabled__('data_rescale.r')
    dset$data=data_rescale(dset$data,modifyList(list(cols='__NUMERIC__'),dset));
    dset$didrescale=T
  }
  if(!is.null(dset$dosummarize) && dset$dosummarize && (is.null(dset$didsummarize)||!dset$didsummarize)){
    source_disabled__('data_summarize.r')
    dset$data=data_summarize(dset$data,modifyList(list(cols='__NUMERIC__'),dset));
    dset$didsummarize=T
  }
  return(dset);
}
