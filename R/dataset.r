#% Copyright (C) 2022 by Ahmet Sacan, Rawan Shraim.
# A "dataset" ("dset" is commonly used as a variable name) is a list('data'=>data.frame(), ...) and can contain additional bookkeeping fields, eg.., dset$didsummarize.

###############################################################
dataset_applyfunction_sample=function(dset){
  print('This is dummy sample for dataset_applyfunction. No changes are done to the dataset. Another example of a function you can apply is "dataset_keeponlysurfaceproteins"')
  return(dset)
}

dataset_keeponlysurfaceproteins=function(data){
  #attr does not work well when we package this in a library, b/c this function becomes locked and its attributes cannot be changed.
  #surfaceprots=attr(dataset_keeponlysurfaceproteins, "surfaceprots")
  surfaceprots=.GlobalEnv$zoz.dataset_keeponlysurfaceproteins.surfaceprots;
  if(is.null(surfaceprots)) {
#    source('biodb.r') 
    #TODO: Check how mnay proteins each db has 
    #venn diagram of common proteins 
    cirfessgenes=cirfessdb_getspc()
    compar=compartmentsdb_getcellsurfacegenes()
    uniprot=uniprotdb_getecmtotallength()
    surfaceprots=unique(c(cirfessgenes$genesymbol, compar$genesymbol, uniprot$genesymbol))
    #attr(dataset_keeponlysurfaceproteins, "surfaceprots")<<-surfaceprots;
    .GlobalEnv$zoz.dataset_keeponlysurfaceproteins.surfaceprots=surfaceprots;
  }
  
  surface_data=data[which(rownames(data) %in% surfaceprots),]
  return(surface_data)
}

###############################################################
#after loading, you can use dset$data.
# We use this function for multiple purposes: to load an experimental dataset (doenrich should be OFF), and to process the project data (dosummarize should be OFF).
dataset_load=function(dset, ...){
  if(is.character(dset)){
    if(dset=="__ALLHUMANGENES__"){
#      source('biodb.r');
      genes=uniprotdb_allgenesymbols(9606);
      dset=list(data=data.frame(matrix(,nrow=length(genes),ncol=0),row.names=genes), dosummarize=F)
    }
    else{
      dset=list(datafile=dset);
    }
  }
#  source('util.r')
  dset=list_merge(dset,list(...));
  #checks if dataset is already loaded into dset$data 
  if('data' %in% names(dset)){
    #do nothing.
  }
  else if('datafile' %in% names(dset)){    
    datafile=dset$datafile;
    cat(paste0('Reading datafile [',datafile,']...\n'));
#  	source('util.r')
    installpackageifmissing('openxlsx');
    dset$data=openxlsx::read.xlsx(datafile,rowNames=T)
  }
  else{
    print(dset)
    stop('Cannot load dataset. One of [datafile] or [data] fields needs to be specified. The dataset entry is printed above.');
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
#		source('data_enrich.r')
		cat('Enriching with additional information. This may take a while...\n')
		dset$data=data_enrich(dset$data,oenrich);
    dset$didenrich=T;
	}

  if((is.null(dset$dohandlenan)&&!is.null(dset$handlenan) || !is.null(dset$dohandlenan)&&dset$dohandlenan) && (is.null(dset$didhandlenan)||!dset$didhandlenan)){
#    source('data_handlenan.r')
    dset$data=data_handlenan(dset$data,modifyList(list(cols='__NUMERIC__'),dset));
    dset$didhandlenan=T
  }
  if((is.null(dset$dorescale)&&!is.null(dset$rescale) || !is.null(dset$dorescale)&&dset$dorescale) && (is.null(dset$didrescale)||!dset$didrescale)){
#    source('data_rescale.r')
    dset$data=data_rescale(dset$data,modifyList(list(cols='__NUMERIC__'),dset));
    dset$didrescale=T
  }
  if(!is.null(dset$dosummarize) && dset$dosummarize && (is.null(dset$didsummarize)||!dset$didsummarize)){
#    source('data_summarize.r')
    dset$data=data_summarize(dset$data,modifyList(list(cols='__NUMERIC__'),dset));
    dset$didsummarize=T
  }
  return(dset);
}
