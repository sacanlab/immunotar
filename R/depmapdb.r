source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2022 by Ahmet Sacan

###############################################################
# Search depmapdb sampleinfo table. only search within the searchfields columns. query is searched as a substring (using sql LIKE clause).

options(timeout=100000)

#' depmapdb_searchsamples function allows users to search the DepMap database to see what cancer phenotypes are included and the depmapid's of those phenotypes. 
#' The input to this function includes the disease names of interest
#' The output is a data-frame of meta data from DepMap that includes the depmapid's, the primary disease, the cancer subtype, and the cell line that was tested in DepMap
#' 
#' @export
depmapdb_searchsamples = function(queries,searchinfields=c('samplecollectionsite','primarydisease','subtype','lineage','lineagesubtype','cellosaurusncitdisease'),exact=FALSE,where='',getfields=NULL){
  source_disabled__('biodb.r')
  t=biodb_textsearch('depmapdb','sampleinfo',queries,searchinfields,exact,where,getfields=getfields);

  #library(plyr); library(dplyr) #we only need dplyr::select(), but we are required to load plyr, then dplyr here, otherwise loading dplyr here and plyr elsewhere causes problems.
  #t=t %>% select('depmapid','primarydisease','subtype', everything())
  #dplyr has issues/conflicts. let's use data.table instead.
  source_disabled__('util.r')
  installpackageifmissing('data.table')
  if(is.null(getfields)){  t=data.table::setcolorder(t,c('depmapid','primarydisease','subtype')); }
  return(t)
}


###############################################################
#' depmapdb_getgenedependency function allows the user to query a certain geneset in specific diseases to extract the probability of the gene dependency in the specific cancer phenotype. 
#' The input to this function is the cancer phenotype and the genesymbols to query.
#' The output of this function is a dataframe with the depmapid, the genesymbol and the probability. 
#' 
#' 
# I recommend you manually identify the depmapids you want to use and provide their numeric ids as input.
# If depmapids is text, we use depmapdb_searchsamples() to retrieve the ids.
# This returns one row per sample (depmapid), so each gene may have multiple rows in the returned result. Multiple rows for a gene are combined in enrich_depmap() to get a single probability value for each gene.
#' @export
depmapdb_getgenedependency = function(depmapids=c(),genesymbols=c(),o=list()){
  o = modifyList(list(
    project='crispr' #can be one of crispr|achilles
    ), o);
  if(is.character(depmapids)){
    depmapids=gsub("'", '', depmapids)
    t = depmapdb_searchsamples(depmapids,getfields=c('depmapid'));
    depmapids=unlist(t$depmapid);
  }
  
  wheres=c();
  if(length(depmapids)>0){
    wheres = c(wheres, paste0('depmapid IN (',paste0(as.character(depmapids), collapse=", "), ')'));
  }
  if(length(genesymbols)>0){
    wheres = c(wheres, paste0('genesymbol IN (',paste0(sapply(genesymbols, function(x) toString(shQuote(x))), collapse=", "), ')'));
  }
  if(!length(wheres)){ wheres=c('1'); }
  
  sql = paste0('SELECT * FROM ', o$project, 'genedependency WHERE ',paste0(wheres, collapse=' AND '));
  source_disabled__('biodb.r')
  t=biodb_query('depmapdb',sql);
  return(t);
}
