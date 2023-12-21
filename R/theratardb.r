#% Copyright (C) 2022 by Ahmet Sacan and Rawan Shraim


###############################################################
# This file was adapted from depmapdb.r 

###############################################################
#' theratardb_searchdisease is used to query what diseases exist in the database
#' The input to this function is a disease key-word
#' The output is the list of diseases found that match the key-word exactly or have the key-word within the phrase. 
#' 
#' @export
theratardb_searchdisease = function(queries,exact=FALSE,where=''){
  return(biodb_textsearch('theratardb','gene_disease',queries,c('disease'),exact,where,getfields='DISTINCT(disease) AS disease',combinequerieswith='OR'));
}

#' theratardb_moas is used to query the various of mode of actions (MOAs) of drugs in the database 
#' No input is required for this function. 
#' The output is the list of distinct MOAs in the database. 
#' 
#' @export
theratardb_moas = function(){
  ss=biodb_query('theratardb','SELECT DISTINCT(moas) FROM gene_disease');
  print(ss);
#  source('util.r')
  ss=sort(unique(arr_csv(str_csv(ss[['moas']]))));
  return(ss);

}

###############################################################
#' theratardb_disease2genesymbols is used to query targets associated to specified cancer phenotypes present in the database. 
#' The input to the function is the cancer phenotype. The user can also specify the mode of action of drug of interest using the moa parameter and the status of drug development using the status parameter. 
#' The output is a data-frame from the database with all available metadata assoicated to genes linked to the disease. If user only wants to retrieve gene symbols, the asframe parameter can be set to FALSE. 
#' 
#Get gene symbols for a list of diseases. Returns a data frame or genelist; depending on the value of asframe.
#asframe can be True or a list of fieldnames. When asframe is false, we only retrieve the genesymbol.


#' @export
theratardb_disease2genesymbols = function(diseases,asframe=T,moa=NULL,status=NULL,minstatusscore=NULL,exact=NULL,where=''){
  if(is.null(exact)||!exact){
    r=theratardb_searchdisease(diseases,exact=exact);
    diseases=theratardb_searchdisease(diseases,exact=exact)[['disease']];
    if(!length(diseases)){ print('Empty disease list (Did not find any matching diseases'); return(c()); }
  }
  if(is.character(asframe)){  getfields=asframe;   asframe=T;  }
  else if(asframe){    getfields='*';     }
  else{ getfields='genesymbol'; }
  
  params=list();
  sql_params=biodb_makeplaceholders(diseases,params); params=sql_params[[2]];
  sql=paste0('SELECT ',str_csv(getfields),' FROM "gene_disease" WHERE disease IN (',sql_params[[1]],')');
  
  if(where!=''){ sql=paste0(sql,' AND (',where,')'); }
  if(!is.null(moa)){
    sql_params=biodb_makecsvfieldlike('moas',moa,params); params=sql_params[[2]];
    sql=paste0(sql,' AND (',sql_params[[1]],')');
  }
  if(!missing(status)){
    sql_params=biodb_makeplaceholders(status,params); params=sql_params[[2]];
    sql=paste0(sql,' AND higheststatus IN (',sql_params[[1]],')');
  }
  if(!missing(minstatusscore)){
    sql=paste0(sql,' AND higheststatus_ >=',minstatusscore);
  }
  #print(sql);
  #print(params)
  rs=biodb_query('theratardb',sql,params);
  if(asframe){ return(rs); }
  if(!length(rs)){ return(c()); }
  genes=paste0(rs$genesymbol,collapse=',')
  if(genes==''){ return(c()); }
  return(unique(unlist(strsplit(genes,","))));
}
