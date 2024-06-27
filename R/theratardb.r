source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2022 by Ahmet Sacan and Rawan Shraim
source_disabled__('biodb.r')

###############################################################
# This file was adapted from depmapdb.r 

###############################################################
#' theratardb_searchdisease is used to query what diseases exist in the database
#' The input to this function is a disease key-word
#' The output is the list of diseases found that match the key-word exactly or have the key-word within the phrase. 
#' 
theratardb_searchdisease = function(queries,exact=FALSE,where=''){
  #if exact=NULL, we use exact=FALSE if a query contains '%'.
  return(biodb_textsearch('theratardb','gene_disease',queries,c('disease'),exact,where,getfields='DISTINCT(disease) AS disease',combinequerieswith='OR'));
}

#' theratardb_moas is used to query the various of mode of actions (MOAs) of drugs in the database 
#' No input is required for this function. 
#' The output is the list of distinct MOAs in the database. 
#' 
theratardb_moas = function(){
  ss=biodb_query('theratardb','SELECT DISTINCT(moas) FROM gene_disease');
  #print(ss);
  #source('util.r')
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

theratardb_disease2genesymbols = function(diseases,asframe=T,moa=NULL,status=NULL,minstatusscore=NULL,maxstatusscore=NULL,exact=NULL,where='',immunomult=1,immunoadd=0){
  if(is.null(exact)||!exact){
    diseasesin=diseases; #copy for warning purposes.
    r=theratardb_searchdisease(diseases,exact=exact);
    diseases=theratardb_searchdisease(diseases,exact=exact)[['disease']];
    if(!length(diseases)){ warnf('theratard: Empty disease list. Did not find any matching diseases for: %s',diseasesin); return(c()); }
  }
  if(is.character(asframe)){  getfields=asframe; if(immunomult!=1||immunoadd!=0){ getfields=c(getfields,'moas'); };  asframe=T;  }
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
  if(!is.null(status)){
    sql_params=biodb_makeplaceholders(status,params); params=sql_params[[2]];
    sql=paste0(sql,' AND higheststatus IN (',sql_params[[1]],')');
  }
  if(!is.null(minstatusscore)){
    sql=paste0(sql,' AND higheststatus_ >=',minstatusscore);
  }
  if(!is.null(maxstatusscore)){
    sql=paste0(sql,' AND higheststatus_ <=',maxstatusscore);
  }
  rs=biodb_query('theratardb',sql,params);

  if(asframe){
    if(immunomult!=1||immunoadd!=0){
        Iimmuno = grepl('(CAR-T-Cell-Therapy|adc_antigen)',rs$moas);
        if(immunomult!=1){rs[Iimmuno,'higheststatus_']=rs[Iimmuno,'higheststatus_']*immunomult; }
        if(immunoadd!=0){rs[Iimmuno,'higheststatus_']=rs[Iimmuno,'higheststatus_'] + sign(rs[Iimmuno,'higheststatus_'])*immunoadd; }
      }  
    return(rs);    
  }
  if(!length(rs)){ return(c()); }
  genes=paste0(rs$genesymbol,collapse=',')
  if(genes==''){ return(c()); }
  return(unique(unlist(strsplit(genes,","))));
}



#stk__=dbg_nicestack(1); message(sprintf('theratardb.r sourced from: %s',stk__));
