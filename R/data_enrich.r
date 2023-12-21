#% Copyright (C) 2022 by Rawan Shraim, Ahmet Sacan

##Function to call all enrichment functions 
#source('util.r');
#source('enrich.r');

data_enrich=function(d, o=list()){
  if(is.character(o)) o=list(enrichtypes=o);
  alltypes=unlist(strsplit('gtex,evodevo_pediatric,healthyprot,compartments_sp,cirfess_spc,uniprot,opentarget,surfaceopentarget,theratarsurface',','));
  #alltypes=unlist(strsplit('gtex,evodevo_pediatric,healthyprot,compartments_sp,cirfess_spc,uniprot',','));
  #only include depmap in the default alltypes if depmapids is provided
  if(!is.null(o$depmapids)) alltypes=c(alltypes,'depmap');
  #only include go in the default alltypes if goids is provided
  if(!is.null(o$goids)) alltypes=c(alltypes,'go');

  o = list_merge(list(
    enrichtypes=alltypes
    #some enrich_ function take in additional options, which can/must be specified.
    #depmap: depmapids
    #go: goids, go_collate, go_colname
    ,dbg=F #print debugging messages
  ), o);

#  source('util.r')
  types=ensurecsvlist(o$enrichtypes,alltypes)
  if(length(types)==0) return(d); #no summarization done.

  for(type in types){
    if(o[['dbg']]) print(sprintf('enriching [%s]... \n',type));
    #handle the enrich types that require additional arguments.
    if(type=='depmap'){
      if(is.null(o$depmapids)){
        stop('Depmap enrichment requires depmapids (numeric ids or a search text) to be provided.');
      }
      d=enrich_depmap(d,o$depmapids,o$depmap_collate,o$depmap_mergefunc);
    }
    else if(type=='go'){
      if(is.null(o$goids)){
        stop('GO enrichment requires goids (numeric ids or a search text) to be provided.');
      }
      d=enrich_go(d,o$goids,o$go_collate,o$go_colname)
    }
    else{
      #construct the function name from the type. e.g, type='gtext' --> func='enrich_gtex'
      func = paste0('enrich_',type);
      if(!exists(func)){
        stop(sprintf('Enrichment function [%s] does not exist.',func))
      }
      func=get(func);
      d=func(d);
    }
  }
  return(d)
  
}
