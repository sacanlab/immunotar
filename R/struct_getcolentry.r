#perform exact or prefix-matched indexing. If prefix-matched, the prefix is required to have '_'.
#returns list(val=>...,found=>...,var=>...)
#' @export
struct_getcolentry=function(o,colname){
#We may rename columns
#% (e.g, add '_percentile' suffix) and would still want the user options
#% apply to these new column names. 
#% Copyright (C) 2022 by Ahmet Sacan

#  source('util.r')
  var = fieldname_match(names(o), colname);
  if(is.null(var)){ return(list(val=NULL,found=F)); }
  return(list(val=o[[var]],found=T,var=var));  
}
