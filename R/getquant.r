#% Copyright (C) 2022 by Rawan Shraim 
#TODO: if this function/file is no longer needed, place it in Archive/


getquant=function(data, threshold){
  
  if(is.character(threshold)){ 
    if(grepl("%",threshold)){
      thresh1=as.numeric(gsub("%", "", threshold))/100
    }
    data = data_rescale(data,'percentile');
  }
  
  
  #else if(is.numeric(threshold)){
  #  thresh1=threshold
  #}
  #else{thresh1=0.5}
  #quant=quantile(data, thresh1, na.rm=T)
  
  return(quant)
  
}
