#% Copyright (C) 2022 by Rawan Shraim and Ahmet Sacan 

##Applying the vec_rescale function to a data frame 
data_rescale=function(d, o=list()){
#  source('getcoloption.r')
#  source('vec_rescale.r')
#  source('util.r')
    
  
  if(!is.list(o)){ o=list(rescale=o); }
  o = list_merge(list(
    rescale='percentile' #% Default scaling type applied to all columns. Can utilbe one of none|percentile|range. Can be a csv/list of multiple scaling types if you want to generate multiple columns for each column.
    ,rescaleparam=NULL # added this parameter #parameter for rescale options such as 'curve' 
    ,coloptions=NULL #% You can specify weight/handlenan/rescale options for each column separately.
    ,colrescale=NULL #% This is another place scale option for each column can be specified. This can be a struct or a list.
    ,cols=NULL #%if non-empty, we only apply to these columns. Can bee a list of column names or a logical/numerical index for columns. or '__NUMERIC__' to only use numeric columns.
    ,dbg=F #print debugging messages
), o);
  
#  source('util.r')
  Icols=data_Icols(d,o$cols);
  
  for(c in Icols){
    type = getcoloption(o, 'rescale', colnames(d)[c] );
    param = getcoloption(o, 'rescaleparam', colnames(d)[c], warn=F ); #some rescale types (e.g. "curve") require an additinal parameter.
    if(o[['dbg']]) print(paste0('colname=[',colnames(d)[c],'], rescale-type=[',type,'], rescale-param=[',param,']\n'));
    if(type!="none"){
      d[,c]=vec_rescale(d[,c], type = type, param=param);
      colnames(d)[c]=paste0(colnames(d)[c],'_',type)
    }
  }
  return(d); 
}

#makes use of o$curve parameter, but calls data_rescale() to do the rescaling.
data_rescale_curve=function(d,o=list()){
#  source('getcoloption.r')
#  source('util.r')
  Icols=data_Icols(d,o$cols);
  
  #o=modifyList(list(curve=0), o); #by default, don't curve.
  o=list_merge(list(
    curves=NULL #if given, we use it, otherwise we collect curve values using getcoloption().
  ),o)
  if(is.null(o$curves)){ curves=getcoloption_curves(colnames(d)[Icols],o); }
  else{ curves=o$curves; }

  #convert curve values to colrescale='curve' & colrescaleparam=curvevalue, so we can rely on the data_rescale function.
  colrescale=list();
  colrescaleparam=list();
  for(c in Icols){
    colname=colnames(d)[c];
    curve = curves[[colname]];
    if(is.null(curve) || curve==0){ colrescale[[colname]] = 'none'; }
    else{ colrescale[[colname]] = 'curve'; colrescaleparam[[colname]] = curve; }
  }
  #TODO: (Rawan) discuss - i think the colrescale needs to be called rescale and the parameter needs to be called rescaleparam and update
  # data_rescale to have an if param to use vec_rescale_curve 
  
  return(data_rescale(d, list(colrescale=colrescale, colrescaleparam=colrescaleparam)));
  
}
