source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2022 by Rawan Shraim and Ahmet Sacan 

##Applying the vec_handlenan function to a dataframe 
source_disabled__('util.r')


data_handlenan=function(d, o=list()){
  source_disabled__('getcoloption.r')
  source_disabled__('vec_handlenan.r')
  if(!is.list(o)){ o=list(handlenan=o); }
  o = opt_set(
    handlenan=NULL #% Default scaling type applied to all columns. Can be one of none|percentile|rangemap0100. Can be a csv/list of multiple scaling types if you want to generate multiple columns for each column.
    ,coloptions=NULL #% You can specify weight/handlenan/scale options for each column separately.
    ,colscale=NULL #% This is another place scale option for each column can be specified. This can be a struct or a list.
    ,cols=NULL #%if non-empty, we only apply to these columns. Can bee a list of column names or a logical/numerical index for columns. or '__NUMERIC__' to only use numeric columns.
    ,dbg=F #print debugging messages
  , o);
  
  Icols=data_Icols(d,o$cols);
  
  Inan=is.na(d[,Icols]);
  if(!any(Inan)){ return(d); }
  
#TODO: (discuss) add 'knnimpute' as a handlenan option. Need to do that at the data level, not at the vec level.

  for(ci in 1:length(Icols)){
    if(!any(Inan[,ci])){ next; }
    c=Icols[ci];
    type = getcoloption(o, 'handlenan', colnames(d)[c] );
    if(o[['dbg']]) print(sprintf('colname=[%s], handlenan-type=[%s]\n',colnames(d)[c],type));
    if(!is.null(type)&&type!="none"){
      d[,c]=vec_handlenan(d[,c], type = type, name=colnames(d)[c]);
    }
  }
  return(d);
}


#stk__=dbg_nicestack(1); message(sprintf('data_handlenan.r sourced from: %s',stk__));
