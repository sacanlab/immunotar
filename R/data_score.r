#% Copyright (C) 2022 by Rawan Shraim, Ahmet Sacan

#source('getcoloption.r')
#source('util.r')

data_score=function(d, o=list(),...){
#  source('util.r')
  o = list_merge(list(
    weight=NULL #default weight. When NULL, we check for a value in getcoloption.r::suggestedoptions().
    ,curve=NULL #default curvature.
    ,coloptions=NULL #% You can specify weight/handlenan/rescale options for each column separately.
    ,colrescale=NULL #% This is another place scale option for each column can be specified. This can be a struct or a list.
    ,cols=NULL #%if non-empty, we only apply to these columns. Can bee a list of column names or a logical/numerical index for columns. or '__NUMERIC__' to only use numeric columns.
    ,getfull=T #if True, we return additional information. If False, we only return the score vector.
    ,outfile=NULL #specify an Excel file to save the data & score to.
    ,openoutputfile=T #if an output file is written, should we open that file when done?
    ,curveddata=NULL #if given, we'll use it instead of applying data_rescale_curve.
    ,weights=NULL #if given, we'll use it, otherwise we collect the weight values using getcoloption()
    ,curves=NULL #if given, we'll use it, otherwise we collect the curve values using getcoloption()
  ), o);
  o=list_merge(o,list(...));
  
  Icols=data_Icols(d,o$cols);
  numIcols=length(Icols);

  
  if(!is.null(o$curves)||is.null(o$curveddata)){
    o$curves=getcoloption_curves(colnames(d)[Icols],o);    
#    source('data_rescale.r')
    oldcolnames=colnames(d);
    d=data_rescale_curve(d, o);
    colnames(d)=oldcolnames; #data_rescale_curve will add "_curve" suffix, but we need to preserve the column names.
    m=d[,Icols];
  } else{  m=o$curveddata; }

  weights = getcoloption_weights(colnames(m),o);
  W = as.numeric(weights);

  Inan=is.na(m);
  if(!any(Inan)){ #%for efficiency, consider no-nan case separately.
    Wtotal = sum(abs(W));
    mW = sweep(m, MARGIN=2, W, `*`) #https://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector
    score = rowSums(mW) / Wtotal;
    #TODO: (Discuss) Rawan changed the score vector because it was not producing the score. 
    #TODO: Ahmet says: I don't know why the following line is needed.
    #mW$score=score
  }
  else{
    W_ = matrix(W, nrow=nrow(d), ncol=length(W), byrow=TRUE);   #%expand W_, so we can mark-off nan positions to calculate row-specific Wtotal
    W_[Inan]=0;
    Wtotal_=rowSums(abs(W_));
    m[Inan]=0;
    mW = m * W_;
    score = rowSums(mW)
    score = score / Wtotal_;
  }

  if(o$getfull||!is.null(o$outfile)){
    if('score' %in% rownames(d)) warning('data already has a column named [score]. The score column will be overridden here. You probably should have removed that column becore calling data_score(), otherwise the old score column may be used for calculation of the new score column.');
    dwithscore = d;
    dwithscore$score = score;
    installpackageifmissing('data.table')
    #data.table::setcolorder(dwithscore,c('score'))
    dwithscore=dwithscore[order(dwithscore$score, decreasing = T),]
  }
  if(!is.null(o$outfile)){
    xls_write(o$outfile,dwithscore,rowNames=T,open=o$openoutputfile);
  }

  #NOTE: (discussed).  Return the score variable without sorting, so it stays consistent with the original data, in case user wants to merge with it. 
  if(!o$getfull){ return(score); }

  score=as.data.frame(score);
  rownames(score)=rownames(d);
  return(list(curveddata=m, weighteddata=mW, weights=weights, curves=o$curves, score=score, datawithscore=dwithscore))

}

#Prepares the data using data_handlenan() and data_rescale();
# then calls data_score().
data_prepareandwithscore=function(d,o=list()){
#  source('data_handlenan.r')
	d=data_handlenan(d,o)
#  source('data_rescale.r')
	d=data_rescale(d,o)
  return(data_score(d,o));
}
