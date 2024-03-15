source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2022 by Rawan Shraim, Ahmet Sacan

data_score=function(d,...){
  if(!exists('opt_set')){ source_disabled__('util.r'); } 
  if(!exists('data_rescale')){ source_disabled__('data_rescale.r'); }
  if(!exists('getcoloption')){ source_disabled__('getcoloption.r'); }

  o = opt_set(
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
    ,getcurves=FALSE #when curves are NULL, we don't crate a list for them.  Set this to TRUE to ensure curves list is available in the output
  ,...);
  
  Icols=data_Icols(d,o$cols);
  numIcols=length(Icols);

  if(!is.null(o$curves)||is.null(o$curveddata) || (o$getcurves&&isempty(o$curves)) ){
    #performance shortcut if curving is not needed:
    if(!isempty(o$curves)&&all(o$curves==0)){
      m=d[,Icols];
    }
    else{
      o$curves=getcoloption_curves(colnames(d)[Icols],o);
      oldcolnames=colnames(d);
      d=data_rescale_curve(d, o);
      colnames(d)=oldcolnames; #data_rescale_curve will add "_curve" suffix, but we need to preserve the column names.
      m=d[,Icols];
    }
  } else{  m=o$curveddata[,Icols]; }

  weights = getcoloption_weights(colnames(m),o);
  
  #print(weights$expr_mean_percentile)
  
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
    if('score' %in% colnames(d)){ warnf('data already has a column named [score]. The score column will be overridden here. You probably should have removed that column becore calling data_score(), otherwise the old score column may be used for calculation of the new score column.'); }
    dwithscore = d;
    if(length(score)!=nrow(d)){
      2+2;
    }
    dwithscore$score = score;
    #make score the first column:
    dwithscore=dwithscore[,c(which(colnames(dwithscore) %in% c('score')), which(!colnames(dwithscore)%in% c('score') ))];
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
  if(!exists('data_handlenan')){ source_disabled__('data_handlenan.r'); } 
  if(!exists('data_rescale')){ source_disabled__('data_rescale.r'); }

	d=data_handlenan(d,o)
	d=data_rescale(d,o)
  return(data_score(d,o));
}


#stk__=dbg_nicestack(1); message(sprintf('data_score.r sourced from: %s',stk__));
