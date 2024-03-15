source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2022 by Ahmet Sacan, Rawan Shraim

# takes in a vector or numbers and returns percentile values. Any NaN values will remain as NaN in the output.
# type: percentile|rangemap0100|tpm|zscore|log2
# Some rescale types (e.g. "curve") require an additinal parameter.

vec_rescale=function(v, type='percentile', param=NULL){
  if(!exists('opt_set')){ source_disabled__('util.r'); } 
  type=csv(type);
  if(length(type)>1){  for(type_ in type){ v=vec_rescale(v,type_);  }; return(v);  }
  
  if(type=='percentile'){ 
    Inotnan = !is.na(v);
    numnotnan = sum(Inotnan);
    if( numnotnan == 0 ){ return(v); } #if they are all nan's, return all nan's.
    else if(numnotnan == 1){ ranks=v; ranks[Inotnan]=50; return(ranks); } #if there is only one number, return its rank as 50%.
    
    ranks = (rank(v,na.last='keep')-1) / (numnotnan-1)*100;
    minrank = min(ranks, na.rm=T)
    maxrank = max(ranks, na.rm=T)
    if(minrank == maxrank){ #if there is only one unique rank, return its rank as 50%.
      ranks[Inotnan] = 50;
      return(ranks);
    }
    #hard-set minimum rank to 0 and maximum rank to 100.
    if( minrank != 0){ ranks[ ranks == minrank ] = 0; }
    if( maxrank != 100){ ranks[ ranks == maxrank ] = 100; }
    
    return( ranks ) 
  }
  else if(type=='rangemap0100'){
    minv = min(v, na.rm=T)
    maxv = max(v, na.rm=T)
    return((v - minv)/(maxv-minv) * 100)
  }
  else if(type=='none'){
    return(v);
  }
  else if(type=='tpm'||type==''){
    if(hasnegative(v)){ warning('tpm transformation is requested, but there are some negative values. tpm should only be applied to positive data columns.\n'); }
    return( v / sum(v,na.rm=T) * 10^6)
  }
  else if(type=='zscore'){
    return(scale(v));
  }
  else if(type=='log2'){
    if(hasnegative(v)){ warning('log2 transformation is requested, but there are some negative values. log2 should only be applied to positive data columns.\n'); }
    return(log2(v+1));
  }
  else if(type=='curve'){
    if(is.null(param)){ stop('For rescale type [curve], an additional rescale parameter must be specified.'); }
    if(!exists('vec_rescale_curve')){ source_disabled__('vec_rescale_curve.r'); } 
    return(vec_rescale_curve(v, curvature=param));
  }
  #If you add additional rescale types, also update in vec_rescaletypes().
  else{
    stop(paste0('Unknown rescale type [',type,']. Must be one of percentile|rangemap0100|none.'));
  }
}
#returns the rescale types we have implemented here (used in project_shortenfeaturenames)
vec_rescaletypes=function(){
  return(csv('percentile,rangemap0100,tpm,zscore,log2,curve'));
}





#stk__=dbg_nicestack(1); message(sprintf('vec_rescale.r sourced from: %s',stk__));
