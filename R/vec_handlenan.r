#% Copyright (C) 2022 by Rawan Shraim

##Looks at every column and decides what to do with the NAN values based on user input.
##Can set to minimum in the column, maximum in the columns, zero, or any other numeric value decided by the user. 
#% type can be one of none|0|0ormin|min|max|any-number (e.g, -inf)
#0ormin: uses 0 if all data is positive; uses min otherwise.
#name variable is given for debugging purposes only, so warnings can be printed with that name.

vec_handlenan=function(v, type='none', name=''){
  if(type == 'none' || is.na(type)){
    return(v)
  }
  
  I=which(is.na(v))
  if(any(I)){
    if(is.numeric(type)){
      if(type == 0 && any(v[-I]<0)){
        warning(paste0('The vector ',name,' contains negative values and using 0 for NaNs is not appropriate\n'))
      }
      v[I]=type
    }
    else if(type == 'min'){
      v[I]=min(v, na.rm = T)
    }
    else if(type == '0ormin'){
      minv=min(v, na.rm = T);
      if(minv<0){ v[I]=minv; }
      else{ v[I]=0; }
    }
    else if(type == 'max'){
      v[I]=max(v, na.rm = T)
    }
    else if(type == 'mean'){
      v[I]=mean(v, na.rm = T)
    }
    else{
      stop(paste0('Unknown handlenan type [',type,']'))
    }
  }
  return(v)
}


#stk__=dbg_nicestack(1); message(sprintf('vec_handlenan.r sourced from: %s',stk__));
