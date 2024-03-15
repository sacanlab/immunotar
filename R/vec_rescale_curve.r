#% Copyright (C) 2022 by Ahmet Sacan, Rawan Shraim

#change the values in x using a monotonic curve (monotonicity only guaranteed within [xmin...xmax] range).
#curvature varies betwen -inf .. . +inf.
#xmin/xmax will be calculated from v if not provided.
#ymin/ymax will be set to xmin/xmax if not provided.

vec_rescale_curve=function(x, curvature, xmin=NULL,xmax=NULL, ymin=NULL,ymax=NULL){
#NOTE: (Rawan) implement this function.
  x2=x[!is.na(x)]
  if(is.null(xmin)) xmin=min(x2);
  if(is.null(xmax)) xmax=max(x2);
  if(is.null(ymin)) ymin=xmin;
  if(is.null(ymax)) ymax=xmax;
  #NOTE: (Rawan ) - only apply this to the none NA values, done 
  if(any(x2<xmin) || any(x2>xmax)){
    warning('This function is designed to only work for a specific range and may be numerically unstable outside that range. Some of the input numbers are outside the range [xmin..xmax]. Either adjust xmin,xmax; or ensure the numbers are within the range.\n');
    }

#See similar formulas on: https://math.stackexchange.com/questions/65641/i-need-to-define-a-family-one-parameter-of-monotonic-curves
# also see vec_rescale_curve_plots.pptx for the desired results. 
#https://math.stackexchange.com/questions/3329323/transform-a-straight-line-into-a-curve


# scale x to range 0..1:
  x = (x-xmin)/(xmax-xmin);

#calculate y (currently in range 0..1):
  y = x^exp(curvature);

#scale y to range ymin..ymax
  y = y*(ymax-ymin) + ymin;
  
  return(y);

}


#This solution is not used, but kept for reference.
#This solution is based on https://math.stackexchange.com/questions/65641/i-need-to-define-a-family-one-parameter-of-monotonic-curves
# We use x^n + y^n = 1, but define n=exp(curvature) instead, so it can be varied -inf...+inf.
vec_rescale_curve_circleformula=function(x, curvature, xmin=NULL,xmax=NULL, ymin=NULL,ymax=NULL){
if(is.null(xmin)) xmin=0;
if(is.null(xmax)) xmax=1;
if(is.null(ymin)) ymin=xmin;
if(is.null(ymax)) ymax=xmax;
if(any(x<xmin)||any(x>xmax)){
	warning('This function is designed to only work for a specific range and may be numerically unstable outside that range. Some of the input numbers are outside the range [xmin..xmax]. Either adjust xmin,xmax; or ensure the numbers are within the range.\n');
}

# scale x to range 0..1:
x = (x-xmin)/(xmax-xmin);

#calculate y (currently in range 0..1):
exp_curve = exp(curvature);
exp_curve_inv = 1/exp_curve;
y = (1-(1-x)^exp_curve)^exp_curve_inv;


#scale y to range ymin..ymax
y = y*(ymax-ymin) + ymin;
return(y);
}


#stk__=dbg_nicestack(1); message(sprintf('vec_rescale_curve.r sourced from: %s',stk__));
