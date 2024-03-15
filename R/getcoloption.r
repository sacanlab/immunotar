#% Copyright (C) 2022 by Rawan Shraim and Ahmet Sacan 

#Fields that are "the higher the better" should have a positive weight.
#Fields that are "the lower the better" should have a negative weight.
#' @export
suggestedoptions=function(){
#Done - double-check the weights, espcially for enrich'ments that result in multiple columns (e.g., gtext). Make sure all such columns can be used with the same-sign'ed weight.
#Done - double-checked the handlenan.
  return(list(
    curve = 0
    ,coloptions=list(
      gtex=list(
        weight=-1 #TODO: Possible - Change so that it divides by 8 automatically in the code in case we change number of columns for GTEx in the future 
        ,handlenan=0 #NOTE: We probably need to change this to zero. e.g., PIGY is not available in Gtex, but that is due to no-detection (it is not due to PIGY not being considered in Gtex).
      )
      ,evodevo_pediatric=list(
        weight=-1
        ,handlenan=0 #NOTE: Similar to GTEx 
      )
      ,healthyprot=list(
        weight=-1
        ,handlenan='min' #we assume a protein to have expression=0 if it is not in healthyprot database. #NOTE: Checked they are in log scale so negative values exist. 
      )
      ,depmap=list(
        weight=1
        ,handlenan='min' #Depmap is not an exhaustive/genome-scale/unbiased assay, so genes that are not assayed should be left as 'nan'.
      )
      ,compartments_sp=list(
        weight=1
        ,handlenan=0 #if it's missing from the database, we'll assume it is not a surface protein; i.e., confidence=zero.
      )
      ,cirfess_spc=list(
        weight=1
        ,handlenan=0 #spc scores range between 1-4. if it is not in the database, we'll assume it is not a surface protein; ie., spc=zero.
      )
      ,go=list( 
        weight=NULL #go terms are custom for each project and we cannot assume a particular weight. You must set the weight yourself.
        ,handlenan=0 #if it is not in the database, we can assume the gene is not associated with that go term.
      )
      ,uniprot_ecmtotallength=list( weight=1, handlenan=0 )
      ,opentargetsurface=list( weight=1, handlenan=0 )
      ,expr=list(weight="+1 each")
      ,expr_numlowexpressed=list(weight=-1)
    )
  ));
}


###############################################################
#% Utility function to obtain column-specific option value or if it is not
#% available, use default.
#% List of places we check for the option (latter places override previous
#% places). Examples below given for optionname='rescale', and
#% colname='healthyrnaseq'
#% * o.rescale
#% * o.healthyrnaseq.rescale
#% * o.colrescale.healthyrnaseq
#%
#% When using healthyrnaseq as a key, we do flexible matching, e.g. if
#% o.healthyrnaseq is not present, but there is an entry
#% o.healthrnaseq_nonan, we use that.
#%
#% o.colrescale can be a struct, a vector, or a list (cell array). If it is a
#% vector or a list, we use colind to retrieve the entry that corresponds to
#% colname. Using a vector/list is not recommended, because you then need to
#% maintain colind as you select or add columns to the data.
#gettingdefault__=T is used internally/recursively to get colname='DEFAULT'.
#gettingsuggested__=T is used internally/recursively to get recommended values.

getcoloption=function(o, optionname, colname, gettingdefault__=F, gettingsuggested__=F, warn=T){
  found=F #whether we find an option specifically/explicitly for this column.
  ocolname=NULL; #the actual index name we find in o (may be exactly colname, or a prefix_ of it.)
  #% if o is a dataframe, we assume it lists optionname's on rows. one of the columns may be labeled as 'DEFAULT' (must be capital).
  if(is.data.frame(o)){
    if(optionname %in% rownames(o) && colname %in% colnames(o) && !is.na(o[optionname, colname])){
      out=o[optionname, colname]
      found=T
      ocolname=colname;
    }
  }
  else if(is.list(o)){
    default=o[[optionname]] # %this is the default for all columns.
    out=default;
        
    #%% Check if option is set in o.coloptions.colname.optionname
    if('coloptions' %in% names(o) && !is.null(o$coloptions)){
      #struct_getcolentry essentially gets o$coloptions$colname, while allowing for a prefix search.
      # (e.g., if we need colname='gtex_percentile' but that is not listed, we also search for colname='gtex')
      val_found=struct_getcolentry(o$coloptions,colname); val=val_found$val;
      if(val_found$found && optionname %in% names(val)){
        out=val[[optionname]];
        found=T
        ocolname=val_found$var;
      }
    }
    
    #%% Check if option is set in e.g., o.colrescale.colname
    f=paste0('col',optionname)
    if(f %in% names(o) && !is.null(o[[f]])){
      colvals=o[[f]];
      val_found=struct_getcolentry(colvals,colname); val=val_found$val;
      if(val_found$found){ out=val; found=T; ocolname=val_found$var; }
    }
  }
  else{
    return(o);
  }
  if(gettingdefault__){ return(list(val=out,found=found,ocolname=ocolname)); }
  if(gettingsuggested__){ return(list(val=out,found=found,ocolname=ocolname )); }
  
  if(!is.null(out) && is.character(out) && out %in% c('DEFAULT','default')){
    val_found=getcoloption(o,optionname,'DEFAULT',gettingdefault__T);
    if(!val_found$found){ val_found=getcoloption(o,optionname,'default',gettingdefault__T); }
    if(val_found$found){ out=val_found$val; ocolname=val_found$var; }
  }

  if(is.null(out)){ #see if we have a suggested value for this column.
    val_found=getcoloption(suggestedoptions(),optionname,colname,T,gettingsuggested__=T);  val=val_found$val;
    if(!is.null(val)){ out=val; found=val_found$found; ocolname=val_found$var; }
  }
  if(is.null(out) && warn){
    msg=sprintf('Cannot find a value for: column [%s] option [%s]. Using NULL as the option value. You can set a column-specific value or a default value for all columns in your project configuration. ',colname, optionname);
    if(optionname=='weight'){ msg=sprintf('%s Note that using NULL means this column is ignored in Score calculations. A positive weight (e.g., +1) should be used for columns that want to maximize (ie., the higher the better); and a negative weight (e.g, -1) should be used for columns you want to minimize (ie., the lower the better).', msg); }
    else if(optionname=='handlenan'){ msg=sprintf('%s Using NULL means missing data values for this column are ignored in the Score calculations (giving other columns more weight). You can use 0, "min", "max", "mean" if you would like to replace the missing values.',msg); }
    warning(sprintf('%s\n',msg));
  }

  if(!is.null(out)){ attr(out,'found')=found; attr(out,'ocolname')=ocolname; }
  return(out)
}


###############################################################
#get the weight vector for the specified colnames.

getcoloption_weights=function(colnames,o=list()){
  if(!is.null(o[['weights']])){
    if(!all(colnames %in% names(o$weights))){
      cat(paste0('---WARNING: weights argument is given, but it does not contain a value for every colnames. Filling in the missing ones from other option parameters.\n'));
      missingcolnames = setdiff(colnames, names(o$weights));
      o$weights = list_merge(o$weights, getcoloption_weights(missingcolnames, list_merge(o,list(weights=NULL))));
    }
    return(o$weights[colnames]);
  }

  numcols=length(colnames);
  ocolnames=c(); #the colname that we find in the option (may be exactly colname or a prefix_)
  W=rep(1, numcols); #1's will be replaced with the weight of each column. If a weight is not found, we stop  below.
  for(i in 1:numcols){
    w = getcoloption(o, 'weight', colnames[i] );

    #if the weight is specified as e.g,. "1.5 each", we don't need to resolve multi-matched colnames later, so no need to keep those in ocolnames.
    if(is.character(w) && grepl('each$',w)){ w=gsub(' *each$','',w); }
    else if(!is.null(attr(w,'ocolname'))) { ocolnames[i] = attr(w,'ocolname'); }
    if(is.null(w)){ stop(sprintf('Could not get the weight value for [%s]',colnames[i])); }
    W[i] = w;
  }

  #Handle multi-matched colnames (e.g., we have gtex_numsamples, gtex_maxtpm, etc., but only have "gtex" listed as an option in o. )
  ocolnamefreq=plyr::count(ocolnames);
  Imultimatch=ocolnamefreq$freq>1;
  for(i in which(Imultimatch)){
    ocolname=ocolnamefreq[i,'x'];
    if(is.na(ocolname)) next;
    inds = which(ocolnames %in% ocolname);
    if(length(unique(W[inds]))>1){ 
      stop(sprintf('Unexpected: The weight option colname [%s] matches multiple columns: [%s], but we found different option values for them.',ocolname,paste0(colnames[inds],collapse=', ')));
    }
    w=W[inds[1]]; #they should all be the same, just use the first one.
    if(!is.character(w) || !grepl('total$',w)){ warning(paste0('The weight option colname [',ocolname,'] matches multiple columns: [',paste0(colnames[inds],collapse=', '),']. The option value is [',w,'] which will be distributed (ie. divided by [',length(inds),']). Use "',w,' total" as the weight value to avoid this warning;  or use "',w,' each" if you intended to use this weight for all matching columns.\n')); }
    if(is.character(w)&&grepl('total$',w)){
      w=gsub(' *total$','',w);
    }
    W[inds]=as.numeric(w)/length(inds);
  }

  #remove any remaining "..total" suffix (which matched single-columnname and weren't handled in the multi-match above)
  for(i in 1:numcols){
    w=W[i];
    if(is.character(w)){
      if(grepl('total$',w)) w=gsub(' *total$','',w);
    }
  }
  W = as.numeric(W);
  W = as.list(W);
  names(W)=colnames;
  return(W);
}

###############################################################
getcoloption_curves=function(colnames,o=list()){
  if(!is.null(o$curves)){
    if(!all(colnames %in% names(o$curves))){
      dbg_warnf('curves argument is given, but it does not contain a value for every colnames. Filling in the missing ones from other option parameters.');
      missingcolnames = setdiff(colnames, names(o$curves));
      o$curves = list_merge(o$curves, getcoloption_curves(missingcolnames, list_merge(o,list(curves=NULL))));
    }
    return(o$curves[colnames]);       
  }
  curves=list();
  for(colname in colnames){
    curve = getcoloption(o, 'curve', colname );
    if(is.null(curve)){ curve=0; }
    curves[[colname]]=curve;
  }
  return(curves);
}


#stk__=dbg_nicestack(1); message(sprintf('getcoloption.r sourced from: %s',stk__));
