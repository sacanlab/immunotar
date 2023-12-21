#% Copyright (C) 2022 by Rawan Shraim, Ahmet Sacan
#source('biodb.r')
#source('util.r')
#source('theratardb.r')

###############################################################
#Merge assuming x (maindata) has genesymbol as rownames, and y (moredata) has a column named genesymbol.
#If y is a dataframe with rownames and you want to use its rownames as merge-keys, use by.y=0.
#Keeps all rows from x (even if some rows do not appear in y).
#Doesn't keep rows of y that are not in d. (Except if x is empty, then we return y).
#ycolprefix (if given) will prefix the y columns (except for the by.y column) before they are merged to x

enrich_merge = function(x, y, ycolprefix='', by.x=0, by.y='genesymbol', all.x=T, all.y=F){
  if(!length(y)){ return(x); }
  if(ycolprefix!=''){
    ynames = names(y);
    for(i in 1:length(ynames)){
      if(ynames[i]!=by.y){ ynames[i] = paste0(ycolprefix,ynames[i]); }
    }
    names(y)=ynames;
  }
  if(!nrow(x)){ return(y); }
  x= merge(x,y, by.x=by.x, by.y=by.y, all.x=all.x, all.y=all.y);
 #merge() creates a new column for Row.names; move it back to rownames and delete that column.
  rownames( x ) = x$Row.names;   x = x[, !names(x) %in% c('Row.names',by.y) ]
  return( x );
}

###############################################################
# Adds max TPM value of each gene across all tissues surveyed in GTEx

enrich_gtex = function(d){
  return( enrich_merge(d, gtexdb_getsummary(rownames(d)), 'gtex_') );
}

###############################################################
# Adds max RPKM value of each gene across all tissues surveyed in GTEx.

enrich_evodevo_pediatric = function(d){
  return( enrich_merge(d, evodevodb_pediatric_getsummary(rownames(d)), 'evodevo_pediatric_') );
}

###############################################################
#' Compartments is a protein localization database that has a confidence score related to the protein being a surface membrane protein. 
#' The input to the function is a vector or gene/protein names or an expression data-set
#' The output will include the confidence score associated to each gene in the expression dataset. 
#' 
enrich_compartments_sp = function(d){
  moredata=compartmentsdb_getcellsurfacegenes(rownames(d));
  return(enrich_merge(d, moredata,'compartments_sp_') );
}

###############################################################
#' CIRFESS is a protein localization database that has a confidence score related to the protein being a surface membrane protein. 
#' The input to the function is a vector or gene/protein names or an expression data-set
#' The output will include the confidence score associated to each gene in the expression data-set 
#' 
#' @export
enrich_cirfess_spc = function(d){
  moredata=cirfessdb_getspc(rownames(d));
  return( enrich_merge(d, moredata,'cirfess_') );
}

###############################################################

enrich_healthyprot = function(d){
  moredata=jiangproteomedb_getsummary_maxabundance(rownames(d));
  return(enrich_merge(d, moredata, 'healthyprot_'))
}


###############################################################
#depmapids pull every cell line queried by depmap 
#collate='merge': adds a single column merging all genes from each depmapid. Probabilities from multiple depmapids for each gene are merged by taking the maximum probability. If you want to use the average probability, use mergefunc=mean.
#collate='separate': adds a separate column for each depmapid.

#' The Depmap project completed a CRISPR and siRNA screen of many genes across available cancer cell lines. The database has the probability that a gene is associated to cancer cell line proliferation. Given there are multiple cancer cell lines per cancer phenotype, the max probability is reported back for each gene. 
#' The input to this function is an expression data-set and the cancer phenotype. 
#' The output of this function is a column added to the expression data-set labeled depmap which includes the probability of the gene being associated to the cancer phenotype specified. 
#' 
#' @export
enrich_depmap = function(d, depmapids, collate='merge', mergefunc=max, colname=NULL ){
  if(is.null(collate)){ collate='separate'; }
  if(is.null(mergefunc)){ mergefunc=max; }
  if(is.character(mergefunc)){ mergefunc=str2func(mergefunc); }

#  source('depmapdb.r')
#  source('data_combineduplicaterows.r');
  if(collate=='merge'){
    ddep = depmapdb_getgenedependency(depmapids)[, c('genesymbol','probability')];
    
    #When using multiple depmapids, each gene will have multiple records in the moredata,
    #one for each depmapid. Consolidate these duplicates by taking the maximum probability among all depmapids.
    #NOTE: Does depmap website use a different strategy to consolidate probabilities from multiple cell lines? Depmap website provides combined probabilities, but they are not providing those values. Neither have they gotten back to us about how these values are calculated.
    ddep = data_combineduplicaterows( ddep, idcolumn='genesymbol', func=mergefunc )

  	if(is.null(colname)){ colname='depmap_probability'; }
    names(ddep)[names(ddep)=='probability'] = colname;

    return( enrich_merge(d, ddep ) );
  }
  else if(collate=='separate'){
    for(depmapid in depmapids){
      if(is.null(colname)){ thiscolname=paste0('depmap_probability_',depmapid); }
      else{ thiscolname=paste0(colname,depmapid); }
      ddep = depmapdb_getgenedependency(depmapid)[, c('genesymbol','probability')];
      #we still need to combine, b/c depmapid may be a text that matches multiple depmapid's in ddep. If you want a separate column for each depmapid, ensure the input depmapids is numeric (so no text-search is done to match multiple numeric depmapids).
      ddep = data_combineduplicaterows( ddep, idcolumn='genesymbol', func=mergefunc )
      names(ddep)[names(ddep)=='probability'] = thiscolname;
      d=enrich_merge(d,ddep);
    }
    return(d);
  }
  else{
    stop('Invalid collate option. Must be one of merge|separate.')
  }
}


###############################################################
#goids can be numeric GO ids; or text(s) to be searched using godb_searchterm.
#collate='merge': adds a single column merging all genes from each goid/goterm.
#collate='separate': adds a separate column for each goid/goterm.
#when collate='separate', colname is used as a prefix, and goid is added to obtain the colname.


#' Gene Ontology is a functional database that links genes to biological pathways. The user can query which genes in an expression database are associated to pathways of interest using enrich_go. 
#' The input to this function is an expression data-set and pathways of interest in parameter goids. 
#' The output of this function is an added column to the expression data-set labeled go which includes a binary scoring based on whether the gene in the expression data-set is found in the GO pathways queried. 
#' 
#' @export
enrich_go = function(d, goids, collate='separate', colname=NULL){
  if(is.null(collate)){ collate='separate'; }
	genes=c();
  for(goid in goids){
  	if(is.null(colname)){ thiscolname=paste0('go_',goid); }
  	else if(collate=='separate'){ thiscolname=paste0(colname,goid); }
  	else{ thiscolname=colname; }
  	
  	if(is.character(goid)){
  		goid_=godb_searchterm(goid);
	    if(!length(goid_)){
	    	warning(paste0('Goid [',goid,'] did not match any genes'));
	    	next;
	    }
  		goid=unlist(goid_$id);
  	}

    gogenes = godb_goids2genesymbols(goid,asframe=F);
    if(!length(gogenes)){
    	warning(paste0('Goid [',goid,'] did not match any genes'));
    	next;
    }
    
  	if(collate=='separate'){
  		dgo=data.frame(genesymbol=gogenes,col1=rep(1,length(gogenes))); colnames(dgo)[2]=thiscolname;
  		d=enrich_merge(d,dgo);
  	}
    else{
    	genes=c(genes,gogenes);
    }
  }
	if(length(genes) && collate!='separate'){
  	if(is.null(colname)){
  		if(length(goids)==1){ colname=paste0('go_',goids); }
  		else{ colname='go'; }
  	}
		dgo=data.frame(genesymbol=genes,col1=rep(1,length(genes))); colnames(dgo)[2]=colname;
		d=enrich_merge(d,dgo);
	}
	return(d)
}

###############################################################
#' Uniprot is a protein database that includes information about protein localization, functionality and structure. Using enrich_uniprot, the user can query the extra-cellular length of proteins in an expression data-set. 
#' The input to this function is the expression data-set. 
#' The output of this function is a column added to the expression data-set labeled uniprot which includes the length of the extracellular section of the protein based on uniprot. 
#' 
#' @export
enrich_uniprot = function(d){
  moredata=uniprotdb_getecmtotallength(rownames(d));
  return(enrich_merge(d, moredata, 'uniprot_'))
}

################################################################
#' The Pediatric molecular target list is a list of targets with developed reagents that are associated to pediatric cancers. Users have the ability to query their expression data-set agaist this list by running it through enrich_opentargets. 
#' The input to this function is an expression data-set.
#' The output of this function is a column added to the expression data-set labeled opentarget which is a binary score depending on whether or not the gene/protein from the expression data-set is found in this database. 
#' 
#' @export
enrich_opentarget = function(d){
  moredata=opentarget_getgenes(rownames(d));
  return( enrich_merge(d, unique(moredata),'opentarget_') );
}

###############################################################
#Add an opentargets enrichment for only surface proteins using the biodb.r surface protein only function 

enrich_surfaceopentarget = function(d){
  moredata=opentargetsurface_getgenes(rownames(d))
  return(enrich_merge(d, unique(moredata), 'opentargetsurface_'))
}

####################################################
#' Therapeutic target database includes protein/gene targets, their associated developed drugs, and phase of testing of drug. The user can query this database using enrich_theratarsuface. 
#' The input to this function is the expression data-set, the diseases of interested, the mode of action for the drug. 
#' The output is an added column to the expression data-set that includes scores for each gene/protein that is found in the database. The score is associated to the phase of development of the drug. 
#' 

#' @export
enrich_theratarsurface=function(d, diseases='%', moa=NULL, immunomult=1){
  
  gene_disease=theratardb_disease2genesymbols(diseases, minstatusscore=1, moa = moa, asframe=c('genesymbol','higheststatus_'))
  
  if(immunomult != 1){
    gene_disease_CAR=theratardb_disease2genesymbols(diseases,minstatusscore=1, moa = 'CAR-T-Cell-Therapy%', asframe=c('genesymbol','higheststatus_'))
    ###Look for intersecting genes between gene_disease and gene_disease_CAR and multiple score in gene disease by immunomult
    gene_disease_CAR$higheststatus_=immunomult*gene_disease_CAR$higheststatus_
    gene_disease=rbind(gene_disease_CAR, gene_disease[-which(gene_disease_CAR$genesymbol %in% gene_disease$genesymbol),])
  }
  gene_disease_surface=getsurfacegenes(gene_disease)
  return(enrich_merge(d, unique(gene_disease_surface), 'theratarsurface'))
}
