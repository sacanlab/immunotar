source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2023,2024 by Ahmet Sacan and Rawan Shraim


source_disabled__('biodb.r')
source_disabled__('config.r')
source_disabled__('theratardb.r')
source_disabled__('util.r')

###############################################################
#this is meant for getting the files provided by pancancer project (not the files we generate)
#fileid: meta|protein_averaged. any other fileid is used as filename in the url.
pancancer_file=function(fileid,o=list(),...){
  baseurl='https://cog.sanger.ac.uk/cmp/download/';
  fileurls=list(meta='model_list_20240110.csv', protein_averaged='Proteomics_20221214.zip');
  fileurl=fileurls[[fileid]];
  if(isempty(fileurl)) fileurl=fileid;
  url=paste0(baseurl,fileurl);
  #datafile: https://cog.sanger.ac.uk/cmp/download/
  #        Unzip, and datafile is: Protein_matrix_averaged_20221214.tsv

  downloaddir=io_mkdirif(io_name(biodb_dir(),'pancancer'));
  file=downloadurl(url,file=paste0(downloaddir,'/'));

  if(fileid=='protein_averaged'){
    zipfile=file;
    fs=unzip(zipfile,list=T);
    I=grepl('^Protein_matrix_averaged_[0-9]+\\.tsv',fs$Name);
    stopfif(sum(I)!=1,"downloaded zip file does not contain the expected data file or contains multiple hits. The filenames were: ",fs$Name);
    f=fs$Name[I];
    io_issecurefilename(f);
    file=io_name(downloaddir,f);
    if(io_isfileandnotempty(file)){ return(file); }
    unzip(zipfile,files=f,exdir=downloaddir);
    assertf(io_isfile(file),'unzipped, but cannot find file where its supposed to be [%s]',file);
  }
  return(file);
}
###############################################################
#load information available for all pancancer samples/models
#checkdata=T will check the data file and remove any meta samples for which data is not available.
pancancer_getmetadata=function(checkdata=T,dbg=F){
  metafile=pancancer_file('meta')
  ocache=cache_opts(dataid=checkdata,memcache=T,recache=checkdata&&dbg)
  if(!ocache$recache){
    #msgf("Returning cached metadata...");
    return(cache_load(ocache)); 
  }
  meta=data_readfile(metafile, header=T)
  I=grepl('Childhood',meta$cancer_type_detail); #this doesn't look to be a reliable way to identify which ones are cancer.
  #And some samples don't have age information.
  #Consider any known age<=20 or cancer_type_detail='Childhood' (if age is not available), to be pediatric.
  J=meta$age_at_sampling<=20;
  I[!J]=F; #if age>20, we ignore the 'Childhood' categorization.
  K=I|J;
  K[is.na(K)]=F;
  meta$ispediatric=K;
  
  if(checkdata){
    d=pancancer_getalldata();
    I = meta$model_id %in% colnames(d);
    if(!all(I)){
      #msgf("Some meta model_ids (SIDs) did not have any entries in the datafile; I am removing them from the meta data. The missing meta SIDs were: %s",str_csv(meta[!I]$model_id));
      msgf("Removing %d of %d meta SIDs for which no data is available.",sum(I),nrow(meta));
      msgfif(dbg,'The following meta SIDs were not found in the data file: %s',str_csv(meta[!I]$model_id));
      meta=meta[I,];
    }
    if(dbg){
      dnotmeta=base::setdiff(colnames(d),meta$model_id);
      msgfif(!isempty(dnotmeta), 'The following data file SIDs are not listed in the meta file: %s',str_csv(dnotmeta));
    }
  }
  cache_save(meta,ocache);
  return(meta);
}  
###############################################################
#minsamples: only get cancers where we have at least this many samples.
#see pancancer_analysis.rmd for a frequency plot of available cancers.
pancancer_cancerlist=function(...){
  o=opt_set( #list the options that the cachefile depends on.
    minsamples=3
    ,onlypediatric=F
    ,removenonspecific=T #remove entries with nonspecific cancer (anything that starts with 'Other')
    ,removenoncancer=T
  ,...);
  meta=pancancer_getmetadata();
  if(o$onlypediatric){
    meta=meta[meta$ispediatric,];
  }
  if(o$removenoncancer){
    meta=meta[!is.na(meta$cancer_type),]
  }

  ss=unlist(meta[,'cancer_type']);
  if(o$minsamples>0){
    d=data.frame(Group=ss) |> dplyr::group_by(Group) |> dplyr::summarize(N=dplyr::n())
    ss=d[d$N>=o$minsamples,]$Group;
  }
  else{
    ss=unique(ss);
  }
  if(o$removenoncancer){ ss=ss[!(ss %in% c('Non-Cancerous'))]; }
  if(o$removenonspecific){
     ss=ss[!grepl('^Other ',ss)]; }
  return(ss)
}
pancancer_pediatriccancerlist=function(...){
  return(pancancer_cancerlist(...,onlypediatric=T));
}

###############################################################
pancancer_getalldata=function(fileid='protein_averaged'){
  file=pancancer_file(fileid);
  d=data_readfile_cached(file,header=T,rowNames=F);
  assertf(colnames(d)[[1]]=='uniprot_id' && d[[2,1]]=='model_name' && d[[2,2]]=='model_id', 'Data format does not match the expected information we have seen before.');
  sids=unlist(d[3:nrow(d),2]);
  assertf(arr_isunique(sids),'Sample ids are not unique');
  genes=unlist(d[1,3:ncol(d)]);
  assertf(arr_isunique(genes),'Gene symbols are not unique');
  d=d[3:nrow(d),3:ncol(d)];
  d=data.table::transpose(d);
  d=as.data.frame(d)
  rownames(d)=genes;
  colnames(d)=sids;
  d=type.convert(d, as.is =TRUE)  #this will make sure the data is now numeric (previously, it was text, because the table contained mixed text/number data)
  return(d);
}


###############################################################
#get the cancer data file for a specific cancername.
#cancername may be a list if you need to match multiple cancernames.
pancancer_getcancerdatafile=function(cancername,...){
  ocache=list( #list the options that the cachefile depends on.
    onlypediatric=F
    ,onlysurfacegenes=F
    ,compartments_score=2 #if onlysurfacegenes=T, this threshold is used.
  );
  o=opt_set(ocache,...)
  ocache=o[names(o) %in% names(ocache)];
  cachefile=io_sanitizefilename(paste0('pancancer.',cancername,'.',str_md5(ocache),'.fst'));
  cachefile=io_name(sys_cachedir(),cachefile); 
  if(io_isfileandnotempty(cachefile)){ return(cachefile); }

  meta=pancancer_getmetadata();
  if(o$onlypediatric){ meta=meta[meta$ispediatric,]; }
  I=meta$cancer_type %in% cancername;
  #if(!any(I)){    browser();  }
  stopfif(!any(I),'Requested cancername [%s] is not found among the cancer_types.',cancername);
  meta=meta[I,];
  d=pancancer_getalldata();
  J=colnames(d) %in% meta$model_id;
  if(!any(J)){
    msgf('None of the samples in the meta data for cancer [%s] are available in the pancer alldata file. The SIDs were: ',cancername,str_csv(meta$model_id))
    return(as.data.frame())
  }
  d=d[, J, drop=F]; #without drop=F, the result is returned as a vector if it's a single column.
  if(o$onlysurfacegenes){  d=getsurfacegenes(d,compartments_score=compartments_score);  }
  data_writefile(d,cachefile);
  return(cachefile);
}
pancancer_getpediatriccancerdatafile=function(cancername,...){
  return(pancancer_getcancerdatafile(cancername,...,onlypediatric=T));
}

#asframe can be T/F or a csv of theratar,depmap
pancancer_diseasemap=function(asframe=F,cancername=NULL){
  map=list();
  map[["B-Cell Non-Hodgkin's Lymphoma"]]=list(short='BLL',theratar=list('B-cell non-hodgkin lymphoma', 'Diffuse large B-cell lymphoma'), depmap=list('B-cell, Non-Hodgkins'))
  map[["T-Lymphoblastic Leukemia"]] = list(short='T-ALL',theratar=list('T-cell leukaemia','Adult T acute lymphoblastic leukemia', "T-cell acute lymphoblastic leukaemia"), depmap=list("Acute Lymphoblastic Leukemia (ALL), T-cell")); #these two words capture all non-hodgkin entries and exclude yes-Hodgkin.
  map[["Acute Myeloid Leukemia"]]=list(short='AML',theratar=list('%myeloid leukaemia', 'Acute myeloid leukemia'), depmap=list("(AML)"))
  map[["Ewing's Sarcoma"]]=list(short='ES',theratar=list('ewing%'), depmap=list('ewings'))
  map[["B-Lymphoblastic Leukemia"]]=list(short='B-ALL',theratar=list('B Lymphoblastic leukaemia', 'B-cell acute lymphoblastic leukaemia'), depmap=list('Acute Lymphoblastic Leukemia (ALL), B-cell'))
  map[["Melanoma"]]=list(short='ML',theratar=list('%melanoma'), depmap=list("melanoma"))
  map[["Neuroblastoma"]]=list(short='NBL',theratar=list('neuroblastoma'), depmap=list("neuroblastoma"))
  map[["Non-Small Cell Lung Carcinoma"]]=list(short='NSCLC',theratar=list('%non-small cell lung cancer', '%non-small-cell lung cancer', 'non small cell lung cancer'), depmap=list("non-small cell lung cancer"))
  map[["Rhabdomyosarcoma"]]=list(short='RMS',theratar=list('rhabdomyosarcoma'), depmap=list("rhabdomyosarcoma"))
  map[["Osteosarcoma"]]=list(short='OS',theratar=list('osteosarcoma'), depmap=list('osteosarcoma'))
  map[["Hepatocellular Carcinoma"]]=list(short='HC',theratar=list('hepatocellular'), depmap=list("hepatocellular carcinoma"))
  map[["T-Cell Non-Hodgkin's Lymphoma"]]=list(short='TLL',theratar=list('%t-cell lymphoma'), depmap=list("T-cell, non-hodgkins"))
  map[["Burkitt's Lymphoma"]]=list(short='BL',theratar='%burkitt%',depmap='%burkitt%');


  if(var_tobool(asframe)){
    source_disabled__('depmapdb.r')
    source_disabled__('theratardb.r')

    if(is.logical(asframe)){ asframe=c('theratar','depmap'); }
    asframe=csv(asframe);
    for(type in asframe){
      d=data.frame();
      for(name in names(map)){
        r=list(short=map[[name]]$short,cancername=name,searchnames=str_csv(unlist(map[[name]][[type]])),type=type);
        if(type=='theratar'){
          hits=theratardb_searchdisease(map[[name]][[type]]);
          rs=data_colmergeascombinations(r, dplyr::distinct(hits[,'disease',drop=F]))
          d=plyr::rbind.fill(d,rs);
        }
        else{
          hits=depmapdb_searchsamples(map[[name]][[type]],searchinfields=c("primarydisease","subtype"));
          hits=dplyr::distinct(hits[,c('primarydisease','subtype'),drop=F])
          colnames(hits)[colnames(hits)%in%'primarydisease']='disease'
          rs=data_colmergeascombinations(r,hits)
          d=plyr::rbind.fill(d,rs);
        }
      }
    }
    return(d)
  }


  if(!is.null(cancername)){
    ret=map[[cancername]];
    if(is.null(ret)){
      warnf('Cancername [%s] not found in the map. Using it as is.',cancername);
      ret=list(theratar=cancername,depmap=cancername);
    }
    return(ret);
  }

  return(map);
}
pancancer_theratar_map=function(cancername=NULL){
  map=pancancer_diseasemap(cancername=cancername);
  if(is.null(cancername)){
    ret=list();
    for(name in names(map)){ ret[[name]] = map[[name]]$theratar; }
    return(ret);
  }
  else{
    return(map$theratar);
  }
}
pancancer_depmap_map=function(cancername=NULL){
  map=pancancer_diseasemap(cancername=cancername);
  if(is.null(cancername)){
    ret=list();
    for(name in names(map)){ ret[[name]] = map[[name]]$depmap; }
    return(ret);
  }
  else{
    return(map$depmap);
  }
}

pancancer_shortname_map=function(cancername=NULL){
  map=pancancer_diseasemap(cancername=cancername);
  if(is.null(cancername)){
    ret=list();
    for(name in names(map)){ ret[[name]] = map[[name]]$short; }
    return(ret);
  }
  else{
    return(map$short);
  }
}
########################################################################
#Make a single project
#Additional options (onlypediatric,onlysurfacegenes,compartments_score) are passed into pancancer_getcancerdatafile()
pancancer_project=function(cancername,...){
  source_disabled__('depmapdb.r')
  source_disabled__('theratardb.r')
 o=opt_set(
    importdefaultparams=T
    ,importoptimizedparams=NULL #will default to TRUE if no optimizedparamsfile is given. when such a file is given, it'll be an import.
    ,devmode=F #used to create a smaller data for quick code-testing
    ,optimizedparamsfile=T #if given, we'll add it as an importfile. This ends up being the same file as optimizedparams.yml; but a different file when devmode=T.
    ,datafile=NULL #when not given, we'll use pancancer_getcancerdatafile().
    ,immunoadd=0  # whether to artifically inflate theratar targets that are immunotherapy-related. (suggested value: 10)
    ,onlysurfacetargets=T
    ,knownscores=NULL #datatable with rownames genes and score column containing positive  values (or negative values for knownnegative targets). if we given, we'll use it; if not given, we use theratardb. If you use your own knownscores, you may want to set onlysurfacetargets=F if you don't want the non-surface proteins in your knownscores to be removed.
    ,projectname=NULL #defaults to cancername.
    ,projectshortname=NULL #defaults to pancancer_shortname_map(cancername)
    ,withgtextissues=F #whether to use gtex-per-tissue enrichment type.
    ,importfile=NULL
  ,...)
  if(isempty(o$datafile)){
    o$datafile=pancancer_getcancerdatafile(cancername,o);
  }
  theratardisease=pancancer_theratar_map(cancername=cancername);

  knownscores=o[['knownscores']];
  if(is.null(knownscores)){
    dthera=theratardb_disease2genesymbols(theratardisease,asframe=c('genesymbol','higheststatus_'),immunoadd=o$immunoadd);
    dthera = data_combineduplicaterows( dthera, idcolumn='genesymbol', func=max ) #there is a slight problem here when we are dealing with negative status scores, but we'll ignore it. Ideally, we would use absmax() for negative scores, but keep max() if there is a positive version of a gene.
    rownames(dthera)=dthera$genesymbol;
    dthera=dthera[,'higheststatus_',drop=F]; colnames(dthera)='score';      
    knownscores=dthera;
  }
  knownscores_pos=knownscores[knownscores$score>0,,drop=F];
  knownscores_neg=knownscores[knownscores$score<0,,drop=F];
  if(!isempty(knownscores_pos)&&o$onlysurfacetargets){ knownscores_pos=getsurfacegenes(knownscores_pos); } #only the positive are filtered. leave negatives as negative, even if they are not surface proteins.

  depmapdisease=pancancer_depmap_map(cancername=cancername);
  ddepmap=depmapdb_searchsamples(depmapdisease,searchinfields=c("primarydisease","subtype"));
  depmapids=ddepmap$depmapid
  
  p=list(
    dataset=list(datafile=o$datafile)
    ,importdefaultparams=o$importdefaultparams
    ,importoptimizedparams=o$importoptimizedparams
    ,enrich=list(
      enrichtypes=paste0('gtex,evodevo_pediatric,healthyprot,compartments_sp,cirfess_spc,uniprot,depmap,opentargetsurface',var_pick(o$withgtextissues,',gtextissue',''))
      ,depmapids=depmapids
    )
    #ahmet: I turned off this default weight
    #,weight = 1 #DISCUSS: Do not set a global weight for a project, otherwise we may miss spelling mistakes, etc. (e.g., opentargetsurface vs. opentargetsurface)
    ,getfull=T
    #these are not required by project-run, but useful for annotation/reports.
    ,projectname=var_pickfirstnotnull(o$projectname,cancername)
    ,projectshortname=var_pickfirstnotnull(o$projecshorttname,pancancer_shortname_map(cancername))
    ,disease=cancername 
    ,theratardisease=theratardisease
    ,knownscores=knownscores
    ,knownpositives=rownames(knownscores_pos)
    ,knownnegatives=rownames(knownscores_neg)
    ,numknownpositives=nrow(knownscores_pos)
    ,knownpositivescores=knownscores_pos
    ,knownnegativescores=knownscores_neg
    ,depmapdisease=depmapdisease
    ,devmode=o$devmode
    ,randid=str_rand(32) #randomid used in projects_setupcluster() to tell if the current project list is the same as the one that was exported previously.
  )
  
  if(var_tobool(o$optimizedparamsfile)){
    optimizedparamsfile=project_optimizedparamsfile(...);
    p$importfile=optimizedparamsfile;
    if(is.null(o$importoptimizedparams)){ p$importoptimizedparams=FALSE; }
  }
  else if(is.null(o$importoptimizedparams)){ p$importoptimizedparams=TRUE; }
  
  return(p)
}

pancancer_customproject_neuroblastoma=function(...){
    knownscores=data.frame(score=c(12,12,6.5,6.5,5,5))
    rownames(knownscores)=c('ALK', 'GPC2', 'CD276','GFRA2', 'L1CAM', 'DLK1');
    return(pancancer_project('Neuroblastoma',...,knownscores=knownscores));
}

pancancer_customproject_ewing=function(...){
  knownscores=data.frame(score=c(12,12,12,12,12,12,12,12,12,6,6,1))
  rownames(knownscores)=c('IL1RAP', 'ATP11C', 'STEAP2','ADGRG2','ENPP1','CDH11','STEAP1','LINGO1','SLCO5A1','CD99', 'ROR1', 'ENG');
  return(ewingmm_project("Ewing's Sarcoma",knownscores=knownscores, ...));
}



########################################################################
#cancernames will default to pancancer_getcancerlist(...)
#other options are passed into pancer_project()
pancancer_projects=function(cancernames=NULL,...){
  o=opt_set(
    minknownpositives=1
    ,docache=T
    ,recache=F
    ,prepare=T
    ,run=T #implies prepare=T
    ,devmode=F
    ,optimizedparamsfile=T #if given, we'll load it and (re-)apply it to each project.
    ,doparallel=T
  ,...)

  o$optimizedparamsfile=project_optimizedparamsfile(...); #need to have the optimizedparamsfile listed in o, so a different cachefile is used for different optimizedparams files.
  ocache=cache_opts(list_removefields(o,'doparallel'),depends=c(pancancer_file('meta'),pancancer_file('protein_averaged'), time_numeric('2024-05-03 23:17:00')));
  if(!ocache$recache){
    msgf('Loading projects from cachefile [%s]  ...',ocache$cachefile);
    ps=cache_load(ocache);
    ps=projects_cache_updateifneeded(ps,ocache,o);
    return(ps);
  }
  else{
    if(is.null(cancernames)){ cancernames=pancancer_cancerlist(...); }
    ps=list();
    for(i in 1:length(cancernames)){
      ps[[i]]=pancancer_project(cancernames[[i]],...)
      if(o$devmode && i==2){
        warnf('Devel-mode is ON. Keeping only two projects for a quick-run.');
        break;
      }
    }
    if(o$minknownpositives>0){
      I=lists_extractfield(ps,'numknownpositives')>=o$minknownpositives;
      ps=ps[I];
      }
    if(o$run){
      ps=pancancer_projects(cancernames,o,doparallel=F,run=F,prepare=T);
      o$prepare=F;    
    }
    if(o$prepare){
      msgf('Preparing the projects. This may take a while...')
      if(!exists('projects_prepare')){ source_disabled__('projects.r'); }
      ps=projects_prepare(ps,doparallel=o$doparallel);

      if(o$minknownpositives>0){
        I=lists_extractfield(ps,'numknownpositives__')>=o$minknownpositives;
        ps=ps[I];
      }
    }
    
    if(o$run){
      ps=projects_fillweightsigns(ps,o,doparallel=o$doparallel);
      ps=projects_run(ps,doparallel=o$doparallel);
    }
    
    cache_save(ps,ocache);
  }

  return(ps);
}

pancancer_pediatricprojects=function(cancernames=NULL,...){
  return(pancancer_projects(cancernames,onlypediatric=T,...));
}

########################################################################
