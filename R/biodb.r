source_disabled__=function(...){invisible(NULL)}
#Biodb database functions
# Copyright (C) 2022 by Ahmet Sacan
source_disabled__('util.r');

###############################################################
#directory where the sqlite files are kept. defaults to {datadir}/biodb
biodb_dir=function(){
  if(!exists('config')){ source_disabled__('config.r'); } 
  dir=config('biodbdir');
  if(is.null(dir)){ dir=io_name(sys_datadir(),'biodb'); }
  return(io_mkdirif(dir));
}
###############################################################
biodb_file=function(dbname,downloadifmissing=T){
  if(!exists('config')){ source_disabled__('config.r'); } 
  sqlitefile = config(paste0(dbname,'file')); #e.g., config("depmapdbfile")
  if(is.null(sqlitefile)){
    sqlitefile = file.path(config('biodbdir'),paste0(dbname,'.sqlite'));
  }
  canpopulate = config(paste0(dbname,'_populate'));
  if(!file.exists(sqlitefile) && !is.null(canpopulate)&&canpopulate){ #e.g, if opentargetdb_populate=True, then expect to have a file opentargetdb.r and a function opentargetdb_populate().
    cat(paste0('---NOTICE: ',dbname,' is a populatable database. Attempting to call the populate function...\n' ))
    source(paste0(dbname,'.r'));
    fun=get(paste0(dbname,'_populate'));
    fun(sqlitefile);
  }
  if(downloadifmissing && !file.exists(sqlitefile)){
    url = config(paste0(dbname,'url')); #e.g., config("depmapdburl")
    if(is.null(url)){
      s = paste0(readLines(paste0('http://sacan.biomed.drexel.edu/lic/candownloadfile?file=', basename(sqlitefile)),warn=F),collapse="\n");
      if(!grepl('<candownload>yes</candownload>',s,fixed=T) && basename(sqlitefile) != paste0(dbname,'.sqlite')){
        s = paste0(readLines(paste0('http://sacan.biomed.drexel.edu/lic/candownloadfile?file=', dbname, '.sqlite'),warn=F),collapse="\n");
      }
      if(grepl('<candownload>yes</candownload>',s,fixed=T)){
        url=gsub('^.*<url>(.*)</url>.*$','\\1',s)
      }
      else{ stop(paste0('Database file for [',dbname,'] not available and no URL to download the database is available. You need to set [',dbname,'url] in config.yml')); }
    }
    #TODO: Replace this with the new download() function.
    source_disabled__('bmes_download.r')
    message(paste0('I will attempt to download the database [',dbname,'] --> [',sqlitefile,']. Downloading may take a while (minutes to hours, depending on file size and your connection speed)...'))
    dir.create(dirname(sqlitefile), showWarnings = FALSE)
    sqlitefile = bmes_download(url,sqlitefile);
  }
  #stop(sqlitefile);
  #print(sqlitefile);
  return(sqlitefile);
}
  
###############################################################
biodb_connect=function(dbname,downloadifmissing=T){
  if(!exists('opt_set')){ source_disabled__('util.r'); } 
  installpackageifmissing(c("RSQLite","DBI"))
  sqlitefile=biodb_file(dbname,downloadifmissing);
  #print(sqlitefile)
  db=DBI::dbConnect(RSQLite::SQLite(),sqlitefile);
  return(db);
}
###############################################################
biodb_tablenames=function(dbname,table){
  if(is.character(dbname)){ db=biodb_connect(dbname) } else{ db=dbname; }
  ret=DBI::dbListTables(db);
  if(is.character(dbname)){ DBI::dbDisconnect(db); }
  return(ret);
}
###############################################################
#quote a text to make it SQL-safe.
q=function(s){
  return(DBI::dbQuoteLiteral(ANSI(),s)); #todo: do we need to pass in a connection as the first argument?
}
###############################################################
biodb_istable=function(dbname,table){
  if(is.character(dbname)){ db=biodb_connect(dbname) } else{ db=dbname; }
  ret=DBI::dbExistsTable(db,table);
  if(is.character(dbname)){ DBI::dbDisconnect(db); }
  return(ret);
}
###############################################################
biodb_tablefields=function(dbname,table){
  if(is.character(dbname)){ db=biodb_connect(dbname) } else{ db=dbname; }
  fields=DBI::dbListFields(db,table)
  if(is.character(dbname)){ DBI::dbDisconnect(db); }
  return(fields);
}
biodb_istablefield=function(dbname,table,field){
  return(field %in% biodb_tablefields(dbname,table));
}

###############################################################
#additional arguments (query,params) are passed onto 
biodb_query = function(dbname,sql,params){
  if(is.character(dbname)){ db=biodb_connect(dbname) } else{ db=dbname; }
  if(missing(params)){  t=DBI::dbGetQuery( db,sql ); }
  else{ t=DBI::dbGetQuery(db,sql,params=params); }
  if(is.character(dbname)){ DBI::dbDisconnect(db); }
  return(t);
}


###############################################################
#get previously unused placeholder names.
#params is a list of previously set up placeholder=>value items.
biodb_newplaceholdernames=function(n,params=NULL,prefix='q'){
  ret=c();
  if(missing(params)){ params=list(); }
  iname=length(params);
  for(i in 1:n){
    paramname='';
    while(T){ #find the next non-existent paramname.
      iname=iname+1;
      paramname=paste0(prefix, as.character(iname)); 
      if(!(paramname %in% names(params))){ break; }
    }
    ret[[length(ret)+1]]=paramname;
  }
  return(ret);
}

###############################################################
#return [q1,q2,q3..qn] and corresponding [q1=ss[1], q2=ss[2],...]. Useful in constructing sql statements that involve " IN (.., ..,) " constructs, which are built here using: " IN (:q1,:q2, ...) "
biodb_makeplaceholders=function(ss,params=NULL,prefix='q'){
  if(missing(params)){ params=list(); }
  paramnames=biodb_newplaceholdernames(length(ss),params,prefix);
  sql='';
  for(i in seq_along(ss)){
    if(i>1){ sql=paste0(sql,','); }
    sql=paste0(sql,':',paramnames[[i]]);
    params[[paramnames[[i]]]]=ss[i];
  }
  return(list(sql,params));
}

###############################################################
#return [field LIKE q1 OR field LIKE q2 OR field LIKE q3..qn] and corresponding [q1=ss[1], q2=ss[2],...]. Useful in constructing sql statements that involve " (field LIKE '...' OR field LIKE '...' OR ...) " constructs, which are built here using: " (field LIKE :q1 OR field LIKE :q2 OR ...) "
#if an ss does not already contain percent sign (or addpercents=TRUE), we add percents to the beginning and the end.
biodb_makefieldlike=function(field,ss,params=NULL,prefix='q',addpercents=NULL){
  if(missing(params)){ params=list(); }
  if(is.null(addpercents)) addpercents=!any(grepl('%',ss,fixed=T));
  paramnames=biodb_newplaceholdernames(length(ss),params,prefix);
  sql='';
  for(i in seq_along(ss)){
    if(i>1){ sql=paste0(sql,' OR '); }
    sql=paste0(sql,' ', field, ' LIKE :',paramnames[[i]]);
    val=ss[[i]];
    if(addpercents) val=str_ensuresuffix(str_ensureprefix(val,'%'),'%'); }
    params[[paramnames[[i]]]]=val;
    return(list(sql,params))
}

###############################################################
#check for a match of ss in a table field that contains csv of values. ss is considered exact item.
biodb_makecsvfieldlike=function(field,ss,params=NULL,prefix='q',exact=NULL){
  if(missing(params)){ params=list(); }
  sql='';
  for(i in seq_along(ss)){
    if(i>1){ sql=paste0(sql,' OR '); }
    val=ss[[i]];
    if(!is.null(exact)&&!exact){ val=str_ensuresuffix(str_ensureprefix(val,'%'),'%'); }
    paramnames=biodb_newplaceholdernames(4,params,prefix);
    sql=paste0(sql
    ,' ', field, ' ',ifelse(is.null(exact)&&(startsWith(val,'%')||endsWith(val,'%'))||!is.null(exact)&&!exact,'LIKE','='), ' :',paramnames[[1]]
    ,' OR ', field, ' LIKE :',paramnames[[2]]
    ,' OR ', field, ' LIKE :',paramnames[[3]]
    ,' OR ', field, ' LIKE :',paramnames[[4]]);
    params[[paramnames[[1]]]]=val;
    params[[paramnames[[2]]]]=paste0('%,',val);
    params[[paramnames[[3]]]]=paste0(val,',%');
    params[[paramnames[[4]]]]=paste0('%,',val,',%');
  }
  return(list(sql,params));
}


###############################################################
# searchinfields: If missing or empty, will default to all fields of the table.
# combinequerieswith: when there are multiple queries, should we AND them or OR them?
# exact: match entire value in table or do a substring search. You can specify exact=NULL, in which case exact=FALSE will be done if the query starts or ends with "%"
biodb_textsearch = function(dbname,tablename,queries,searchinfields=c(),exact=FALSE,where='',getfields=NULL,combinequerieswith='AND'){
    db=biodb_connect(dbname);
    if(missing(searchinfields)||length(searchinfields)==0){
      searchinfields=DBI::dbListFields(db,tablename)
    }
    params=list()
    paramnames=biodb_newplaceholdernames(length(queries),params,'qt');
    sql='';
    for(qi in seq_along(queries)){
      if(qi==1){ sql=paste0(sql,' ('); }
      else{ sql=paste0(sql,' ',combinequerieswith,' ('); }
      val=queries[[qi]];

      paramname=paramnames[[qi]]
      if(is.null(exact)&&(startsWith(val,'%')||endsWith(val,'%'))){
        params[[paramname]]=val;
        thisexact=F
      }
      else if(!is.null(exact)&&!exact){
        params[[paramname]]=str_ensuresuffix(str_ensureprefix(val,'%'),'%');
        thisexact=F
      }
      else{
        params[[paramname]]=val;
        thisexact=T
      }
            
      for(fi in seq_along(searchinfields)){
        if(fi>1){ sql=paste0(sql,' OR'); }
        sql = paste0(sql,' "', searchinfields[fi], '"', ifelse(thisexact,'=',' LIKE '), ':',paramname)
      }
      sql=paste0(sql,')')
    }
    if(where!=''){ sql=paste0('(',sql,') AND (',where,')'); }
    if(is.null(getfields)){ getfields='*'; }
    else{ getfields=paste0(getfields,collapse=', '); }
    sql=paste0('SELECT ',getfields,' FROM ',tablename,' WHERE ',sql);
    #print(sql);
    t=DBI::dbGetQuery(db,sql,params=params);
    DBI::dbDisconnect(db);

    return(t)
}

###############################################################


###############################################################
# searchinfields: If missing or empty, will default to all fields of the table.
# exact: match entire value in table or do a substring search.
#' @export
godb_searchterm = function(queries,searchinfields=c('name','description'),exact=FALSE,where='',obsolete=FALSE){
  if(!obsolete){
    if(where==''){ where='name NOT LIKE "obsolete %"'; }
    else{ where=paste0('(',where,') AND name NOT LIKE "obsolete %"' ); }
  }
  return(biodb_textsearch('godb','go',queries,searchinfields,exact,where));
}

###############################################################
#Get gene symbols for a list of goids. Returns a data frame or genelist; depending on the value of asframe.
#' @export
godb_goids2genesymbols = function(goids,asframe=T){
  if(asframe){ getfield='*'; }
  else{ getfield='genesymbol'; }
  sql=paste0('SELECT genesymbol,',getfield,' FROM "go" WHERE id IN (',paste(goids, collapse=","),')');
  #print(sql)
  rs=biodb_query('uniprotdb',sql);
  #print(rs);
  if(asframe){ return(rs); }
  if(!length(rs)){ return(c()); }
  genes=paste0(rs$genesymbol,collapse=',')
  if(genes==''){ return(c()); }
  return(unique(unlist(strsplit(genes,","))));
}
###############################################################
#Get gene symbols for a single goid. Returns list of gene symbols.
godb_goid2genesymbols = function(goid){
  sql=paste0('SELECT genesymbol FROM "go" WHERE id=',goid);
  #print(sql);
  rs=biodb_query('uniprotdb',sql);
  #print(rs);
  genes=rs$genesymbol;
  if(genes==''){ return(c()); }
  return(unlist(strsplit(genes,",")));
}

###############################################################
uniprotdb_allgenesymbols=function(taxonid=9606){ #taxonid=9606 is for human.
  d=biodb_connect('uniprotdb');
  if(biodb_istable(d,'uniprot')&&biodb_istablefield(d,'uniprot','ncbitaxon')){
    sql=paste0('SELECT DISTINCT(genesymbol) FROM "uniprot" WHERE ncbitaxon=',taxonid);
    rs=biodb_query(d,sql);
    if(!length(rs)){ stop(paste0('No genesymbols found for the requested taxonid [',taxonid,']. uniprotdb probably needs to be rebuilt.')); }
    return(rs$genesymbol);
  }
  else{
    sql=paste0('SELECT count(*) as count FROM "biodbbuild" WHERE species=',taxonid);
    r=biodb_query(d,sql);
    if(!r$count){ stop(paste0('uniprotdb is not built for the requested taxonid [',taxonid,']. uniprotdb probably needs to be rebuilt.')); }
    if(!biodb_istablefield(d,'go','ncbitaxon')){
      sql=paste0('SELECT count(*) as count FROM "biodbbuild" WHERE species!=',taxonid);
      r=biodb_query(d,sql);
      if(r$count){ stop(paste0('uniprotdb.go table is built for multiple species and no taxonid has been recorded. Cannot obtain genes for only taxon [',taxonid,']. uniprotdb needs to be rebuilt.')); }
      sql=paste0('SELECT genesymbol FROM "go"');
    }
    else{
      sql=paste0('SELECT genesymbol FROM "go" WHERE ncbitaxon=',taxonid);
    }
    rs=biodb_query('uniprotdb',sql);
    genes=rs$genesymbol;
    return(unique(unlist(strsplit(genes,","))));
  }
}


###############################################################
# Just a utility function to add genesymbol selection constraint to SQL.
# Append " AND genesymbol IN (....)" to sql and construct params accordingly.

biodb_query_wheregenesymbols = function(db,sql,genesymbols,params, genesymbolfield='genesymbol'){
  if(missing(genesymbols) || !length(genesymbols)){
    #print('missing genesymbols');
    ret=biodb_query(db,sql,params);
  }
  #when we have too many genesymbols in the query, the sql length becomes a problem and produces "Error: too many SQL variables". So, for large # of genesymbols, retrieve the data from the database and find intersection in R.
  else if(length(genesymbols)>1000){
    ret=biodb_query_wheregenesymbols(db,sql,params=params,genesymbolfield=genesymbolfield);
    #Rawan added to take care of error as the ret colnames are genesymbol not 'id' 
    genesymbolfield='genesymbol'
    if(length(ret)>0){
      if(!('genesymbol' %in% colnames(ret))){
        stopfif('Large number of genes being searched in this function, which only works if you retrieve back the %s column in your SELECT ... query. Add %s as one of the SELECTed columns.',genesymbolfield,genesymbolfield);
      }
      ret=ret[ret[,genesymbolfield] %in% genesymbols, ];
    }
  }
  else{
    #print('available genesymbols');
    sql_params = biodb_makeplaceholders(genesymbols,params); params=sql_params[[2]];
    if(grepl('\\bWHERE\\b',sql,ignore.case=T)){ sql = paste0(sql,' AND ',genesymbolfield,' IN (',sql_params[[1]],')'); }
    else { sql = paste0(sql,' WHERE ',genesymbolfield,' IN (',sql_params[[1]],')'); }
    #this is now done in biodb_makeplaceholders()
    #if(missing(params)){ params=unlist(sql_params[2]); }
    #else{ params = map(c, params, unlist(sql_params[2])); }
    #ret=biodb_query(db,sql,params);
    ret=biodb_query(db,sql,params);
  }
  if(genesymbolfield!='genesymbol' && !('genesymbol' %in% colnames(ret))){
    colnames(ret)[colnames(ret) == genesymbolfield] ='genesymbol';
  }
  return(ret);
}

###############################################################
# If genesymbols is not given, we retrieve the entire table.
compartmentsdb_getcellsurfacegenes=function(genesymbols){
  db=biodb_connect('compartmentsdb');
  fields = biodb_tablefields(db,'integrated');
  
  if('goid' %in% fields){
    sql='SELECT * FROM integrated WHERE goid=9986';
  }
  else if('compartment' %in% fields){
    sql='SELECT * FROM integrated WHERE compartment="Cell surface"';
  }
  else{
    #if table is missing compartment & goid fields, check if table was specifically built for the 'Cell surface' compartment.
    okay=TRUE;
    if(!biodb_istable(db,'singlecompartmenttables')){ okay=FALSE; }
    if(okay){
      r=biodb_query(db,'SELECT * FROM singlecompartmenttables WHERE channel="integrated"')
      if(r[1,'onlygoid']!=9986 && r[1,'onlycompartment']!='Cell surface'){ okay=FALSE; }
    }
    if(!okay){
      stop('Table [integrated] does not contain the necessary goid/compartment field (maybe it was built for another single compartment). You need to re-populate the database for all compartments, or specifically for "Cell surface"');
    }
    sql='SELECT * FROM integrated WHERE 1';
  }
  ret=biodb_query_wheregenesymbols(db,sql,genesymbols)
  DBI::dbDisconnect(db);
  return(ret);
}

###############################################################
#Get genes and their max expressions across all tissues.
#The database uses genesymbol as primary key; for multiple ensemblid's that map 
#to the same genesymbol, we use max/average to combine them. There were only 5 such duplicates.
#genesymbols input is optional. When not given, we retrieve the entire table.
gtexdb_getsummary = function(genesymbols){
  return(biodb_query_wheregenesymbols('gtexdb','SELECT * FROM rna_tpm_summary',genesymbols));
}


###############################################################
gtexdb_geteachtissue = function(genesymbols){
  return(biodb_query_wheregenesymbols('gtexdb','SELECT * FROM rna_majortissuetpm',genesymbols));
}

###############################################################
#Get genes and their max expressions across all tissues.
#The database uses genesymbol as primary key; for multiple ensemblid's that map 
#to the same genesymbol, we use max/average to combine them.
#genesymbols input is optional. When not given, we retrieve the entire table.
gtexdb_getsummary_maxtpm = function(genesymbols){
  return(biodb_query_wheregenesymbols('gtexdb','SELECT genesymbol,maxtpm FROM rna_tpm_summary',genesymbols));
}

###############################################################
#Similar to gtexdb_getsummary(), but uses EvoDevo Pediatric data.
evodevodb_pediatric_getsummary = function(genesymbols){
  ret=biodb_query_wheregenesymbols('evodevodb','SELECT * FROM rna_rpkm_summary WHERE stage="pediatric"',genesymbols);
  ret$stage=NULL; #this removes the stage field.
  return(ret)
}

###############################################################
#Similar to gtexdb_getsummary_maxtpm(), but uses EvoDevo Pediatric data.
evodevodb_pediatric_getsummary_maxrpkm = function(genesymbols){
  return(biodb_query_wheregenesymbols('evodevodb','SELECT genesymbol,maxrpkm FROM rna_rpkm_summary WHERE stage="pediatric"',genesymbols));
}

###############################################################
#Similar to gtexdb_getsummary(), but uses JiangProteome data.
jiangproteomedb_getsummary = function(genesymbols){
  ret=biodb_query_wheregenesymbols('jiangproteomedb','SELECT * FROM ptn_relabundance_summary',genesymbols);
  ret$stage=NULL; #this removes the stage field.
  return(ret)
}

###############################################################
#Get proteins and their max relative abundance across all tissues.
#The database uses genesymbol as primary key; for multiple ensemblid's that map 
#to the same genesymbol, we use max/average to combine them.
jiangproteomedb_getsummary_maxabundance = function(genesymbols){
  return(biodb_query_wheregenesymbols('jiangproteomedb','SELECT genesymbol,maxabund FROM ptn_relabundance_summary',genesymbols));
}

###############################################################
# Get SPC scores, which is an integer 0-4 representing how likely a protein is a surface protein. SPC is simply a summation of 4 other methods.
cirfessdb_getspc = function(genesymbols){
  return(biodb_query_wheregenesymbols('cirfessdb','SELECT * FROM spc',genesymbols));
}

###############################################################
uniprotdb_getecmtotallength = function(genesymbols){
  return(biodb_query_wheregenesymbols('uniprotdb','SELECT id,ecmtotallength FROM genesymbol',genesymbols,genesymbolfield='id'));
}

###############################################################
#extract genesymbols from a data frame.
# one of the columns must be "genesymbol"; otherwise we use the rownames.
dataframe_getgenesymbols=function(d){
  stopifnot(is.data.frame(d));
  if('genesymbol' %in% colnames(d)){ genesymbols=d$genesymbol; }
  else{ genesymbols=rownames(d); }
  return(genesymbols);
}

###################################################
#can take a dataframe as input  (to filter out any dataframe genes that are not surfacegenes.)
#Use NaN (or a very large number, e.g., infinity) for a score to disable it.
#If you need to get the list of genes from different sources separately in a list, set getseparate=T.
#compartments_score=2 threshold works best (based on experience)
#' @export
getsurfacegenes=function(genesymbols=NULL, spc_score=0, compartments_score=2, uniprot_ecmtotallength=NaN,getseparate=FALSE){
  if(is.data.frame(genesymbols)){
    stopfif(getseparate, "getseparate option is only available for a genesymbol list, and not for a dataframe.");
    d=genesymbols;
    genesymbols=dataframe_getgenesymbols(d);
    surfgenes=getsurfacegenes(genesymbols);
    return( d[genesymbols %in% surfgenes, ,drop=F] );
  }else{
    stopifnot(is.null(genesymbols)||istexts(genesymbols));
    sgenes=NULL; cgenes=NULL; ugenes=NULL;
    if(!is.nan(spc_score)&&!is.infinite(spc_score)){
      spc=cirfessdb_getspc(genesymbols)
      sgenes=spc$genesymbol[which(spc$spc > spc_score)];
    }
    if(!is.nan(compartments_score)&&!is.infinite(compartments_score)){
      comp=compartmentsdb_getcellsurfacegenes(genesymbols)
      cgenes=comp$genesymbol[which(comp$confidence >= compartments_score)]
    }
    if(!is.nan(compartments_score)&&!is.infinite(compartments_score)){
      uni=uniprotdb_getecmtotallength(genesymbols)
      ugenes=uni$genesymbol[which(uni$ecmtotallength >= uniprot_ecmtotallength)];
    }
    if(getseparate){
      return(list(cirfess=sgenes,compartments=cgenes,uniprot=ugenes));
    }
    return( unique(c(sgenes,cgenes,ugenes)) );
  }
}


##################################################

opentarget_getgenes = function(genesymbols){
  source_disabled__('opentargetdb.r')
  opentargetdb_populateifneeded();
  return(biodb_query_wheregenesymbols('opentarget','SELECT targetSymbol,1 AS isopentarget FROM opentarget', genesymbols, genesymbolfield = 'targetSymbol'));
}


#done: Rawan to make this function extract
#only surface proteins from opentarget list 

###################################################
opentargetsurface_getgenes=function(genesymbols){
  ret=opentarget_getgenes(genesymbols) #this dataframe has: "genesymbol" and "isopentarget"
  ret=getsurfacegenes(ret)
  colnames(ret)[colnames(ret) %in% 'isopentarget']='isopentargetsurface';
  return(ret)
}

###############################################################
