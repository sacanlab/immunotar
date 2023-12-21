#General utility functions
# Copyright (C) 2022,2023 by Ahmet Sacan

###############################################################
dbg_nicestack=function(cutlevels=1,withargs=F){
	st=sys.calls();
	if(length(st)<=cutlevels){ st=c(); }
	else{ st=st[1:(length(st)-cutlevels)]; }
	if(withargs){ return( paste(st,collapse=' -> ')); }
  else{
    ss=c(); for(f in st){ ss=c(ss,f[[1]]); }
    return(paste(ss,collapse= ' -> ' )); 
  }
}
dbg_trace=function(cutlevels=1){
  cat('%s\n',dbg_nicestack(cutlevels+1));
}

###############################################################
# this is a replacement for ifelse(). R"s ifelse() behavior is just dumb. It only works in a reasonable way if the lengths of the iftrue/iffalse are the same.
var_pick=function(condition,iftrue,iffalse){
  if(condition){ return(iftrue); }
  else{ return(iffalse); }
}
var_pickfirstnotnull=function(...){
  #for loop is not necessary, c(...) already excludes any NULLs.
  #for(v in c(...)){ if(!is.null(v)){ return(v); } }
  return(c(...)[[1]]);
}
var_tobool=function(x){
  if(isempty(x)){ return(F); }
  if(is.numeric(x)||is.logical(x)){ return(x); }
  if(is.character(x)&&(x %in% c('no','off','false','0'))){ return(F); }
  return(T);
}
#check one or more variables, return the first non-null, and raise error if they are all null.
requirearg=function(...,msg){
  if(missing(msg)){ stop('func_requireinput requires the msg input; you need to provide the msg as a named argument.'); }
  for(x in c(...)){ if(!is.null(x)){ return(x); } }
  stop(msg);
}

#Use fprintf, but convert any non-numeric, non-text argument to yaml
sprintfyaml=function(...){
  args=list(...);
  for(i in 1:length(args)){
    arg=args[[i]];
    if(is.null(arg)){ args[[i]]='NULL'; }
    else if(!is.numeric(arg)&&!is.logical(arg)&&!is.character((arg))){ args[[i]]=str_indentlines(yaml::as.yaml(arg)); }
  }
  return(do.call(sprintf,args));
}
#shorthand for cat(sprintf(...))
catf=function(...){ cat(str_ensuresuffix(sprintfyaml(...),'\n')); }
catfif=function(cond,...){ if(cond){cat(str_ensuresuffix(sprintfyaml(...),'\n')); } }
#shorthand for message(sprintf(...))
msgf=function(...){ message(sprintfyaml(...)); }
msgfif=function(cond,...){ if(cond){ message(sprintfyaml(...)); }}


vec_isunique=function(vec){return(!any(duplicated(vec))); }
###############################################################
vec2dataframe=function(v, colnames_){
  ret=as.data.frame(as.list(v));
  colnames(ret)=colnames_;
  return(ret);
}
###############################################################
str2func=function(s){
  #this implementation is probably not necessary. It looks like we can just use f=get(s) instead.
  f=function(...){}
  body(f)=parse(text = paste0('return(',s,'(...));'));
  return(f)
}
###############################################################
#modifyList() is recursive, this list_merge() is not.
list_merge=function(list1,list2,...){
  list1[names(list2)]=list2;
  for(list3 in list(...)){ list1[names(list3)]=list3; }
  return(list1);
}
#this function is for naming convenience. You should call modifyList() directly for efficiency.
list_merge_recursive=function(list1,list2,...){
  return(modifyList(list1,list2,...))
}

#create a list of only of fs fields.
list_selectfields=function(a,fs){
  b=list();
  for(key in intersect(fs,names(a))){
    b[key]=a[key]
  }
  return(b)
}

###############################################################
is.namedlist=function(a){
  return(is.list(a) && !is.null(names(a)));
}
is.unnamedlist=function(a){
  return(is.list(a) && is.null(names(a)));
}
list_indexes=function(a){
  if(is.namedlist(a)){ return(names(a)); }
  else{ return(1:length(a)); }
}
###############################################################
#we consider an empty text to also be empty, whereas r considers it a list of 1 item. e.g., in r length('') gives 1.
isempty=function(v){
  return(length(v)==0 || (is.character(v)&&length(v)==1&&nchar(v)==0));
}
list_issetandnonempty=function(a,f){
  return((f %in% names(a))&&!isempty(a[[f]]));
}
list_isnotsetorempty=function(a,f){
  return(!(f %in% names(a))||isempty(a[[f]]));
}
#remove trailing slash, except if it appears at the root level. e.g., on windows '//' root level has special meaning, so don't remove that.
io_removetrailingslash=function(path){
  if(length(path)>1){ for(i in 1:length(path)){ path[[i]]=io_removetrailingslash(path[[i]]); } }
  else{
    if(path!='/'&&path!='//'&&path!='\\'&&path!='\\\\'){
      path=gsub('[/\\]$','',path);
    }
  }
  return(path);
}

###############################################################
io_name=function(...,fsep='/'){
  #I don't like how file.path() adds an extra '/' if path already ends with '/'. let's do it ourselves.
  #return(file.path(...,fsep=fsep));
  parts=list(...);
  if(length(parts)==0){ stop('At least one argument must be given'); }
  ret=parts[[1]];
  if(length(parts)>=2){ for(i in 2:length(parts)){
    ret=io_removetrailingslash(ret);
    part=parts[[i]];
    if(length(part)>1){
      if(length(ret)!=1 && length(ret)!=length(parts)){ stop('mismatching parts. You may use a single parent and many children files, or same number of parents and their respective children files. ')}
      if(length(ret)==1){ ret=rep(ret,length(part)); }
      for(j in 1:length(ret)){ ret[[j]]=paste0(ret[[j]],fsep,part[[j]]);  }
    }
    else{
      if(length(ret)>1){
        for(j in 1:length(ret)){ ret[[j]]=paste0(ret[[j]],fsep,part);  }
      }
      else{
        ret=paste0(ret,fsep,part);
      }
    }
  }}
  return(ret);
}
#% return true if path is an absolute path 
#% an aboslute path is one that starts with a '/' or that contains ':'
io_isrealpath=function(path){
	return(!isempty(path) && (str_isprefix(path,'/')||str_isprefix(path,'\\') || grepl('^[^/\\]+:',path)) );
}
###############################################################
#% resolve path, getting rid of '../' and './'
#normalizePath only works for existing files/folders. use this when that may not be the case.
io_realpath=function(path){
  if(isempty(path)){ stop('Refusing to resolve an empty path string. If you want to refer to current folder, use "."'); }

  #% if path is not a full path, assume it is in the current directory.'
  if(!io_isrealpath(path)){ path=io_name(getwd(),path); }
  path=str_replace(path,'\\','/');
  #%get rid of ../ and ./
  while(T){
    path2=gsub('/\\./','/',path);
    path2=gsub('/\\.$','',path2);
    path2=gsub('/([^/]*[^\\./:][^/]*)/\\.\\./','/',path2);
    path2=gsub('/([^/]*[^\\./:][^/]*)/\\.\\.$','/',path2);
    if(path==path2){ break; }
    path=path2;
  }
  return(path);
}
###############################################################
#% check whether path is under the folder and return the remaining part of
#% the path. this is a simplified implementation compared to matlab/php.
io_isunder=function(path,infolder){
	if(sys_iswindows()&&!io_isremote(infolder)){
		path=tolower(path); infolder=tolower(infolder);
	}
	path=str_replace(path,'\\','/');
	infolder=str_replace(infolder,'\\','/');
	path=gsub('(.)/+$','\\1',path); #using (.) in case path is a single '/', which we wouldn't want to remove.
	infolder=gsub('(.)/+$','\\1',infolder);
	if(path==infolder){ return(T); }
	if(nchar(path)==0||nchar(infolder)==0){ stop('Unexpected empty path or folder name'); }
	
	path=io_realpath(path);
	infolder=io_realpath(infolder);
	infolder=str_ensuresuffix(infolder,'/');
	return(str_isprefix(path,infolder));
}

###############################################################
#list.files() returns both files and directories and there is no onlydirs=T option!? (terrible)
#using this function to filter the result and only return files (and not the directories)
#we can't use (...), because it messes up named/unnamed arguments so I replicated the input signature of list.files() here.
#I have added a prepend option to prepend the path in the results.
io_listonlyfiles=function(path=".",pattern=NULL,all.files=FALSE,full.names=FALSE,recursive=FALSE,ignore.case=FALSE,include.dirs=FALSE,no..=FALSE,prepend=T){
  files=list.files(path=path,pattern=pattern,all.files=all.files,full.names=full.names,recursive=recursive,ignore.case=ignore.case,include.dirs=include.dirs,no..=no..);
  filepaths=io_name(path,files);
  I=!file.info(filepaths)$isdir;
  if(prepend){ return(filepaths[I]); }
  else{ return(files[I]); }
}
###############################################################
# R's file.exists() give true for both files and directories. we create our own function names to give more expected naming.
io_isfile=function(path){
  return(file.exists(path) & !dir.exists(path));
}
# just a funciton whose name I like better than dir.exists()
io_isdir=function(path){
  return(dir.exists(path));
}
io_pickfirstisfile=function(...,dieifnotfound=F){
  for(f in c(...)){ if(io_isfile(f)){ return(f); }  }
  if(dieifnotfound){ stop(sprintf('None of the file choices could be located.')); }
  return(NULL);
}
io_isfileordir=function(path){
  return(file.exists(path));
}
io_isfileordiroronly=function(path,onlyfile=F,onlydir=F){
  if(onlydir&&onlyfile){ stop('You must provide only one of onlydir or onlyfile.'); }
  return(onlyfile&io_isfile(path) | onlydir&dir.exists(path) | !onlyfile & !onlydir & (file.exists(path)));
}
io_isftp=function(path){
	return(grepl('^ftps?://',tolower(path)));
}
io_issftp=function(path){
	return(grepl('^(ssh|sftp)(\\.\\w+)?://',tolower(path)));
}
io_isremote=function(path){
	return(io_isftp(path)|io_issftp(path)|grepl('^https?://',path));
}

###############################################################
# A helper function to process an input argument and return a list of files.
# This is useful for functions that need an input which can be a single file, a list of files, or one or more folders (whose files are retrieved)
# if a file has a '*' in it, weuse list.files to get the matching files. The pattern must be only in the filename (and not in the directory name/path).
# If a directory is given, pattern can be given to specify which filenames to match.
io_arg2files=function(files=NULL,default='.',recursive=FALSE,pattern=NULL,dieifnotfound=TRUE){
	if(is.null(files)){ files=default; }
	ret=c();
	for(file in files){
		if(dir.exists(file)){ ret=c(ret, io_listonlyfiles(path=file,pattern=pattern,recursive=recursive,prepend=T)); }
		else if(file.exists(file)){ ret=c(ret,file); }
		else if(grepl('\\*',file)){ 
			dir=dirname(file);
			if(grepl('\\*',dir)){ stop(paste0('Only the basename may contain "*". Invalid files input: [',file,']')); }
			pattern=basename(file);
			ret=c(ret, io_listonlyfiles(path=dir,pattern=pattern,recursive=recursive));
		}
	}
	if(length(ret)==0){ stop(paste0('No matching files found for the requested files:\n',str_lined(str_ensureprefix(files,'    ')))); }
	return(ret);
}

###############################################################
io_arg2rfiles=function(files=NULL,default='.',recursive=FALSE,pattern='\\.[rR]$',dieifnotfound=TRUE){
	return(io_arg2files(files=files,default=default,pattern=pattern,recursive=recursive,dieifnotfound=dieifnotfound));
}

###############################################################
amialibrary=function(){
  tryCatch({
      attr(amialibrary,'dummy')<<-T; #this will throw an error "cannot change value of locked binding for 'amiinstalled'" when this code is coming from an installed library.
      return(F);
    },
    error = function(e){ }
  );
  return(T)
}
mypackagename=function(){
  ret=packageName(); #returns the packagename when installed, returns NULL when not installed.
  if(!is.null(ret)){ return(ret); }
  #Another alternative is getPackageName(), but it returns '.GlobalEnv' when not in a package.

  #pkgload::pkg_name() also checks for a DESCRIPTION file in parent folder. (Good to detect a package folder that is not an installed library.)
  installpackageifmissing('pkgload');
  tryCatch({ return(pkgload::pkg_name()); }, error=function(e){});
  return(NULL);

}
###############################################################
#that R doesn't have a documented solution for script file/directory is reason enough to declare it a terrible language.
#this is a hack that may not work for all cases. solution from: https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script
#this.path() seems to also check the currently open Rstudio file. that may or may not be desireable.
#thisfile will not work correctly when this function is part of an installed library. In such cases, thisfile will return the path of the currently open Rstudio file, if any.
thisfile=function(){
    installpackageifmissing('this.path');
  	return(this.path::this.path());
}
thisdir=function(){
  #thisfile() may give an error: "Error in .gui.path() : R is running from RStudio with no documents open (or source document has no path)". Let's use getwd() as fallback.
  file=NULL; tryCatch({return(dirname(thisfile())); },error=function(e){});
  return(getwd());
}
cdhere=function(){ setwd(thisdir()); }
cd=function(dir=NULL){ if(missing(dir)){ cdhere(); }else{ setwd(dir); }; }
###############################################################
#if basedir itself is a relative path, we'll resolve it using here::here().
io_resolvepath=function(path,basedir=NULL){
  if(grepl('^[a-zA-Z]:',path)){ return(path); }
  if(grepl('^/',path)){ return(path); }

  #try both getwd() and here::here() and thisdir(), prefer existing file/folder. if both are absent, prefer here::here().
  if(is.null(basedir)){
    cwdir = getwd();
    installpackageifmissing('here');
    heredir = here::here();
    thisdir = thisfolder();

    cwpath = io_name(cwdir,path);
    herepath = io_name(heredir,path);
    thispath = io_name(thisdir,path);
    paths = c(herepath); pathsources = c('here()'); #pathsources kept for warning() purposes.
    if(!(cwpath %in% paths) ){ paths = c(paths,cwpath); pathsources=c(pathsources,'getwd()');    }
    if(!(thispath %in% paths) ){ paths = c(paths,thispath); pathsources=c(pathsources,'thispath()');    }
    if(length(paths)==1){ return(paths[[1]]); } #they are all the same, no need to check if file/folder exists.

    I = io_isfileordir((paths));
    goodpaths=paths[I];
    goodpathsources=pathsources[I];
    if(length(goodpaths)==0){ return(herepath); } #none of them already exist. return herepath.
    else if(length(goodpaths)>1){
      warning(sprintf('Path [%s] exist under %s:\n%s\nThe first of these locations will be used.', path,str_scsv(goodpathsources),str_lined(str_ensureprefix(goodpaths,'  '))));
    }
    return(goodpaths[[1]]);
  }
  #TODO: we should also consider the possibility that basedir+path can be resolved using cwdir. See util.py implementation.
  if(basedir=='' || basedir=='.' || basedir=='../' || basedir=='..\\'){
    installpackageifmissing('here');
    basedir=here::here(basedir); 
  }
  return(io_name(basedir,path)); 
}
#search for a file (e.g., config.yml) in current folder or in parent folders.
#startingfolder can be a single folder or a list of folders.
#If startingfolder is not given, we use [herepath(), getwd(), thisdir()]
#onlyfirst: determines whether we return the first hit or all hits.
io_searchupward=function(filename,startingfolder=NULL,onlyfile=F,onlydir=F,onlyfirst=T,dieifnotfound=T,dbg=F){
  if(is.null(startingfolder)){
    if(dbg){ print(list('startingfolder options:',here_=here::here(),getwd_=getwd(),thisdir_=thisdir())); }
    startingfolder=unique(c(here::here(),getwd(),thisdir()));
  }
  ret=c();
  folders=startingfolder; #we'll go up, when we hit the top, we'll remove that folder.
  while(length(folders)!=0){
    Ikeepfolders=rep(TRUE,length(folders));
    for(i in seq_along(folders)){
      folder = folders[[i]];
      testpath = io_name(folder,filename);
      if(io_isfileordiroronly(testpath,onlyfile=onlyfile,onlydir=onlydir)){
        if(onlyfirst){ return(testpath); }
        else{ ret=c(ret,testpath); }
      }
      folders[[i]] = dirname(folder);
      Ikeepfolders[[i]]=folders[[i]]!=folder;
    }
    if(!all(Ikeepfolders)){ folders=folders[Ikeepfolders]; }
  }
  if(length(ret)==0&&dieifnotfound){ stop(paste0('Could not locate the file [',filename,']')); }
  return(ret);
}

###############################################################
io_read=function(file){
  #other options are: readBin()
  #suppress: "Warning: truncating string with embedded nuls"
  suppressWarnings({ return(readChar(file, file.info(file)$size, useBytes=T)); });
}
io_readraw=function(file){
  return(readBin(file, 'raw', file.info(file)$size));
}
io_isgrep=function(file,pattern,fixed=T){
  #suppress: "Warning: input string 1 is invalid in this locale"
  #suppressWarnings({ return(grepl(pattern,io_read(file),fixed=fixed)); });
  s=io_readraw(file);
  #remove any null characters to avoid the error "embedded nul in string"
  I=s==0;
  if(any(I)){
    s=s[!I];
    s=rawToChar(s);
    s=iconv(s,to='UTF-8',sub='');
  }
  else{
    s=rawToChar(s);
  }
  return(grepl(pattern,s,fixed=fixed));
}
###############################################################
#writeLines sucks, b/c it uses carriage returns on windows, even with sep="\n" argument.
io_write=function(path,text,append=F){
  if(length(path)>1||str_contains(path,"\n")){
    stop("You probably mixed the order of arguments. Provide file path first.");
  }
	if(length(text)>1){
		text=str_lined(text); #add a trailing newline if one is absent.
		text=str_ensuresuffix(text,"\n");
	}
	mode=ifelse(append,'ab','wb');
	f=file(path,mode);
	cat(text,file=f);
	close(f);
}


###############################################################
#https://conjugateprior.org/2015/06/identifying-the-os-from-r/
#erturn windows|osx|linux
sys_os = function(){
  sysinf = Sys.info()
  if (!is.null(sysinf)){
    os = sysinf['sysname']
    if (os == 'Darwin') os = "osx"
  }
  else {
    os = .Platform$OS.type
    if (grepl("^darwin", R.version$os)) os = "osx"
    else if (grepl("linux-gnu", R.version$os)) os = "linux"
  }
  return(tolower(os))
}
sys_iswindows=function(){
  return(sys_os()=='windows');
}
sys_ismac=function(){
  return(sys_os()=='osx');
}
sys_userhomedir=function(){
  return(fs::path_home());
}
###############################################################
sys_open=function(file){
  if(grepl('"',file,fixed=T)) stop('File should not contain a quote character.');
  if(sys_iswindows())   cmd=paste0('open "',file,'"')
  else if(sys_ismac()) cmd=paste0('open "',file,'"')
  else cmd=paste0('xdg-open "',file,'"')
  cat(sprintf("running command [ %s ] ...\n",cmd))
  system(cmd,wait=F);
}

###############################################################
xls_write=function(file,d,open=F,...){
  installpackageifmissing('openxlsx');
  if(file.exists(file)){
    wb=openxlsx::loadWorkbook(file);
    openxlsx::writeData(wb, sheet=1,d, ...)
    openxlsx::saveWorkbook(wb, file, overwrite=T)
  }
  else  openxlsx::write.xlsx(d,file,...);
  if(open) sys_open(file);
}



###############################################################
hasnegative=function(v){
  return(any(v<0 & !is.na(v)));
}

###############################################################
str_isprefix=function(s,prefix){ return(startsWith(s,prefix)); } #I just like my function names better.
str_issuffix=function(s,prefix){ return(endssWith(s,prefix)); }

ensureprefix=function(s,prefix){
  if(!startsWith(s,prefix)){ s=paste0(prefix,s); }
  return(s);
}
ensuresuffix=function(s,suffix){
  if(!endsWith(s,suffix)){ s=paste0(s,suffix); }
  return(s);
}
strs_withprefix=function(ss,prefix){
  if(!length(ss)){ return(ss); }
  return(paste0(prefix,ss));
}
str_removeprefix=function(s,prefix){
 if(length(s)>1){ for(i in 1:length(s)){ s[[i]]=str_removeprefix(s[[i]],suffix); }; return(s); }
 if(startsWith(s,prefix)){  s=substr(s,nchar(prefix)+1,nchar(s)); }
 return(s);
}
str_removesuffix=function(s,suffix){
 if(length(s)>1){ for(i in 1:length(s)){ s[[i]]=str_removesuffix(s[[i]],suffix); }; return(s); }
 if(endsWith(s,suffix)){  s=substr(s,1,nchar(s)-nchar(suffix)); }
 return(s);
}
str_replace=function(s,find,rep){
  stringr::str_replace(s,stringr::fixed(find),rep);
}
str_indentlines=function(ss,indent='  '){
  for(i in 1:length(ss)){
    s=ss[[i]];
    s=paste0(indent,s);
    s=gsub('\n',paste0('\n  '),s,fixed=T);
    ss[[i]]=s;
  }
  return(ss);
}
###############################################################
#perform exact or prefix-matched indexing. If prefix-matched, the prefix is required to have '_'.
#returns list(val=>...,found=>...,var=>...)
fieldname_match=function(haystack,needle){
  if(needle %in% haystack){
		return(needle);
	}
  
  I=unlist(lapply(haystack, function(f) startsWith(needle, ensuresuffix(f,'_')) ))
  #use the longest matching subscript if there are multiple hits
  nnz=sum(I)
  if(nnz>1){
    matchhaystack = haystack[I];
    I[-which.max(nchar(matchhaystack))]=F; #only keep the field with maximum length.
    nnz=sum(I); #this should now always be 1.
  }
  if(nnz==1){
    return(haystack[I]);    
  }
  else if(nnz>1){ #this should never happen, because we now use the longest match above.
    warning(paste0('There are multiple hits for needle [',needle, '] in haystack. Returning NULL.'));
  }
  return(NULL);  
}



###############################################################
data_Icols=function(d,cols=NULL){
  if(is.null(cols)){
    if(ncol(d)==0){
      warning('Empty data matrix, hence no numerical columns.\n');
      return(c());
    }
    return(1:ncol(d));
  }
  else if(!length(cols)){ return(as.numeric(cols)); }
  else if(is.logical(cols)){ return(which(cols)); }
  else if(is.character(cols)&&length(cols)==1 && cols=='__NUMERIC__'){
    if(!ncol(d)){ return(c()); }
    return(which(unlist(lapply(d, is.numeric))));
  }
  else if(is.character(cols)){
    out=integer(0);
    for(i in 1:length(cols)){
      col=cols[i];
      ind=which(colnames(d)==col);
      if(length(ind)==0){ #allow prefix-selection, but only if prefix is followed by '_'
        ind=which(unlist(lapply(colnames(d), function(f) startsWith(f, ensuresuffix(col,'_')) )))
      }
      else if(length(ind)>1){ warning(paste0('Multiple column names match [',col,'].\n')); }
      if(length(ind)==0){
        stop(paste0('Cannot find column name [',col,'].'));
      }
      #out[i]=ind[1];
      #out=c(out,ind[-1]);
      out=c(out,ind)
    }
    return(out);
  }
  return(cols);
}

###############################################################
askyesorstop=function(prompt='Continue?'){
  #readline() does not work when we run a block in Rstudio (where it considers the code to run in non-interactive mode and input is automatically taken as "").
  #ans=readline(paste0(prompt,'  (y/n) ')); ans=grepl('^(y|yes)$',ans,ignore.case=T);
  ans=askYesNo(prompt);
  if(is.na(ans)||!ans){ stop('--- ERROR: Exiting since you did not select yes.'); }
  return(ans);
}  
###############################################################
str_ensureprefix=function(s,pre){
  if(length(s)==1){
    if(!startsWith(s,pre)){ s=paste0(pre,s); }
  }
  else{
    for(i in seq_along(s)){
      if(!startsWith(s[[i]],pre)){ s[[i]]=paste0(pre,s[[i]]); }
    }
  }
  return(s);
}
###############################################################
str_ensuresuffix=function(s,pre){
  if(length(s)==1){
    if(!endsWith(s,pre)){ s=paste0(s,pre); }
  }
  else{
    for(i in seq_along(s)){
      if(!startsWith(s[[i]],pre)){ s[[i]]=paste0(s[[i]],pre); }
    }
  }
  return(s);
}
###############################################################
str_ensureeol=function(s){
  if(isempty(s)){ return(s); }
  return(str_ensuresuffix(s,"\n"));
}
###############################################################
istext=function(s){
  return(is.character(s) && length(s)==1);
}
str_contains=function(s,needle){ #just a more intuitive funciton name than grepl().
  return(grepl(needle,s,fixed=T));
}
###############################################################
#if s is a single text containing comma(s), split into list.
iscsvtext=function(s){
  return(is.character(s) && length(s)==1&&grepl(',',s,fixed=T));
}
###############################################################
#convert csv text to a list.
ensurecsvlist=function(s, alloweditems=NULL, trim=F){
  if(length(s)==1 && s=='') return(c());
  if(iscsvtext(s)){
    s=unlist(strsplit(s,','));
    if(trim){ s=trimws(s); }
  }
  if(!is.null(alloweditems)){
    #Check for any invalid/unsupported alloweditems
    notfound=setdiff(s,alloweditems)
    if(length(notfound) > 0 ){
      stop(paste0('Invalid type: ["', paste(notfound, collapse = ", "), '"',']'))
    }
  }
  return(s);
}
###############################################################
#convert csv to list
arr_csv=function(s,trim=F){
  if(length(s)>1){ return(s); } #if ss is already a list of multiple texts, no changes are made (trimming is not done).
  return(ensurecsvlist(s,trim=trim));
}
#convert list to csv text
str_csv=function(ss){
  if(length(ss)==1) return(ss); #if ss is already a single text, no changes needed.
  return(paste(ss,collapse=','));
}
#convert list to spaced csv text
str_scsv=function(ss){
  if(length(ss)==1) return(ss); #if ss is already a single text, no changes needed.
  return(paste(ss,collapse=', '));
}
str_lined=function(ss){
  if(length(ss)==1) return(ss); #if ss is already a single text, no changes needed.
  return(paste(ss,collapse='\n'));
}

###############################################################
installpackageifmissing = function( package ) {
 package=ensurecsvlist(package);
 if(length(package)>1){
    for(pack in package){
       installpackageifmissing(pack);
    }
 }
 else{
    package = package[!(package %in% installed.packages()[,"Package"])]
    if(length(package)){ 
      message(cat('Installing ',package,'...'))
      install.packages(package,dependencies = TRUE,repos = "http://cran.us.r-project.org")
    }
    #lapply(package, require, character.only=T)
 }
}
###############################################################
installpackageifmissing_bioc = function( package ) {
 installpackageifmissing('BiocManager')
 package = package[!(package %in% installed.packages()[,"Package"])]
 if(length(package)){ 
   message(cat('Installing ',package,'...'))
   BiocManager::install(package,ask=FALSE)
 }
 #lapply(package, require, character.only=T)
}

###############################################################
installpackageifmissing_github = function( repo, package=NULL ) {
 if(is.null(package)){
  package=basename(repo)
 }
 I=!(package %in% installed.packages()[,"Package"]);
 package = package[I]
 repo = repo[I]
 if(length(repo)){
   installpackageifmissing('devtools')
   message(cat('Installing ',package,'...'))
   for(i in 1:length(repo)){
     devtools::install_github(repo[i],quiet=TRUE,dependencies=TRUE,upgrade_dependencies=TRUE)
   }
 }
 #lapply(package, require, character.only=T)
}

###############################################################

sys_computername = function(){
  installpackageifmissing('stringr')
  ret=unname(unlist(Sys.info()['nodename']));
  ret=stringr::str_replace(ret,'\\..*','');
  return(tolower(ret));
}

sys_issacan = function(){
  return(str_isprefix(sys_computername(),'sacan'));
}

###############################################################
fundependencygraph=function(funcname,env=1){
  installpackageifmissing('plyr')
  installpackageifmissing_github('datastorm-open/DependenciesGraphs')
  #TODO: We may need to load library(plyr) for some strange package-loading-order requirement. Check back later.
  #library(plyr,quietly = TRUE)
  #library(DependenciesGraphs)
  plot(DependenciesGraphs::funDependencies(env,funcname))
}


###############################################################
# apply function to items in a list and collect the results as a list.
list_map_parallel=function(a, fun, numcores=NULL, ...){
  #TODO: Can we comment out library(foreach) ??
  library(foreach)
  #library(doParallel)
  #print(paste0('util.r line 312 ', numcores))

  if(is.null(numcores)){numcores = min(parallel::detectCores()-2, length(a))};
  if(sys_ismac()){ type='FORK'; }
  else{ type='PSOCK'; }
  mycluster = parallel::makeCluster(numcores,type=type)
  doParallel::registerDoParallel(mycluster)
  inds = list_indexes(a);
  
  out = foreach::foreach(i = inds) %dopar% {
    #print(i)
    fun(a[[i]], ...)
  }
  
  parallel::stopCluster(mycluster)
  return(out);
}

###############################################################
list_map=function(a,fun,doparallel=F,numcores=NULL, trace=1, ...){
  if(!is.null(numcores)){
    if(numcores <= 1 & doparallel){
      warning('Number of Cores is less than 2 and parallel computing will be turned off')
      doparallel=F
    }
  }
  if(doparallel){
    #print(paste0('list_map::335::args ', list(...), numcores))
    return(list_map_parallel(a,fun,numcores,...));
  }
  
  else{
    for(i in list_indexes(a)){
      a[[i]] = fun(a[[i]],...);
    }
    return(a);
  }
}

##############################################################
# get a file path. if filename is not a full path and it does not already exist, return a path in config('datadir').
getdatafile=function(filename){
  if(is.null(filename)){ filename=paste0(config('datadir'),'/', filename); }
  if(!grepl(filename,'/',fixed=T) && !grepl(filename,'\\',fixed=T) && !file.exists(filename)){
    filename=paste0(config('datadir'),filename);
  }
  return(filename);
}

#Update optimized YAML file function
updateoptimizedyaml=function(paramlist,filename=NULL){
  library(yaml)
  yamlfile=getdatafile(filename);
  if(file.exists(yamlfile)){ yaml=read_yaml(yamlfile); }
  else{ yaml=list(); }
  if(is.null(yaml$score) || paramlist$value > yaml$score){
    yaml$colweight=paramlist$weights
    yaml$score=paramlist$value
  }
  write_yaml(yaml, yamlfile)
}
  

###############################################################
#Save log excel file and best weights in YAML file
#by Rawan.
xlsxlogfile=function(r, logfile, ...){
  logfile=getdatafile(logfile);
  library(plyr)
  if(file.exists(logfile)){
      rs=readxl::read_xlsx(logfile)
  }
  else{rs=data.frame()}
  r=as.array(c(r$value, unlist(r$weights)))
  names(r)[1]="score"
  r=as.data.frame(t(r))
  rs=rbind.fill(rs, r)
  writexl::write_xlsx(rs, logfile)
  
  return(rs) 
}


###############################################################
#find and remove opts (and prefixed args) from argv
#opts is array(key=>defaultvalue). non-key'ed strings array(value) are taken as value=>null.
#prefix is eg. "--" that the keys are prefixed by.
#returns [$argv_leftover, $o]
#Example usage: cli_argvparseout(commandArgs(trailingOnly=T))
#Keep skipfirst=FALSE, R does not really include filename in the list (when trailingOnly=T).
cli_argvparseout=function(argv,opts=NULL,prefix='',skipfirst=FALSE){
  o=list();
  if(!is.null(opts)){ o=opts; }
  argv2=c();
  
  i=ifelse(skipfirst,2,1);
  while(i<=length(argv)){ #I use a while loop, b/c we may need to look-ahead and increase i in the loop.
    v=argv[[i]];
    vval=FALSE;
    if(str_contains(v,'=')&&!io_isremote(v)){ #avoid parsing the equal sign in http://a.b/c?d=e
    	toks=stringr::str_split_fixed(v,'=',2);
      v=toks[[1]]; vval=toks[[2]];
			if(vval=='true'){ vval=TRUE }
			else if(vval=='false'){ vval=FALSE; }
			if(endsWith(v,'[]')){
				v=str_removesuffix(v,'[]');
				if(v %in% names(o)){ o[[v]]=c(o[[v]],vval); }
				else{ o[[v]]=vval; }
			}
      else{ o[[v]]=vval; }
    }
    else{
			key=str_removeprefix(v,prefix);
			if((nchar(prefix)&&str_isprefix(v,prefix))||(key %in% names(o))){
				if((key %in% names(o))){ o[[key]]=TRUE; }
	      else{
	      	if(i<length(argv)){ o[[key]] = argv[[i+1]]; i=i+1; }
	      	else{ o[[key]]=FALSE; } #do we want this to be false?! (also update php if modified)
	      }
			}
		  else{
			  argv2=c(argv2,v);
		  }
    }
    i=i+1;
  }
  return(list(argv2,o));
}
###############################################################
#mapnames: if given, we map the unnamed argv's using these names. if a mapname is already provided in o[mapname], we do not consider it for another mapping (the value will be consider for the next mapname or end up in unnamed list).
#unnamedname: after we exhause mapnames, any remaining values will be returned as o[[unnamedname]] list.
cli_argv2opt=function(argv,mapnames=c(),unnamedname='unnamed__',opts=NULL,prefix='',skipfirst=FALSE){
 a=cli_argvparseout(argv,opts,prefix,skipfirst);
 args=a[[1]]; o=a[[2]];
 mapnames=setdiff(mapnames,names(o));
 n=min(length(args),length(mapnames));
 if(n>0){ for(i in 1:n){
 	o[[mapnames[[i]]]] = args[[i]];
 }}
 if(length(args)>length(mapnames)){
 	o[[unnamedname]] = args[length(mapnames)+1:length(args)];
 }
 return(o);
}


###############################################################
#find an executable in PATH.
#similar to bmes.locateexe()
exec_locateexe=function(exe,morefolders=c(),dieifnotfound=T){
	if(str_contains(exe,'/')||str_contains(exe,'\\')){ return(exe); }
	
	out='';
	whichcmd=ifelse(sys_iswindows(),'where','which');
	#we are now using system2, so there is no need to escape
	#if(str_contains(exe,'"')){ stop(sprintf('exe name containing double-quote character not allowed: [%s]',exe)); }
	#out=system(sprintf('%s "%s"',whichcmd,exe),intern=T);
	suppressWarnings({ tryfile=system2(whichcmd,shQuote(exe),stdout=T); }); status=attr(tryfile,'status');
	if((is.null(status)||status==0) && !isempty(tryfile)){
		#system2() return lined results (and may have multiple lines in Windows for multiple hits). keep the first one.
		tryfile=tryfile[1];
		out=stringr::str_trim(tryfile);
		#%windows may return multiple hits. only keep the first one.
		#out=regexprep(out,'\r?\n.*','');
	}
	if(isempty(out)&&!isempty(morefolders)){
		for(mi in 1:length(morefolders)){
			folder=morefolders[[mi]];
			if(io_isfile(io_name(folder,exe))){ out=io_name(folder,exe); break; }
			if(sys_iswindows()&&io_isfile(io_name(folder, paste0(exe,'.exe')))){ out=io_name(folder, paste0(exe,'.exe')); break; }
		}
	}
	if(isempty(out)&&dieifnotfound){
		stop(sprintf('Cannot locate executable [%s] on system path. You need to add the folder containing that executable to your system PATH.',exe));
	}
	return(out);
}
###############################################################
io_mkdirif=function(dir){
  if(!io_isdir(dir)){
    if(io_isfile(dir)){ stop(sprintf('Cannot create directory [%s], b/c it already exists as a file.',dir)); }
    dir.create(dir);
  }
}
###############################################################
io_cygpath=function(path){
	path=str_replace(path,'\\','/');
	if(str_isprefix(path,'/')){ return(path); }
	#%if str_isprefix(sys_computername,'sacan'); path=regexprep(path,'^([a-zA-Z]):','/$1'); end	
	#if(str_contains(path,'"')){ stop(sprintf('path containing double-quote character not allowed: [%s]',path)); }
	#path=system(sprintf('cygpath "%s"',path), intern=T);
  cygpathexe=exec_locateexe('cygpath',morefolders='c:/cygwin/bin');
	path=system2(cygpathexe,shQuote(path),stdout=T);
	path=str_removesuffix(path,"\n");
	return(path);
}
exec_iscygexe=function(exe){
	return( sys_iswindows()&&(io_isunder(exe,'C:/cygwin/')||io_isgrep(exe,'cygwin1.dll')) ); #%are there other cygwinXXX.dll's that we need to worry about?
}
#% if exe is a cygwin program, then use io_cygpath for the path.
io_cygpathif=function(path,exe){
 if(exec_iscygexe(exe)){ path=io_cygpath(path); }
	return(path);
}
#rsync complains about windows paths, thinking they are remote. this is a hack for it. It works for cygwin-rsync and R-rsync (the rsync that is in RStudio).
io_rsyncpath=function(path){
  path=gsub('\\','/',path,fixed=T);
  if(sys_iswindows()&&grepl('^[a-zA-Z]:/',path)){
    path=gsub('^([a-zA-Z]):/',sprintf('//%s/\\1$/',sys_computername()),path);
  }
  return(path);
}
#when include is given (e.g., '*.r'), we only transfer those files. exclusions are applied within those includes.
io_rsync=function(src,dest,include=c(),exclude=c(),dryrun=F,quiet=F){
  args=c('-auvzm');
  if(dryrun){ args=c(args,'--dry-run'); }
  if(dir.exists(src)){ src=str_ensuresuffix(src,'/'); dest=str_ensuresuffix(dest,'/'); }
  if(dir.exists(dest)){ dest=str_ensuresuffix(dest,'/'); }

  excludefromtempfile=NULL
  if(length(exclude)>3){ #when too many excludes, create an exclude-from file for them.
    excludefromtempfile=tempfile(fileext='.txt');
    io_write(excludefromtempfile,exclude);
    args=c(args,'--exclude-from',excludefromtempfile);
  }
  else if(length(exclude)>0){ for(i in 1:length(exclude)){
    args=c(args,'--exclude',exclude[i]);
  }}
  if(!isempty(include)){
    args=c(args,'--include','*/');
    for(i in 1:length(include)){ args=c(args,'--include',include[i]); }
    args=c(args,'--exclude', '*');
  }

  rsyncexe='rsync';
  #on ahmet's laptop, RStudio-rsync is misbehaving.. let's give rsync a try.
  if(io_isfile('C:/cygwin/bin/rsync.exe')){ rsyncexe='C:/cygwin/bin/rsync.exe'; }

  #src=io_rsyncpath(src); dest=io_rsyncpath(dest);
  src=io_cygpathif(src,rsyncexe); dest=io_cygpathif(dest,rsyncexe);
  args=c(args,src,dest);

  if(!quiet){
    cat(sprintf('Running rsync command:\n'));
    cat(paste(c('  ',rsyncexe,args),collapse=' ')); cat('\n');
  }
	#s=system2(rsyncexe,shQuote(args),stdout=!quiet,stderr=!quiet);
  s=system2(rsyncexe,shQuote(args),stdout=T,stderr=T); #for some bizarre reason, unless we capture stdout/stderr, rsync doesn't actually work. So, we need to get output even if we don't display it.
  if(!quiet){ cat(str_lined(s)); }
  if(!isempty(excludefromtempfile)){ file.remove(excludefromtempfile); }
}