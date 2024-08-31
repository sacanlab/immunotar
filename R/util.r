#General utility functions
# Copyright (C) 2022,2023 by Ahmet Sacan

# R Programming Reminders
# is.null() returns true for c(), but false for list(). If a variable could be a list(), you should use isempty() instead.
# Always consider the possibility that the awful prefixed-indexing that R has may create a problem. a$b will retrieve any key that starts with 'b'. a[['b']] is safer.
# In a .r file, only use source('...') inside functions, (e.g., not at the top of a .r file). Source'ing outside the functions may cause infinite recursion. This does not apply to rmd files.
# Parallel computing does not provide useful error messages; to see where an error happened, you need to turn parallel off.
# a$b=NULL or a[['b']]=NULL ends up removing the key b (ugh!). If you want to keep b, use: a['b']=list(NULL).
# Rstudio debugging works only if a file is loaded using debugSource(). If you have internal source('..') loads of the same file, debugging of that file will no longer work. You can wrap such source('...') calls with if(!exists(...)) to workaround this problem.
# When selecting from a data frame d[I,] is dangerous; the result is unpredictable. Could be a data frame if I is multiple rows, or a vector (no longer a dataframe) if I is a single row. Always use d[I,,drop=F] when indexing a data.frame(). (Same with column selection.)

###############################################################
#stack is a list of items [[functionname, arg1, arg2, etc.], [functionname, arg1, arg2, etc.], ...]
#note that if you use print(dbg_trace(...)), the print() function adds another stack layer. Use st=dbg_trace(); print(st) instead.
dbg_backtrace=function(cutlevels=1,withargs=F){
	st=sys.calls();
  cutlevels=max(0,cutlevels); #can't be less than 0.
	if(length(st)<=cutlevels){ st=c(); }
	else{ st=st[1:(length(st)-cutlevels)]; }
  return(st);
}
dbg_nicestack=function(cutlevels=1,withargs=F,cleanup=T){
  st=dbg_backtrace(cutlevels+1);
  if(cleanup){
    st2=list();
    for(f in st){
      if(length(f[[1]])==1){
        if(typeof(f[[1]])=='closure'){ f[[1]]='[closure]'; }
        if(!(as.character(f[[1]]) %in% c('source','eval','withVisible'))){
           st2[[length(st2)+1]]=f; 
        }
      }
    }
    st=st2;
  }
	if(withargs){ return( paste(st,collapse=' -> ')); }
  else{
    ss=c(); for(f in st){ ss=c(ss,f[[1]]); }
    return(paste(ss,collapse= ' -> ' )); 
  }
}
dbg_trace=function(cutlevels=1){
  s=dbg_nicestack(cutlevels+1);
  if(isempty(s)){ s='[empty callstack]'; }
  cat(sprintf('%s\n',s));
  return(invisible(NULL))
}
dbg_caller=function(cutlevels=1){
  s=dbg_backtrace(cutlevels+1);
  if(length(s)<2){ s='[empty callstack]'; }
  else{ s=s[length(s)-1]; }
  return(s)
}

sys_amiclimain=function(cutlevels=1){
  st=dbg_backtrace(cutlevels+1);
  return(!length(st));
}
#check whether a script is being run as the main file in rstudio.
sys_amirstudiomain=function(cutlevels=1){
  st=dbg_backtrace(cutlevels+1);
  return(length(st)==4
  &&length(st[[1]])==3&&st[[1]][[1]]=="source"&&st[[1]][[3]]==TRUE
  &&st[[2]][[1]]=='withVisible'
  &&st[[3]][[1]]=='eval'
  &&st[[4]][[1]]=='eval'
  );
}
sys_amimain=function(cutlevels=1){
  return(sys_amiclimain(cutlevels+1)||sys_amirstudiomain(cutlevels+1));
}
###############################################################
#when we create an R package, we need to disable source() calls. We do that by renaming them to source_disabled__
source_disabled__=function(...){ return(invisible(NULL)); }
###############################################################
#get the value of a variable or a default value (if the variable does not exist)
var_get=function(name,default=NULL){
  if(grepl('^\\.GlobalEnv\\$',name)){ env=globalenv();  name=gsub('^\\.GlobalEnv\\$','',name); }
  else{   env=parent.env(environment());  }
	if(name %in% names(env)){ return(env[[name]]); }
	return(default);
}
###############################################################
# this is a replacement for ifelse(). R"s ifelse() behavior is just dumb. It only works in a reasonable way if the lengths of the iftrue/iffalse are the same.
var_pick=function(condition,iftrue,iffalse){
  if(var_tobool(condition)){ return(iftrue); }
  else{ return(iffalse); }
}
var_pickfirstnotnull=function(...){
  #for loop is not necessary, c(...) already excludes any NULLs.
  #for(v in c(...)){ if(!is.null(v)){ return(v); } }
  #return(c(...)[[1]]); #this doesn't work well. if one of the arguments is a matrix, it only returns the first element of that matrix.
  for(v in list(...)){ if(!is.null(v)){ return(v); } }
}
var_tobool=function(x){
  if(isempty(x)){ return(F); }
  if(length(x)>1){ return(T); }
  if(is.na(x)){ return(F); }
  if(is.numeric(x)){ return(x!=0); }
  if(is.logical(x)){ return(x); }
  if(is.character(x)&&length(x)==1&&(x %in% c('no','off','false','0'))){ return(F); }
  return(T);
}
#return true if a and be are references to the same object
#https://stackoverflow.com/questions/7326865/in-r-how-can-i-check-if-two-variable-names-reference-the-same-underlying-object
var_issameobject=function(a,b){
  if(is.function(a)||is.function(b)){ return(identical(all.equal(a,b),TRUE)); }
  else{ return(tracemem(a)==tracemem(b)); }
}
var_equals=function(a,b){
  return(var_issameobject(a,b)||identical(a,b));
}
#check one or more variables, return the first non-null, and raise error if they are all null.
requirearg=function(...,msg){
  if(missing(msg)){ stop('func_requireinput requires the msg input; you need to provide the msg as a named argument.'); }
  for(x in c(...)){ if(!is.null(x)){ return(x); } }
  stop(msg);
}
forlength=function(a){
  #"for(i in 1:length(a))" does not work when a is empty, b/c 1:0 generates [1,0] (stupid!). This is to fix that without having to write if statements every time we have a for loop.
  if(!length(a)){ return(c()); }
  return(1:length(a));
}
#Use fprintf, but convert any non-numeric, non-text argument to yaml
sprintfyaml=function(...){
  args=list(...);
  if(!length(args)){ return(''); }
  if(length(args)>1&&(is.logical(args[[1]])||is.numeric(args[[1]]))){ warning('Did you forget to call an *if (eg., "stopfif") function instead of the non-if function (e.g, "stopf")\n'); }
  for(i in forlength(args)){
    arg=args[[i]];
    if(is.null(arg)){ args[[i]]='NULL'; }
    else if(is.data.frame(arg)){ args[[i]]=str_indentlines(str_lined(capture.output(arg))); }
    else if(length(arg)!=1 || (!is.numeric(arg)&&!is.logical(arg)&&!is.character((arg)))){ args[[i]]=str_indentlines(yaml::as.yaml(arg)); }
    else if(length(args)>1 && (is.numeric(arg)||is.logical(arg))){ args[[i]]=arg; }
    else if(!is.character((args[[i]]))){ args[[i]]=as.character(args[[i]]); }
  }
  if(length(args)==1){ return(sprintf('%s',args)); }
  return(do.call(sprintf,args));
}
#shorthand for cat(sprintf(...))
catf=function(...){ cat(str_ensureeol(sprintfyaml(...))); }
catfif=function(cond,...){ if(var_tobool(cond)){cat(str_ensureeol(sprintfyaml(...))); } }
#shorthand for message(sprintf(...))
msgf=function(...){ message(sprintfyaml(...)); }
msgfif=function(cond,...){ if(var_tobool(cond)){ message(sprintfyaml(...)); }}
stopf=function(...){  st=dbg_nicestack(2); stop(paste0(sprintfyaml(...),"\n",sprintfyaml('Stack: %s',st))); }
stopfif=function(cond,...){ if(var_tobool(cond)){  st=dbg_nicestack(2); stop(paste0(sprintfyaml(...),"\n",sprintfyaml('Stack: %s',st))); } }
assertf=function(cond,...){ stopfif(!var_tobool(cond),...); }
#warning() prints newline at the end in R, but not in Rstudio chunk. No good way of distinguishing between the two. I add a new line to avoid multiple warnings printing on the same line.
#warnf=function(...){ message(sprintfyaml(...)); }
warnf=function(...){  warning(str_ensureeol(sprintfyaml(...))); }
warnfif=function(cond,...){ if(var_tobool(cond)){ warning(str_ensureeol(sprintfyaml(...))); } }
#warnf=function(s,...){ st=dbg_nicestack(2); warnf(paste0('%s: ',s),st,...); }
#warnfif=function(cond,s,...){ if(var_tobool(cond)){ st=dbg_nicestack(2); warnf(paste0('%s: ',s),st,...); } }
dbg_warnf=function(s,...){ st=dbg_nicestack(2); warnf(paste0('%s: ',s),st,...); }
dbg_warnfif=function(cond,s,...){ if(var_tobool(cond)){ st=dbg_nicestack(2); warnf(paste0('%s: ',s),st,...); } }
dbg_stopif=function(cond=NULL){ if(missing(cond)||var_tobool(cond)){ browser(); } }
dbg_stop=function(){ browser(); }


arr_isunique=function(vec){return(!any(duplicated(vec))); }
vec_isunique=function(vec){return(!any(duplicated(vec))); }
arr_unique_inds=function(vec){ return(split(seq_along(vec), vec)); } #returns a list where keys are the unique values and values are the indices where they appear in vec.
#group vec, but use another vec "by" to identify the unique inds. Useful when grouping strs by their prefixes, e.g., identifying column groups in a data frame, where vec would be the column names and by would be the column group names.
arr_groupby=function(vec,by){
  ret = arr_unique_inds(by);
  for(i in forlength(ret)){
    ret[[i]]=vec[ ret[[i]] ];
  }
  return(ret);
}
###############################################################
absmax = function(x) { x[which.max( abs(x))]}
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
###############################################################
#merge a value from list2 only if it is not-null
list_mergenonnull=function(list1,list2){
  for(f in names(list2)){
    v=list2[[f]];
    if(!is.null(v)){ list1[[f]]=v; }
  }
  return(list1);
}
###############################################################
#merge a value from list2 only if it is not-null
list_mergenonempty=function(list1,list2){
  for(f in names(list2)){
    v=list2[[f]];
    if(!isempty(v)){ list1[[f]]=v; }
  }
  return(list1);
}
###############################################################

fst2xlsx=function(fstfile,xlfile=NULL){
  if(is.null(xlfile)){ xlfile=paste0(fstfile,'.','xlsx'); }
  data_writefile(data_readfile(fstfile,rowNames=T),xlfile,rowNames=T);
#TODO: rowNames are not written. Check why. e.g., fst2xlsx('C:/Users/ahmet/AppData/Local/cache/pancancer.Acute Myeloid Leuke.9a40415f501d43182b1db271090834.fst') 
  return(xlfile);
}

#this function is for naming convenience. You should call modifyList() directly for efficiency.
list_merge_recursive=function(list1,list2,...){
  return(modifyList(list1,list2,...))
}
list_removefields=function(a,f,...){
  if(isempty(a)){ return(a); }
  fs=unlist(f);
  if(length(c(...))!=0){ fs=c(fs,...); }
  fs=base::intersect(fs,names(a));
  if(isempty(fs)){ return(a); }
  return(within(a,rm(list=fs)));
}
#provide fromto as a list or from,to arg pairs.
list_renamefields=function(a,fromto=NULL,...){
  if(isempty(a)){ return(a); }
  if(length(c(...))!=0){
    pairs=c(fromto,...);
    froms=pairs[seq(1,length(pairs),2)];
    tos=pairs[seq(2,length(pairs),2)];
  }
  else{
    froms=names(fromto);
    tos=unlist(fromto);
  }
  I=match(froms,names(a));
  J=is.na(I);
  if(any(J)){ I=I[!J]; froms=froms[!J]; tos=tos[!J]; }
  names(a)[I]=tos;
  return(a);
}
#add a prefix to each name
list_withprefixnames=function(a,pre){
  return(list_renamefields(a,list_make(names(a),strs_withprefix(names(a),pre))));
}
list_remove=function(a,v,...){
  if(isempty(a)){ return(a); }
  vs=unlist(v);
  if(length(c(...))!=0){ vs=c(vs,...); }
  if(isempty(vs)){ return(a); }
  return(a[!(a %in% v)]);
}
#fs defaults to all variables (and not functions) in the environment
list_fromenvironment=function(env=NULL,fs=NULL){
	if(is.null(env)){ env=parent.env(environment()); }
	if(is.null(fs)){ fs=base::setdiff(ls(envir=env), lsf.str(envir=env)); }
	r=list();
	for(f in fs){
		r[[f]]=env[[f]];
	}
	return(r);
}
#create a list of only of fs fields.
list_selectfields=function(a,fs){
  b=list();
  for(key in intersect(fs,names(a))){
    b[key]=a[key]
  }
  return(b)
}
lists_selectfields=function(aa,fs){
  if(!length(aa)){ return(aa); }
  for(i in 1:length(aa)){
    aa[[i]]=list_selectfields(aa[[i]],fs);
  }
  return(aa)
}
lists_extractfield=function(aa,f){
	if(is.null(aa)){ return(NULL); }
	else if(!length(aa)){ return(list()); }
  b=list()
  for(i in 1:length(aa)){
    b[[i]]=aa[[i]][[f]];
  }
  return(b)
}
lists_todataframe=function(aa){
  if(!length(aa)){return(as.data.frame(c())); }
  for(i in 1:length(aa)){
    if(!is.list(aa[[i]])){ aa[[i]]=as.list(aa[[i]]); }
  }
  d=as.data.frame(aa[[1]],checkNames=F);
  if(length(aa)==1){ return(d); }
  for(i in 2:length(aa)){ d=plyr::rbind.fill(d, as.data.frame(aa[[i]],checkNames=F)); }
  return(d);
}
data_colmergeascombinations=function(p,q){
  p=as.data.frame(p,checkNames=F)
  q=as.data.frame(q,checkNames=F)
  rs=data.frame()
  for(i in 1:nrow(p)){
    r=as.list(p[i,,drop=F]);
    for(j in 1:nrow(q)){
      r=list_merge(r,as.list(q[j,,drop=F]))
      rs=plyr::rbind.fill(rs,as.data.frame(r,checkNames=F));
    }
  }
  return(rs);
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
#Just to help translation from matlab. for text, we return the number of elements.
numel=function(v){
  if(length(v)==0){ return(0); }
  if(is.data.frame(v)){ return(nrow(v)*ncol(v)); }
  if(length(v)>1){ return(length(v)); }
  if(is.character(v)){ return(nchar(v)); }
  return(length(v));
}
#we consider an empty text to also be empty, whereas r considers it a list of 1 item. e.g., in r length('') gives 1.
isempty=function(v){
  return(length(v)==0 || (is.character(v)&&length(v)==1&&!is.na(v)&&nchar(v)==0));
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

list_make=function(keys,values){
  names(values)=keys;
  return(values);
}
###############################################################
#%removes .ext from path
io_filenoext=function(path){
if(length(path)>1){ for(i in 1:length(path)){ path[[i]]=io_filenoext(path[[i]]); }; return(path); }
#%don't remove .ext if removing it would leave empty string, '.', or '..'
Ikeep=grepl('^\\.\\.?[^\\.]+$',basename(path));
ret=str_removesuffix(path, paste0('.',io_filesmartext(path)));
if(any(Ikeep)){ ret[Ikeep]=path[Ikeep]; }
return(ret);
}
io_filenamenoext=function(path){ return(io_filenoext(basename(path))); }

#% replace unallowed filename characters with rep
#% maxlen is not as intricate as the php implementation.
io_sanitizefilename=function(file,rep='.',maxlen=64){
fileorig=file; #%if we end up needing md5(), we'll md5 the original file rather than the replaced version, to maintain maximum info.
#%if you change the default rep, change also in exec_sanitizefilename()
file=regexprep(file,'[^a-zA-Z0-9_\\-\\. ]',rep);
if(file=='.'){ file='_'; }
else if(file=='..'){ file='__'; }
else{ file=regexprep(file,'\\.\\.+','.'); }

if(nchar(file)>maxlen){
	ext=io_filesmartext(file);
	if(nchar(ext)>5 && nchar(ext)>floor(maxlen/2)){	ext=substr(ext,1,floor(maxlen/2)); }
	filenoext=io_filenamenoext(file);
	lenfilenoext=maxlen-nchar(ext)-2;
	#%we keep the first half of the filename, to make it recognizable.
	part1=substr(filenoext,1,floor(lenfilenoext/2));
	part2=str_md5(fileorig);
	part2=substr(part2,1,min(lenfilenoext-nchar(part1),nchar(part2)));
	file=paste0(part1,'.',part2);
	if(!isempty(ext)&&nchar(ext)<10){ file=paste0(file,'.',ext); }
}
return(file);
}
#%#return true if a secure name (stays within directory)
io_issecurefilename=function(file,die=T){
  ret=TRUE;
  if(!is.character(file)||length(file)!=1){ ret=FALSE; }
  else if(file=='.'||file=='..'){ ret=FALSE; }
  else if(grepl('[:/\\\\\\*]',file)){ ret=FALSE; }
  if(!ret&&die){
    stopf('security breach: illegal filename [%s]',file);
  }
  return(ret);
}
#% return file extension, without the dot.
#% considers .tar.gz as a single extension.
io_filesmartext=function(filename){
	if(length(filename)>1){
		for(fi in 1:length(filename)){ filename[[fi]]=io_filesmartext(filename[[fi]]); }
		return(filename);
	}
  ext=regexp_tokeni(filename,'(?:[^\\\\/:])\\.(tar\\.(?:gz|bz2|bz))$');
  if(!isempty(ext)){return(ext); }
  ext=regexp_token(filename,'(?:[^\\\\/:])\\.([^\\.\\\\/:]+)$');
  if(!isempty(ext)){return(ext); }
  return('');
}
io_fileext=function(file){
  ext=tools::file_ext(file);
  #ext=gsub('^\\.+','',ext); #not necessary. file_ext() does not return with a period.
  return(ext);
}
io_isfileext=function(file,exts){
  fileext=io_fileext(file);
  for(ext in exts){
    if(isempty(ext)&&isempty(fileext)){ return(TRUE); }
    ext=str_ensureprefix(ext,'.');
    if(str_issuffixi(file,ext)){ return(TRUE); }
  }
  return(FALSE);
}
io_changefileext=function(path,newext){
  filenoext=io_filenoext(path);
  if(!isempty(newext)){ newext=str_ensureprefix(newext,'.') }
  path=paste0(filenoext,newext);
  return(path);
}
#% add filename prefix
# sanitizing may avoid e.g., undesired '..' within filename.
io_addfilenameprefix=function(path,prefix,sanitize=F){
filename=paste0(prefix,basename(path));
if(sanitize){ filename=io_sanitizefilename(filename); }
ret=io_name(dirname(path),filename);
return(ret);
}

#% add filename suffix (preserving the file extension).
io_addfilenamesuffix=function(path,suffix,sanitize=T){
filenoext=io_filenoext(path);
ext=io_filesmartext(path);
path=paste0(filenoext,suffix);
if(!isempty(ext)){ path=paste0(path,'.',ext);}
return(path);
}
str_md5=function(s){
  if(!is.character(s)||length(s)>1){ s=serialize(s,connection=NULL); }
  return(digest::digest(s, algo="md5", serialize=F));
}

io_name=function(...,fsep='/'){
  #I don't like how file.path() adds an extra '/' if path already ends with '/'. let's do it ourselves.
  #return(file.path(...,fsep=fsep));
  parts=list(...);
  if(length(parts)==0){ stop('At least one argument must be given'); }
  ret=parts[[1]];
  if(length(parts)>=2){ for(i in 2:length(parts)){
    ret=io_removetrailingslash(ret);
    part=parts[[i]];
    part=gsub('^[/\\]+','',part);

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
#on mac or unix, we also use ~ as a possible starting character.
io_isrealpath=function(path){
	return(!isempty(path) && ((!sys_iswindows()&&str_isprefix(path,'~')) || str_isprefix(path,'/')||str_isprefix(path,'\\') || grepl('^[^/\\]+:',path)) );
}

###############################################################
#% resolve path, getting rid of '../' and './'
#normalizePath only works for existing files/folders. use this when that may not be the case.
io_realpath=function(path){
  if(isempty(path)){ stop('Refusing to resolve an empty path string. If you want to refer to current folder, use "."'); }

  #% if path is not a full path, assume it is in the current directory.
  if(!io_isrealpath(path)){ path=io_name(getwd(),path); }
  path=str_replace(path,'\\','/');
  path=gsub('//+','/',path); # replace '//' with '/ %this would be a problem if path is a remote url

  #%get rid of ../ and ./
  while(T){
    path2=gsub('/\\./','/',path);
    path2=gsub('/\\.$','',path2);
    path2=gsub('/([^/]*[^\\./:][^/]*)/\\.\\./','/',path2);
    path2=gsub('/([^/]*[^\\./:][^/]*)/\\.\\.$','/',path2);
    if(path==path2){ break; }
    path=path2;
  }
  path=gsub('//+','/',path); # replace '//' with '/ %this would be a problem if path is a remote url
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
  return(!isempty(path) & file.exists(path) & !dir.exists(path));
}
# just a funciton whose name I like better than dir.exists()
io_isdir=function(path){
  return(!isempty(path) & dir.exists(path));
}
io_pickfirstisfile=function(...,dieifnotfound=F){
  for(f in c(...)){ if(io_isfile(f)){ return(f); }  }
  if(dieifnotfound){ stop(sprintf('None of the file choices could be located.')); }
  return(NULL);
}
#' @export
io_isfileordir=function(path){
  return(!isempty(path) & file.exists(path));
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
    error=function(e){ }
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
herehere=function(...){
    installpackageifmissing('here');
    return(here::here(...));
}

###############################################################
#if basedir itself is a relative path, we'll resolve it using here::here().
io_resolvepath=function(path,basedir=NULL){
  if(grepl('^[a-zA-Z]:',path)){ return(path); }
  if(grepl('^/',path)){ return(path); }

  #try both getwd() and here::here() and thisdir(), prefer existing file/folder. if both are absent, prefer here::here().
  if(is.null(basedir)){
    cwdir = getwd();
    installpackageifmissing('here');
    heredir = herehere();
    thisdir = thisdir();

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
      warnf('Path [%s] exist under %s:\n%s\nThe first of these locations will be used.', path,str_scsv(goodpathsources),str_lined(str_ensureprefix(goodpaths,'  ')));
    }
    return(goodpaths[[1]]);
  }
  #TODO: we should also consider the possibility that basedir+path can be resolved using cwdir. See util.py implementation.
  if(basedir=='' || basedir=='.' || basedir=='../' || basedir=='..\\'){
    basedir=herehere(basedir); 
  }
  return(io_name(basedir,path)); 
}
##############################################################
# get a file path. if filename is not a full path and it does not already exist, return a path in config('datadir').
getdatafile=function(filename){
  return(io_which(filename,'{datadir}'));
}

###############################################################
#locate an existing file, making config replacements (e.g., replace '{datadir}' as set in config). look in the package folder too.
#if not found and defaultdir is not given, re return the name unchanged, so the caller would then effectively interpret it to be under cwd.
#if multiple defaultdir's are given, we search each, but use the first when the file doesn't exist.
#defaultdir itself may contain 
#' @export
io_which=function(name,defaultdir=NULL){
  if(!exists('config')){ source_disabled__('config.r'); } 

  if(io_isfileordir(name)){ return(name); }
  if(grepl('{',name,fixed=T)){
    name=config_autofill(name);
    if(io_isfileordir(name)){ return(name); }
  }
  if(io_isrealpath(name)){ return(name); }

  defaultdirorig=defaultdir;
  if(!isempty(defaultdir)){
    for(i in seq_along(defaultdir)){
      defaultdir[[i]]=io_which(defaultdir[[i]]);
      file=io_name(defaultdir[[i]],name);
      if(io_isfileordir(file)){ return(file); }
    }
  }

  #try the data directory
  file=io_name(sys_datadir(),name);
  if(io_isfileordir(file)){ return(file); }


  pkgname=mypackagename();
  if(!isempty(pkgname)){
    #by default, also look in the package's data/ folder. don't even require {datadir} to be part of the requested defaultdirs.
    #if( '{datadir}' %in% defaultdirorig)
    {
      file=system.file(io_name('data',name),package=pkgname)
      if(!isempty(file)){ return(file); }
    }
    file=system.file(name,package=pkgname)
    if(!isempty(file)){ return(file); }
  }

  if(!isempty(defaultdir)){ return(io_name(defaultdir[[1]],name)); }
  return(name);
}

###############################################################
#search for a file (e.g., config.yml) in current folder or in parent folders.
#startingfolder can be a single folder or a list of folders.
#If startingfolder is not given, we use [herepath(), getwd(), thisdir()]
#onlyfirst: determines whether we return the first hit or all hits.
io_searchupward=function(filename,startingfolder=NULL,onlyfile=F,onlydir=F,onlyfirst=T,dieifnotfound=T,dbg=F){
  if(is.null(startingfolder)){
    if(dbg){ print(list('startingfolder options:',here_=herehere(),getwd_=getwd(),thisdir_=thisdir())); }
    startingfolder=unique(c(herehere(),getwd(),thisdir()));
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
io_strcmp=function(a,b){
  if(sys_iswindows()||sys_ismac()){ return(tolower(a)==tolower(b)); }
  else{ return(a==b); }
}
io_issamename=function(f,g){
  if(f==g){ return(TRUE); }
  f=io_realpath(f);
  g=io_realpath(g);
  if(nchar(f)>=2){ f=str_removesuffix(f,'/'); }
  if(nchar(g)>=2){ g=str_removesuffix(g,'/'); }
  return(io_strcmp(f,g));
}
io_read=function(file){
  #other options are: readBin()
  #suppress: "Warning: truncating string with embedded nuls"
  suppressWarnings({ return(readChar(file, file.info(file)$size, useBytes=T)); });
}
io_filesize=function(file){
  ret=file.info(file)$size;
  if(is.na(ret)){ret=0;}
  return(ret);
}
io_isfileandnotempty=function(file){
  return(io_isfile(file)&&var_tobool(io_filesize(file)));
}
io_isnotfileorempty=function(file){
  return(!io_isfile(file)||!var_tobool(io_filesize(file)));
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
hasnegative=function(v){
  return(any(v<0 & !is.na(v)));
}

###############################################################
str_isprefix=function(s,prefix){ return(startsWith(s,prefix)); } #I just like my function names better.
str_isprefixi=function(s,prefix){ return(startsWith(tolower(s),tolower(prefix))); }
str_issuffix=function(s,prefix){ return(endsWith(s,prefix)); }
str_issuffixi=function(s,prefix){ return(endsWith(tolower(s),tolower(prefix))); }

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
str_rand=function(len){
  installpackageifmissing('stringi');
  return(stringi::stri_rand_strings(1,len));
}
regexprep=function(s,find,rep){
  return(gsub(find,rep,s));
}
#returns NULL when not found.
regexp_token=function(s,exp,group=1,insensitive=F){
  if(insensitive){ exp=stringr::regex(exp, ignore_case = T); }
  ret=stringr::str_extract(s,exp,group=group);
  if(length(ret)>1){ return(ret); }
  #str_extract returns NA, but I like NULL better, so the caller can use isempty().
  if(is.na(ret)){ return(NULL); }
  return(ret);
}
regexp_tokeni=function(s,exp,group=1){
  return(regexp_token(s,exp,group,insensitive = T));
}
###############################################################
#translate keys to values using using the mapping of allkeys=>allvalues.
#Example use case: allkeys are uniprotid and allvalues are the corresponding genesymbols. keys are uniprotids that we are trying to translate to their respective genesymbols.
#Warning: if allkeys are not unique, the first found hit is used.
#If keepifnotfound=F, any not-found keys are mapped to notfoundvalue. It's your job to set notfoundvalue to an appropriate data type You pobably don't want to use notfoundvalue=NULL, b/c that will be equivalent to skipping the not-found keys, making the result of this function not line up with the input keys.
map_lookupfromkeysandvalues=function(allkeys,allvalues,keys,keepifnotfound=TRUE,notfoundvalue='',warnifnotunique=T,warnifnotfound=T){
  if(warnifnotunique && !arr_isunique(allkeys)){
    warnf('Duplicate keys detected; if they map to different values, the translation will only result in the first such value.');
  }
  
  #installpackageifmissing_github('nathan-russell/hashmap')
  #map=hashmap::hashmap(allkeys,allvalues);
  #Rawan changed 06/25
  map=stats::setNames(allvalues, allkeys);
  I=map$has_keys(keys);
  if(all(I)){ return(map$find(keys)); }

  #below here, some keys are not found.
  if(warnifnotfound){
    warnf("The following keys could not be found: %s",keys[!I]);
  }
  #I am reusing the keys variable as the returned data (the keys variable becomes the values variable that I return)
  keys[I]=map$find(keys[I]);
  if(!keepifnotfound){
    keys[!I]=notfoundvalue;
  }
  attr(keys,'Inotfound')=which(!I);
  return(keys);
}
###############################################################
#perform exact or prefix-matched indexing. If prefix-matched, the prefix is required to have '_'.
#if multiple prefixes match, use the longest one.
#returns list(val=>...,found=>...,var=>...)
fieldname_match=function(haystack,needle){
	if(isempty(haystack)||isempty(needle)){ return(NULL); }
	if(needle %in% haystack){
		return(needle);
  }
  
  I=unlist(lapply(haystack, function(f) startsWith(needle, ensuresuffix(f,'_')) ))
  #use the longest matching subscript if there are multiple hits
  nnz=sum(I)
  if(nnz==1){
  	return(haystack[I]);
  }
  if(nnz>1){
    matchhaystack = haystack[I];
    match=matchhaystack[[which.max(nchar(matchhaystack))]];
    if(sum(haystack==match)>1){
    	warnf('fieldname_match(): There are multiple hits for needle [%s] in haystack, with match [%s]. Returning NULL.',needle,match);
    	return(NULL);
    }
    return(match);
  }
  return(NULL);  
}


###############################################################
#return the columns that are numeric.
data_numericcols=function(d,na.rm=T){
  if(!ncol(d)){ return(c()); }
  #replace na's with 0; otherwise they are not considered numeric.
  if(na.rm){ d[is.na(d)]=0; }
  return(which(unlist(lapply(d, is.numeric))));
}
###############################################################
data_Icols=function(d,cols=NULL){
  if(is.null(cols)){
    if(ncol(d)==0){
      dbg_warnf('Empty data matrix, hence no numerical columns.');
      return(c());
    }
    return(1:ncol(d));
  }
  else if(!length(cols)){ return(as.numeric(cols)); }
  else if(is.logical(cols)){ return(which(cols)); }
  else if(is.character(cols)&&length(cols)==1 && cols=='__NUMERIC__'){
    return(data_numericcols(d));
  }
  else if(is.character(cols)){
    out=integer(0);
    for(i in 1:length(cols)){
      col=cols[i];
      ind=which(colnames(d)==col);
      if(length(ind)==0){ #allow prefix-selection, but only if prefix is followed by '_'
        ind=which(unlist(lapply(colnames(d), function(f) startsWith(f, ensuresuffix(col,'_')) )))
      }
      else if(length(ind)>1){ warnf('Multiple column names match [%s].',col); }
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
# create an empty data frame.
data_new=function(nrow=NULL,ncol=NULL,rownames=NULL,colnames=NULL){
  if(!is.null(rownames)&&is.null(nrow)){ nrow=length(rownames); }
  if(!is.null(colnames)&&is.null(ncol)){ ncol=length(colnames); }

  if(is.null(nrow)){ nrow=0; }
  if(is.null(ncol)){ ncol=0; }
  d=data.frame(matrix(NA,nrow=nrow,ncol=ncol));
  if(!is.null(rownames)){ rownames(d)=rownames; }
  if(!is.null(colnames)){ colnames(d)=colnames; }
  return(d);
}
###############################################################


data_renamecolumn=function(d,from,to){
  stopifnot(length(from)==1&&length(to)==1);
  I=(colnames(d) %in% from);
  stopfif(!any(I),"Columnname [%s] not present in the data.",from);
  colnames(d)[I]=to;
  return(d)
}
###############################################################

#if tos=NULL, froms must be a list with keys and values.
data_renamecolumns=function(d,fromto){
  for(from in names(fromto)){
    to=fromto[[from]];
    I=(colnames(d) %in% from);
    stopfif(!any(I),"Columnname [%s] not present in the data.",from);
    colnames(d)[I]=to;
  }
  return(d);
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
#return true if it is a single text
istext=function(s){
  return(is.character(s) && length(s)==1);
}
istexts=function(ss){
  if(is.character(ss)){ return(TRUE); }
  if(length(ss)==0){ return(FALSE); }
  for(s in ss){
    if(!is.character(s)){ return(FALSE); }
  }
  return(TRUE);
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
ensurecsvlist=function(s, alloweditems=NULL, trim=F,delim=','){
  if(length(s)==1 && s=='') return(c());
  if(iscsvtext(s)){
    s=unlist(strsplit(s,delim));
    if(trim){ s=trimws(s); }
  }
  if(!is.null(alloweditems)){
    #Check for any invalid/unsupported alloweditems
    notfound=setdiff(s,alloweditems)
    if(length(notfound) > 0 ){
      stop(sprintf('Invalid item(s): [%s]. Allowed items re: [%s].',str_scsv(notfound),str_scsv(alloweditems)));
    }
  }
  return(s);
}
###############################################################
#convert csv to list
arr_csv=function(s,trim=F,delim=','){
  if(length(s)>1){ return(s); } #if ss is already a list of multiple texts, no changes are made (trimming is not done).
  return(ensurecsvlist(s,trim=trim,delim=delim));
}
#same as arr_csv(), just a shorter name. TODO: remove or rename this function. its name is confusing and sounds more like str_csv() than arr_csv().
csv=function(s,trim=F,delim=','){
  if(is.character(trim)){ stop('You must provide a single csv text as the first argument. The second and third arguments are trim=F|T and delim=","')}
  if(length(s)>1){ return(s); } #if ss is already a list of multiple texts, no changes are made (trimming is not done).
  return(ensurecsvlist(s,trim=trim,delim=delim));
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
str_incsv=function(ss,needle){
  return(needle %in% csv(ss));
}
###############################################################
ispackageinstalled=function(package){
  #5x-10x faster than (package %in% installed.packages()[,"Package"])
  return(nchar(system.file(package=package))!=0)
}
###############################################################
ispackagenotinstalled=function(package){
  #5x-10x faster than !(package %in% installed.packages()[,"Package"])
  return(nchar(system.file(package=package))==0)
}
###############################################################
checkpackageinstalled=function(package){
  package=package[unlist(lapply(package,ispackagenotinstalled))];
  if(length(package)){ stop(sprintf('Failed to install packages [ %s ]\n', paste0(package,collapse=', '))); }
}
###############################################################
installpackageifmissing = function( package, checksuccess=T, trybioc=T ) {
if(length(package)==1 && grepl(',',package,fixed=T)){ package=unlist(strsplit(package,',')); }
package = package[unlist(lapply(package,ispackagenotinstalled))]
if(!length(package)){ return(invisible(NULL)); }
message(cat('Installing [ ',paste0(package,collapse=', '),' ] ...'))
install.packages(package,dependencies = TRUE,repos = "http://cran.us.r-project.org")
if(trybioc){
  package=package[unlist(lapply(package,ispackagenotinstalled))];
  if(!length(package)){ return(invisible(NULL)); }
  message(cat('BiocManager: installing [ ',paste0(package,collapse=', '),' ] ...'))
  BiocManager::install(package,ask=FALSE)
}
if(checksuccess){ checkpackageinstalled(package); }
#lapply(package, require, character.only=T)
}
###############################################################
installpackageifmissing_bioc = function( package, checksuccess=T ) {
if(length(package)==1 && grepl(',',package,fixed=T)){ package=unlist(strsplit(package,',')); }
package = package[unlist(lapply(package,ispackagenotinstalled))]
if(!length(package)){ return(invisible(NULL)); }
installpackageifmissing('BiocManager',trybioc=F)
message(cat('BiocManager: installing [ ',paste0(package,collapse=', '),' ] ...'))
BiocManager::install(package,ask=FALSE)
if(checksuccess){ checkpackageinstalled(package); }
}
###############################################################
installpackageifmissing_github = function( repo, package=NULL, checksuccess=T ) {
if(length(repo)==1 && grepl(',',repo,fixed=T)){ repo=unlist(strsplit(repo,',')); }
if(is.null(package)){ package=basename(repo); }
I=unlist(lapply(package,ispackagenotinstalled));
package = package[I]; repo=repo[I];
if(!length(package)){ return(invisible(NULL)); }
installpackageifmissing('devtools')
message(cat('Devtools-github installing [ ',paste0(package,collapse=', '),' ] ...'))
#devtools::install_github(repo,quiet=TRUE,dependencies=TRUE,upgrade_dependencies=TRUE)
devtools::install_github(repo,quiet=TRUE,dependencies=TRUE,upgrade_dependencies=FALSE,upgrade='never')
if(checksuccess){ checkpackageinstalled(package); }
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
sys_issacanlap = function(){
  return(str_isprefix(sys_computername(),'sacanlap'));
}

###############################################################
func_dependencygraph=function(funcname,env=1,doplot=T){
  installpackageifmissing('plyr')
  installpackageifmissing_github('datastorm-open/DependenciesGraphs')
  #TODO: We may need to load library(plyr) for some strange package-loading-order requirement. Check back later.
  #library(magrittr)
  #library(plyr,quietly = TRUE)
  `%>%` = magrittr::`%>%`
  #don't turn this library() off. DependenciesGraphs does not work unless we load the library.
  library(DependenciesGraphs)
  depgraph=DependenciesGraphs::funDependencies(env,funcname);
  if(doplot){ return(plot(depgraph)); }
  else{ return(depgraph); }
}
#excludefuncs can be used to ignore/exclude dependencies when doing recursive. Useful when a dependency is in an if statement that we now will never be needed.
func_depends=function(funcs,recursive=T,excludefuncs=NULL,recache=F){
  ocache=cache_opts(processid='func_depends',dataid=paste0(var_pick(recursive,'R',''),str_md5(list(funcs,excludefuncs))),memcache=T,recache=recache);
  if(!ocache$recache){ return(cache_load(ocache)); }
  if(!recursive){
    installpackageifmissing('functiondepends');
    deps=functiondepends::find_dependencies(funcs);
    ret=unique(deps$Source);
  }
  else{
    deps=c();
    stack=funcs;
    while(length(stack)>0){
      func=stack[[length(stack)]];
      stack=stack[-length(stack)];
      fdeps=func_depends(func,recursive=F,excludefuncs=excludefuncs,recache=recache);
      newdeps=base::setdiff(fdeps,deps);
      deps=c(deps,newdeps);
      newdeps=base::setdiff(newdeps,stack);
      stack=c(stack,newdeps);
    }
    ret=deps;
  }
  if(!is.null(excludefuncs)){ ret=setdiff(ret,csv(excludefuncs)); }
  ret=sort(ret);
  cache_save(ret,ocache);
  return(ret);
}
###############################################################
parallel_amiworker=function(){
  return(!is.null(.GlobalEnv$zoz.parallel.amiworker)&&.GlobalEnv$zoz.parallel.amiworker);
}
###############################################################
#export items in the list a, without changing the variables in our own .GlobalEnv. (we do that by backing up our .GlobalEnv and then restoring it)
#input arguments must be a named list.
parallel_exportvars=function(clust,...){
  vars=opt_set(...);
  backup=c(); absent=c();
  for(name in names(vars)){
    if(name %in% names(.GlobalEnv)){
      backup[[name]]=.GlobalEnv[[name]];
    }
    else{
      absent=c(absent,name);
    }
    .GlobalEnv[[name]]=vars[[name]];
  }
	err=NULL; env=new.env();
  tryCatch({ parallel::clusterExport(clust,names(vars)); }, error=function(e){assign('err',e,env=env);});
  if(length(backup)){ for(name in names(backup)){ .GlobalEnv[[name]]=backup[[name]]; }  }
  if(length(absent)){ rm(list=absent,envir=.GlobalEnv); }
  if(!is.null(err)){ stopf('parallel_startcluster: got the following error when exporting variables to the workers: %s',as.character(err))}
}
###############################################################
#export the lists recorded in zoz.parallel.export
#export variables in the zoz.parallel.export to the cluster. Save them in zoz.parallel.exported list.
parallel_doexports=function(clust){
  #if I am a worker, I will pass on the data I imported from my parent.
  if(parallel_amiworker()&&!is.null(.GlobalEnv[['zoz.parallel.imported']])){
    .GlobalEnv[['zoz.parallel.export']]=list_merge(.GlobalEnv[['zoz.parallel.imported']],.GlobalEnv[['zoz.parallel.export']]);
    rm(zoz.parallel.imported,envir=.GlobalEnv);
  }

  namestoexport=c();
  for(name in names(.GlobalEnv[['zoz.parallel.export']])){
    #this exact object was already exported. skip...
    if(!is.null(.GlobalEnv[['zoz.parallel.exported']][[name]]) && var_issameobject(.GlobalEnv[['zoz.parallel.exported']][[name]],.GlobalEnv[['zoz.parallel.export']][[name]])){
      #msgf('parallel_doexports: Object %s was already exported. Not re-exporting it.',name)
      next;
    }
    if(!(name %in% names(.GlobalEnv))){
      warnf('parallel_doexports: Name %s is requested to be exported or was prevously exported, but it is not available in the .GlobalEnv. Skipping...',name)
      next;
    }
    namestoexport=c(namestoexport,name);
  }
  if(length(namestoexport)>0){
  		parallel::clusterExport(clust,namestoexport);
      .GlobalEnv[['zoz.parallel.exported']]=list_merge(.GlobalEnv[['zoz.parallel.exported']],list_selectfields(.GlobalEnv[['zoz.parallel.export']],namestoexport));

      #if nesting is allowed, tell the workers what they have imported, so they can pass them onto any workers they create.
      if(parallel_allownested()){
        parallel_exportvars(clust,list(zoz.parallel.imported=.GlobalEnv[['zoz.parallel.exported']]));
        #if the worker has already started their own cluster, doexports within worker so it can send the new variables to its subworkers.
        list_map_parallel(1:length(clust),function(...){
          subclust=parallel::getDefaultCluster();
          if(length(subclust)){
            if(!exists('parallel_doexports')){ source_disabled__('util.r');  }
            parallel_doexports(subclust);
          }
        })
      }
  }
  if('zoz.parallel.export' %in% names(.GlobalEnv)){ rm(zoz.parallel.export,envir=.GlobalEnv); }


}
###############################################################
parallel_registerexports=function(names,clust=NULL){
  if(is.null(clust)){  clust=parallel::getDefaultCluster(); }
  changed=FALSE;
  for(name in names){
    if(!(name %in% names(.GlobalEnv))){
      warnf('parallel_doexports: Name %s is requested to be exported but it is not available in the .GlobalEnv. Skipping...')
      next;
    }
    else if(!is.null(.GlobalEnv[['zoz.parallel.export']][[name]]) && var_issameobject(.GlobalEnv[['zoz.parallel.export']][[name]],.GlobalEnv[[name]])){
      warnf('parallel_doexports: Name %s (and the exact same object) was already on the list to be exported.Skipping...',name);
      next;
    }
    .GlobalEnv[['zoz.parallel.export']][[name]]=.GlobalEnv[[name]];
    changed=TRUE;
  }
  if(changed&&!is.null(clust)){
    parallel_doexports(clust);
  }
}
###############################################################
#this function is usually called when we are stopping a cluster or starting a new one.
parallel_backupexported=function(){
  if(is.null(.GlobalEnv[['zoz.parallel.exported']])){ return(invisible(NULL)); }
  #move previously exported variables back into the export list so we can export them when a new cluster is started.
  .GlobalEnv[['zoz.parallel.export']]=list_merge(.GlobalEnv[['zoz.parallel.exported']],.GlobalEnv[['zoz.parallel.export']]);
  .GlobalEnv[['zoz.parallel.exported']]=NULL;
}
###############################################################
#Use backupexports=TRUE when you want them to be reexported next time a global cluster is started.
parallel_stopcluster=function(clust,backupexports=F){
  tryCatch({ parallel::stopCluster(clust); }, error=function(e){msgf('parallel_stopcluster: got the following error when stopping the cluster: %s',as.character(e))} );
  if(backupexports){ parallel_backupexported(); }
}
###############################################################
#get/set a default parallel config variable.
#var is one of maxworkers,maxmemory,allownested.
parallel_config=function(var,value=NULL){
  if(!is.null(value)){
    .GlobalEnv[[paste0('zoz.parallel.',var)]]=value;
    return(value);
  }
  ret=.GlobalEnv[[paste0('zoz.parallel.',var)]];
  if(!is.null(ret)){ return(ret); }
  if(!exists('config')){ source_disabled__('config.r'); } 
  ret=config(paste0('parallel.',var));
  return(ret);
}
###############################################################
#get/set max #of workers available in the system. Defaults to detectcores()-2. You can use config('parallel.maxworkers') to set it.
# we use .GlobalEnv$zoz.parallel.maxworkers to store this. (that value may have been set by a parent node to tell us how many additional workers could be created on this system.).
parallel_maxworkers=function(maxworkers=NULL){
  ret=parallel_config('maxworkers',maxworkers);
  if(is.null(ret)){ ret=parallel::detectCores()-2; }
  return(ret);
}
###############################################################
#Set .GlobalEnv$zoz.parallel.allownested=T to allow the workers to also create clusters of their own. Use a number e.g, zoz.parallel.allownested=2 to set the number of nestings. (zoz.parallel.allownested=1 means false). You may alternatively use config: parallel_allownested. Default is TRUE.
parallel_allownested=function(allownested=NULL){
  ret=parallel_config('allownested',allownested);
  #if(is.null(ret)){ ret=TRUE; } #sacan kept crashing (outofmem?) no matter what we did to limit number of grandchildren. Let's keep it false by default.
  if(is.null(ret)){ ret=FALSE; }
  return(ret);
}
###############################################################
#this is to keep track of total memory we (possibly a child node) are allowed to use.
#if we are a child node, the parent must tell us how much memory we are allocated.
parallel_maxmemory=function(maxmemory=NULL){
  ret=parallel_config('maxmemory',maxmemory);
  stopfif(is.null(ret)&&parallel_amiworker(), 'Missing .GlobalEnv$zoz.parallel.maxmemory variable in this child node. The parent must tell us how much memory I am allocated.');
  if(is.null(ret)){
    installpackageifmissing('memuse');
    r=memuse::Sys.meminfo();
    ret=as.numeric(r$freeram);
  }
  return(ret);
}
###############################################################
#how much memory should each child node be allocated?
parallel_maxchildworkermemory=function(numworkers){
  installpackageifmissing('pryr');
  mymemuse=as.numeric(pryr::mem_used());
  ret=max(floor((parallel_maxmemory()-mymemuse)/numworkers),0);
  return(ret)
}

###############################################################
# What is the maximum number of workers that a child (nested) worker can create?
#numworkers is the number of nodes we are creating/using.
parallel_maxchildworkers=function(numworkers){
  #there's a little inefficiency in using ceil(), but we'll allow it. Better not waste available cores by using floor().
  subworkers = ceiling(parallel_maxworkers()/numworkers); #Don't reserve a spot for the immediate workers (ie., I am not using (maxworkers-numworkers)/numworkers). when the immediate children need work done, they will wait, so they wouldn't be using the cpu cores then. 
  #taking ceiling caused running-out-of-memory 
  #changed to subtract numworkers from maxworkers, to better match memory availability.
  #subworkers = max(floor((parallel_maxworkers()-numworkers)/numworkers),0); 

  installpackageifmissing('pryr');
  mymemuse=as.numeric(pryr::mem_used()); #this is how much memory we use. Assume each of the children will also need this much memory.
  subworkersbymem = max(floor((parallel_maxmemory()-mymemuse)/mymemuse),0);
  #msgf(subworkersbymem);
  return(min(subworkers,subworkersbymem));
}
###############################################################
#changed global=T by default. I think is more advantages to keep a global cluster.
#only for the global cluster we handle the .GlobalEnv$zoz.parallel.export list.
parallel_startcluster=function(numworkers=NULL,numtasks=NULL,global=T,type=NULL,clust=NULL,quiet=F,numcores=NULL){
  warnfif(!is.null(numcores),'numcores argument is absolete. Use numworkers or numtasks instead. If you need to set the maximum #worker nodes, use parallel_maxworkers() to set it, or use config:parallel_maxworkers');

  isnew=FALSE; #will be true if we start a new cluster.
  if(is.null(clust)&&global){  clust=parallel::getDefaultCluster(); }

  if(is.null(numworkers)){
    numworkers=parallel_maxworkers();
    if(!is.null(numtasks)){ numworkers=min(numworkers,numtasks); }
  }

  if(!is.null(clust)){
    if(nrow(showConnections())==0){
      warnfif(!quiet,'parallel_startcluster: A cluster is set, but there are no open connections, which likely means the cluster is stopped. I will attempt to stop this cluster and create a new cluster...');
      parallel_stopcluster(clust,backupexports=T);
      clust=NULL;
    }
    else if(length(clust)<numworkers){
      msgfif(!quiet,'parallel_startcluster: A cluster was previously created with %d cores (which is fewer than the currently requested %d cores). Stopping the old cluster and recreating a new one with %d cores...',length(clust),numworkers,numworkers)
      parallel_stopcluster(clust,backupexports=T);
    }
    else{
      msgfif(!quiet,'parallel_startcluster: A global cluster is already present with numworkers=%d (equal or more than the current request %d cores) Not recreating a new cluster.',length(clust),numworkers);
      attr(clust,'isnew')=FALSE;
      return(clust)
    }
  }

  if(numworkers<1){
    msgfif(!quiet,'numworkers is %d. Refusing to start a new cluster. ',numworkers);
    return(NULL);
  }
  numworkers=max(numworkers,1);

  #return NULL if this is a nested worker and nesting is not allowed.
  if(parallel_amiworker()){
    allownested=parallel_allownested();
    if(allownested<1){
      #this warning message probably never shows up, b/c the parent usually doesn't collect the printout.
      warnfif(!quiet,'Refusing to start a new cluster, because I am already a worker. Use zoz.parallel.  allownested=TRUE if you wish to allow nested parallelism.');
      return(NULL);
    }
  }

  if(is.null(type)){
    if(sys_iswindows()){ type='PSOCK'; }
    else{ type='FORK'; }
  }
  msgfif(!quiet,'Starting a new %s%s cluster with %d cores...',var_pick(global,'global ',''),type,numworkers);
  if(global){ parallel_backupexported(); }
  clust = parallel::makeCluster(numworkers,type=type);
  attr(clust,'isnew')=TRUE;

  #Tell the workers they are workers.
  exportvars=list(zoz.parallel.amiworker=TRUE);

  #If nesting is allowed, tell the workers maximum workers they are allowed to create.
  if(!is.null(.GlobalEnv$zoz.parallel.allownested)){
    allownested=parallel_allownested();
    if(is.numeric(allownested)){allownested=allownested-1;}
    submaxmemory = parallel_maxchildworkermemory(numworkers);
    subworkers=parallel_maxchildworkers(numworkers);
    #TODO: check used and available memory to limit numworkers for children: memuse::Sys.meminfo(), pyry::mem_used()
    if(subworkers<2){ allownested=F; subworkers=0; submaxmemory=0; } #if less than 2 available, don't bother with a nested cluster.
    exportvars[['zoz.parallel.allownested']]=allownested;
    exportvars[['zoz.parallel.maxworkers']]=subworkers;
    exportvars[['zoz.parallel.maxmemory']]=submaxmemory;
  }

  parallel_exportvars(clust,exportvars);

  if(global){
    parallel::setDefaultCluster(clust);
    doParallel::registerDoParallel(clust);
    parallel_doexports(clust);
  }
  return(clust);
}
parallel_startglobalcluster=function(...){
  return(parallel_startcluster(...,global=T));
}
parallel_hasglobalcluster=function(){
  return(!is.null(parallel::getDefaultCluster()));
}
parallel_stopglobalclusterifexists=function(){
  if(parallel_hasglobalcluster()){ parallel_stopglobalcluster(); }
}
parallel_stopglobalcluster=function(backupexports=T){
  parallel_stopcluster(parallel::getDefaultCluster(), backupexports=backupexports);
}
#send functions (and their dependencies) to the workers
parallel_exportfunctions=function(clust=NULL,funcs,withdependencies=T){
  if(withdependencies){ funcs=unique(c(funcs, func_depends(funcs)));  }
  if(is.null(clust)){ clust=parallel_startglobalcluster(); }
  #msgf('Exporting the following functions to the cluster: %s',str_csv(funcs))
  parallel::clusterExport(clust,funcs);
}

###############################################################
# apply function to items in a list and collect the results as a list.
#The input variables here are cryptic, so we don't get a warning for possibly same variable names in globalenv when we use the workaround mentioned below.
list_map_parallel=function(a__list_map_parallel, fun__list_map_parallel,omap=NULL,...){
  if(isempty(a__list_map_parallel)){ return(list()); }
  installpackageifmissing('doParallel')

  if(!parallel_hasglobalcluster()){
    cluster=parallel_startglobalcluster(numtasks=length(a__list_map_parallel));
  }

  `%dopar%` = foreach::`%dopar%`
  i__list_map_parallel=NULL; #might be needed to define in this environment

  #20240202: ahmet: I have been getting error that the function I want to use (e.g., project_load) could not be found.
  #my current workaround is to export the globalenv to each worker (this was not necessary before; check again later.)
  #TODO: this is probably a bug in current foreach/doParallel package. when it is resolved, remove .export=.. and also make the variable names in this function less cryptic.

  #out = foreach::foreach(i__list_map_parallel = list_indexes(a__list_map_parallel) ) %dopar% {
  out = foreach::foreach(i__list_map_parallel = list_indexes(a__list_map_parallel), .errorhandling='pass') %dopar% {
      fun__list_map_parallel(a__list_map_parallel[[i__list_map_parallel]], ...)
  }

  #.errorhandling=='stop' handling does not show where the error happened. we used 'pass' above to capture any errors and print better.
  #Rawan comment = I needed to edit this forloop out to detect errors correctly - is this forloop meant to stay in this code? 
  #Ahmet: Yes, when there is an error in the worker task, it is captured (due to .errorhandling='pass' setting above) and returned; for such errors, the result list will contain the error instead of the function return value that would normally return if there were no errors. This code checks if there are any such errors returned by the workers and stops the master execution. Without this code (and without the ".errorhandling='pass'" option, the master still stops, but the error message it display is not as useful -- it doesn't tell us which function the error happened in.
  for(i in 1:length(out)){
   if(any(class(out[[i]])=='error')){
      dbg_warnf('Caught error in a parallel work.');
      stop(out[[i]]);
   }
   }

  #out=list();
  #for(i__list_map_parallel in list_indexes(a__list_map_parallel)){
  #  out[[i__list_map_parallel]]=fun__list_map_parallel(a__list_map_parallel[[i__list_map_parallel]], ...)
  #}
  
  return(out);
}

###############################################################
list_map=function(a,fun,omap=NULL,...){
  if(!length(a)){ return(list()); }
  if(length(a)>1 && var_tobool(omap$doparallel)){
    clust=parallel_startglobalcluster(numtasks=length(a),quiet=T);
    if(!is.null(clust)){ #a cluster may not be created if there's a nested parallelism.
      #.GlobalEnv$DBG=c(.GlobalEnv$DBG, '1488: doing parallel...');
    	#attr(ret,'didparallel')=TRUE; return(ret);
    	return(list_map_parallel(a,fun,omap,...));
    }
    #else{ .GlobalEnv$DBG=c(.GlobalEnv$DBG,'1492: no cluster available...'); }
  }
 
  a=as.list(a);
  for(i in list_indexes(a)){
    a[[i]] = fun(a[[i]],...);
  }
  return(a);
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
io_mkdir=function(dir){ dir.create(dir); return(dir); }
io_mkdirif=function(dir){
  if(!io_isdir(dir)){
    if(io_isfile(dir)){ stop(sprintf('Cannot create directory [%s], b/c it already exists as a file.',dir)); }
    dir.create(dir);
  }
  return(dir);
}
io_mkfiledirif=function(path){
  return(io_mkdirif(dirname(path)));
}
###############################################################
#just a helper function to create subdir if it is not there (mkdir's the subdir).
sys_dirorsubdir_=function(dir,subdir=NULL){
  io_mkdirif(dir);
  if(!is.null(subdir)){ dir=io_name(dir,subdir);  io_mkdirif(dir); }
  return(dir);
}
sys_cachedir=function(subdir=NULL){
  if(!exists('config')){ source_disabled__('config.r'); } 
  dir=config('cachedir');
  if(is.null(dir)){  dir=config('cachedir',default=sys_usercachedir());  }
  stopfif(is.null(dir));
  cachesubdir=config('cachesubdir');
  if(!is.null(cachesubdir)){ dir=io_name(dir,cachesubdir); io_mkdirif(dir); }
  return(sys_dirorsubdir_(dir,subdir));
}
sys_usercachedir=function(subdir=NULL){
  if(!exists('config')){ source_disabled__('config.r'); } 
  dir=config('cachedir');
  if(is.null(dir)){
    installpackageifmissing('rappdirs');
    dir=rappdirs::user_cache_dir();
    dir=config('cachedir',default=dir);
  }
  return(sys_dirorsubdir_(dir,subdir));
}
sys_tempdir=function(subdir=NULL){
  if(!exists('config')){ source_disabled__('config.r'); } 
  dir=config('tempdir',default=tempdir());
  return(sys_dirorsubdir_(dir,subdir));
}
sys_downloaddir=function(subdir=NULL){
  if(!exists('config')){ source_disabled__('config.r'); } 
  dir=config('downloaddir');
  if(is.null(dir)){  dir=config('downloaddir',default=sys_userdownloaddir());  }
  return(sys_dirorsubdir_(dir,subdir));
}
sys_userdownloaddir=function(subdir=NULL){
  dir=config('userdownloaddir');
  if(is.null(dir)){
    dir=io_name(sys_userhomedir(),'Downloads');
    dir=config('userdownloaddir',default=dir);
  }
  return(sys_dirorsubdir_(dir,subdir));
}
#' @export
sys_datadir=function(subdir=NULL){
  if(!exists('config')){ source_disabled__('config.r'); } 
  dir=config('datadir');
  if(is.null(dir)){
    default=NULL;
    for( base in c(getwd(),herehere(),thisdir()) ){
      trydir=io_name(base,'../data');
      if(io_isdir(trydir)){ default=trydir; break; }
      trydir=io_name(herehere(),'data');
      if(io_isdir(trydir)){ default=trydir; break; }
    }
    if(is.null(default)){ default=sys_userdatadir();   }
     dir=config('datadir',default=default);
  }
  return(sys_dirorsubdir_(dir,subdir));
}
sys_userdatadir=function(subdir=NULL){
  if(!exists('config')){ source_disabled__('config.r'); } 
  dir=config('userdatadir');
  if(is.null(dir)){
    installpackageifmissing('rappdirs');
    dir=rappdirs::user_data_dir();
    dir=config('userdatadir',default=dir);
  }
  return(sys_dirorsubdir_(dir,subdir));
}
###############################################################
data_hasrownames=function(d){
  #return(!all(row.names(d)==seq(1, nrow(d))));
  return(tibble::has_rownames(d));
}
data_column2rownames=function(d,col){
  if(is.logical(col)){
    stopfif(!col,"You should not call this function with col=FALSE");
    col=1;
  }
  #tibble requires col to be a text.
  if(is.numeric(col)){ col=colnames(d)[col]; }
  d=tibble::column_to_rownames(d,col);
  return(d)
}
#colname will default to 'rowname' (or rowname2+ until we obtain a column name not already in data.)
data_rownames2column=function(d,colname=NULL){
  if(!is.null(colname) && (colname %in% colnames(d))){
    if(all(rownames(d)==d[,colname])){ return(d); }
    stopfif('Column %s already exists in the data and has different values than the rownames. Refusing to change them.');
  }
  if(is.null(colname)){
    i=1;
    while(TRUE){
      if(i==1){ colname='rowname'; }
      else{ colname=paste0('rowname',i); }
      if(!(colname %in% colnames(d))){ break; }
      if(all(rownames(d)==d[,colname])){ return(d); }
      i=i+1;
    }
  }
  d=tibble::rownames_to_column(d,colname);
  return(d)
}

#rowNames will default to whether the data has rownames.
data_writefile=function(d,file,format=NULL,rowNames=NULL,append=F,...){
	stopfif(missing(file),'Missing filename as the second argument.')
	if(is.character(d)&&is.data.frame(file)){ temp=d; d=file; file=temp; }
	stopfif(is.character(d),"You need to provide the data as the first argument, and the file as the second argument.")

  if(is.null(format)){ format=io_fileext(file); }
  format=tolower(format);

  #we'll add a rowname column if we decide rowNames automatically.
  autorownames=F;
  if(is.null(rowNames)){ rowNames=data_hasrownames(d); autorownames=rowNames; }
  #when rowNames is not handled by a method, we do it here ourselves.
  #xls_write can handle rowNames, but it doesn't store the 'rowname' title. when we use data_readfile(), we end up not recovering the rownames.
 	if(autorownames
  ||(var_tobool(rowNames)&&(format %in% c('fst')))){
    d=data_rownames2column(d,var_pick(is.character(rowNames),rowNames,NULL));
    rowNames=F;
  }
  if(format %in% c('csv','tsv','tab')){ installpackageifmissing('data.table'); }

  #for methods that don't support an append option, read the file and merge the data.
  if(append&&io_isfileandnotempty(file)&&!(format %in% 'xlsx')){
    o=opt_set(...);
    d2=data_readfile(file,format,rowNames=rowNames);
    d=plyr::rbind.fill(d,d2);
  }

  if(format=='rds'){ saveRDS(d,file,...);  }
  else if(format=='fst'){
    installpackageifmissing('fst');
    fst::write_fst(d,file,...);
  }
  else if(format=='csv'){
    data.table::fwrite(d,file,row.names=rowNames,'sep'=',');
  }
  else if(format=='tsv'||format=='tab'){
    data.table::fwrite(d,file,row.names=rowNames,'sep'="\t");
  }
  else if(format=='xlsx'){
    xls_write(d,file,rowNames=rowNames,append=append,...);
  }
  else{
    stopf('Unknown/Unsupported file [%s] format [%s]',file,format);
  }
}

#rowNames will default to whether we have a column named 'rowname'
data_readfile=function(file,format=NULL,rowNames=NULL,header=T,...){
	if(is.logical(rowNames)&&rowNames){ rowNames=1; }
  if(is.null(format)){ format=io_fileext(file); }
  format=tolower(format);
  if(format %in% c('csv','tsv')){ installpackageifmissing('data.table'); }
  
  if(format=='rds'){ d=readRDS(file,...);  }
  else if(format=='fst'){
    installpackageifmissing('fst');
    d=fst::read_fst(file,...);
  }
  else if(format=='csv'){
    #fread behaves terribly when there's a date string --> it converts it to a date type (instead of keeping it as string); which makes the data incompatible with other R functions such as plyr::rbind()
    d=data.table::fread(file,sep=',',header=header,...);
  }
  else if(format=='tsv'||format=='tab'){
    d=data.table::fread(file,sep="\t",header=header,...);
  }
  else if(format=='xlsx'){
    installpackageifmissing('openxlsx');
    d=openxlsx::read.xlsx(file,rowNames=var_tobool(rowNames),colNames=header,...);
    if(!var_tobool(rowNames)){ rownames(d)=NULL; } #we get '1','2' as rownames. remove them, otherwise data_column2rownames will say rownames already exist.
    #print(d);
    #print(var_tobool(rowNames))
  }
  else{
    stopf('Unknown/Unsupported file [%s] format [%s]',file,format);
  }
  
  #when rowNames is not handled by a method (or we have discovered the rowName ourselves), we do it here ourselves.
 	if(var_tobool(rowNames)&&(format %in% c('csv','tsv','fst'))){
    d=data_column2rownames(d,rowNames);
  }
  else if(is.null(rowNames)){
    rowNames=NULL;
    if(('rowname' %in% colnames(d))){
      rowNames='rowname';
    }
    else if(nrow(d)>0 && ncol(d)>0 && is.character(d[,1])){
      rowNames=1;
      #condition: don't use it if there are any blanks.
      if(any(nchar(d[,1])==0)){ rowNames=NULL; }
      else{
       nums=as.numeric(d[,1]);
       if(all(!is.na(nums))&&any(nums%%1 != 0)){ rowNames=NULL; }
      }
    }
    if(!is.null(rowNames)&&nrow(d)>0&&!arr_isunique(d[,rowNames])){ 
      warnfif(rowNames=='rowname','There is a column "rowname" available in the file [%s], but the entries are not unique. I will not set rownames of the dataframe.',file);
      rowNames=NULL;
    }
    if(!is.null(rowNames)){
      d=data_column2rownames(d,rowNames);
    }
  }

  return(d);
}

#use cachedir='~~INPLACE~~' to store in the same folder as file.
data_readfile_cached=function(file,cachedir=NULL,recache=F,rowNames=NULL,...){
  #if file is an fst or rds file, just read and return.
  if(io_isfileext(file,c('fst','rsd'))){
    return(data_readfile(file,rowNames=rowNames,...));
  }
  file=io_realpath(file);
  #fst format is faster, let's use that. https://stackoverflow.com/questions/58699848/best-file-type-for-loading-data-in-to-r-speed-wise
  ocache=cache_opts(cachedir=cachedir,recache=recache,fileext='fst',depends=file,rowNames=rowNames,...); #I pass in the rowNames so it can be part of the cacheid.

  if(!ocache$recache){ return(data_readfile(ocache$cachefile,rowNames=rowNames,...));  }
  else{
    d=data_readfile(file,rowNames=rowNames,...);
    data_writefile(d,ocache$cachefile,rowNames=rowNames);
    return(d);
  }
}

data_readfile_memcached=function(file,...){
  file=io_realpath(file);
  hash=str_md5(list(file,...));
  if(!is.null(.GlobalEnv$zoz.data_readfile_memcached)&&(hash %in% names(.GlobalEnv$zoz.data_readfile_memcached))){
    return(.GlobalEnv$zoz.data_readfile_memcached[[hash]]);
  }
  else{
    d=data_readfile(file,...);
    .GlobalEnv$zoz.data_readfile_memcached[[hash]]=d;
    return(d);
  }
}
###############################################################

cache_opts=function(...){
  HARDDEPENDS=time_numeric('2024-02-27 10:48:00'); #I use this to invalidate any cache that is older than this date.
  o=opt_set(
    docache=T,recache=F
    ,cachefile=NULL
    ,fileext='rds' #cache_load() and cache_save() may not work with non-rds files if data is not a data.frame and/or the format is not a data file format. In such cases, it's your responsibility to do your own cache_load() and cache_save() logic for non-rds files. 
    ,cachedir=NULL #defaults to sys_cachedir(). Can be '~~INPLACE~~' if you want to use the same folder as the first o$depends file (o$depends file must be present)
    ,depends=NULL
    ,dataid=NULL
    ,processid=NULL
    ,dbg_cutlevels=0
    ,memcache=F #if true, we save it into memory instead of a cachefile. (o$cachefile is ignored)
    ,memid=NULL #used with memcache
  ,...);
  o$depends = c(o$depends,HARDDEPENDS);
  if(!o$docache){ o$recache=T; return(o); }

  if((o$memcache||is.null(o$cachefile)) && is.null(o$processid)){
    caller=dbg_caller(o$dbg_cutlevels+1);
    o$processid=gsub('\\(.*','',caller) #remove () from caller name.
  }
  if((o$memcache||is.null(o$cachefile)) && is.null(o$dataid)){
    ohash=o[base::setdiff(names(o),csv('docache,recache,cachefile,dataid,processid,dbg_cutlevels'))];
    o$dataid=str_md5(ohash);
  }

  if(o$memcache){
    if(is.null(o$memid)){ o$memid=paste0(o$processid,'.',o$dataid); }
    if(!o$recache){ o$recache=is.null(.GlobalEnv$zoz.cache)||is.null(.GlobalEnv$zoz.cache[[o$memid]]); }
    if(!o$recache&&!is.null(o$depends)){
      o$recache=.GlobalEnv$zoz.cache[[o$memid]]$mtime < io_filemtime(o$depends)
    }
  }
  else{
    if(is.null(o$cachefile)&&is.null(o$cachedir)){  o$cachedir=sys_cachedir(); }
    if(is.null(o$cachefile)&&o$cachedir=='~~INPLACE~~'){ 
      stopfif(isempty(o$depends),"cachedir='~~INPLACE~~' can only be used if a depends file/folder input is given.");
      if(io_isdir(o$depends[[1]])){ o$cachedir=o$depends[[1]]; }
      else{ o$cachedir=dirname(o$depends[[1]]); }
    }
    if(is.null(o$cachefile)){
      #I manually add a version number to sometimes force recache. TODO: a better approach would be to use a timestamp (similar to depends), so we don't create a new file for every version.
      o$cachefile=io_name(o$cachedir,io_sanitizefilename(paste0(o$processid,'.',o$dataid,'.',o$fileext)));
      }
    if(!o$recache){ o$recache=!io_isfileandnotempty(o$cachefile); }
    if(!o$recache&&!isempty(o$depends)){
      o$recache=io_filemtime(o$cachefile)<io_filemtime(o$depends);
      #msgfif(o$recache,"cache_opts(): Detected change in the dependency file. Triggering a recache...")
    }
  }
  return(o)
}
cache_load=function(o){
  if(o$memcache){
    return(.GlobalEnv$zoz.cache[[o$memid]]$data);
  }
  else{
    if(io_isfileext(o$cachefile,'rds')){ return(readRDS(o$cachefile)); }
    else{ return(data_readfile(o$cachefile)); } #if the file is not a supported format, data_readfile() will stop.
    #stop("You need to implement your own cache_load for non-rds files. Check fileext input to cache_opts if you unintentionally provided the file extension [%s]",io_fileext(o$cachefile));
  }
}
cache_save=function(d,o){
  if(!is.null(o$docache)&&!o$docache){ return(); }
  if(o$memcache){
    .GlobalEnv$zoz.cache[[o$memid]]=list(mtime=as.numeric(as.POSIXct(Sys.time())),data=d);
  }
  else{
    if(io_isfileext(o$cachefile,'rds')){ saveRDS(d,o$cachefile); }
    else{
      stopfif(!is.data.frame(d),"When the cachefileext is not RDS, the only data type we support is a data.frame (that would then be written using data_writefiel(). You need to implement your own cache_save for non-rds files and/or non-dataframe data. Check fileext input to cache_opts if you unintentionally provided the file extension [%s]",io_fileext(o$cachefile));
      data_writefile(d,o$cachefile); #if the file is not a supported format, data_readfile() will stop.
    }
  }
}
###############################################################
io_filemtime=function(path){
  if(isempty(path)){ return(0); }
  #if it's a list, it might be mixed (number & text), so we need to process them individually.
  #if it's a c(), mixed types are not supported.
  if(is.list(path)&&length(path)>1){
    mtimes=rep(0,length(path));
    for(i in 1:length(path)){ mtimes[[i]]=io_filemtime(path[[i]]); }
  }
  else{
    if(is.numeric(path)){ return(max(path)); }
    mtimes=as.numeric(as.POSIXct(file.mtime(path)));
  }
  if(all(is.na(mtimes))){ return(0); }
  mtime=max(mtimes,na.rm=T);
  return(mtime);
}
io_isnewer=function(mtime,depends,equalisnewer=F){
  if(isempty(depends)){ return(TRUE); }
  mtime=io_filemtime(mtime);
  depends=io_filemtime(depends);
  if(equalisnewer){ return(mtime>=depends); }
  else{ return(mtime>depends); }
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
io_rsync=function(src,dest,include=c(),exclude=c(),dryrun=F,echo=T){
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

  if(echo){
    catf(sprintf('Running rsync command:'));
    catf(paste(c('  ',rsyncexe,args),collapse=' '));
  }
	#s=system2(rsyncexe,shQuote(args),stdout=echo,stderr=echo);
  s=system2(rsyncexe,shQuote(args),stdout=T,stderr=T); #for some bizarre reason, unless we capture stdout/stderr, rsync doesn't actually work. So, we need to get output even if we don't display it.
  if(echo){ catf(str_lined(s)); }
  if(!isempty(excludefromtempfile)){ file.remove(excludefromtempfile); }
}


###############################################################
data_combineduplicaterows =function(df, func=mean, idcolumn='genesymbol'){
  #Solution selected from: https://stackoverflow.com/questions/10180132/consolidate-duplicate-rows
  #library(plyr)
  return( plyr::ddply(df, idcolumn, plyr::numcolwise(func)) );
}
#selected cols will come first in the requested order, and then everything else in their original order.
data_reordercols=function(d,cols){
  stopfif(!is.character(cols),'This function is intended for reordering based on *names of columns');
  dnames=colnames(d);
  return(d[, c(cols,colnames(d)[!(colnames(d) %in% cols)])  ]);
}
###############################################################

#path: use this if you want to save the body to a file, instead of getting it within resp.
http_httr2=function(url,method=NULL,path=NULL){
  installpackageifmissing('httr2')
  library(httr2)
  req=request(url) |> req_options(ssl_verifypeer = 0);
  if(!is.null(method)){ req=req_method(req,method); }
  resp=req_perform(req,path=path);
  return(resp);
}
http_getheaders=function(url,resp=NULL){
  if(is.null(resp)){ resp=http_httr2(url,method='HEAD'); }
  return(httr2::resp_headers(resp));
}
http_getheader=function(url,headerfield,resp=NULL){
  if(is.null(resp)){ resp=http_httr2(url,method='HEAD'); }
  httr2::resp_header(resp,headerfield);
}

http_getfilenamefromurl=function(url){
  filename=gsub('\\?.*','',url);
  filename=basename(filename);
  if(isempty(filename)||filename=='/'){ return('index'); }
  filename=io_sanitizefilename(filename);
  return(filename);
}
http_getfilenamefromheaders=function(url,hs){
  stopfif(is.character(hs),'hs must be a list. http_parseheaders() not implemented yet.');
  if(isempty(hs[['filename']])&&!isempty(hs[['content-disposition']])){
    tok=regexp_token(hs[['content-disposition']],'filename\\s*=\\s*[\'"]([^\'"]+)[\'"]');
    if(!isempty(tok)){ hs[['filename']]=tok; }
  }
  #typical headers don't contain a filename, but we may have added it elsewhere.
  if(!isempty(hs[['filename']])){ return(io_sanitizefilename(basename(hs[['filename']]))); }
  
  filename=http_getfilenamefromurl(url);
  if(!isempty(hs[['content-type']])&&io_isfileext(filename,c('','php','asp','aspx','cgi'))){
    mime=hs[['content-type']];
    mime=gsub(';.*','',mime);
    ext=basename(mime);
    if(str_contains(ext,'.')){ ext=gsub('^[\\.]+\\.','',ext); }
    ext=gsub('.*[\\-\\+]','',ext);
    if(nchar(ext)>8){ ext=io_fileext(ext); }
    if(nchar(ext)>8){ ext=substr(ext,1,8); }
    if(!isempty(ext)){ filename=paste0(filename,'.',ext); }
  }
  return(io_sanitizefilename(basename(hs[['filename']])));
}

#if a request/response is already done, provide it in resp.
#we cache if we are doing the request.
#provide a cachefilename, but make sure it is a name to be used for only storing the result of http_getfilename.
http_getfilename=function(url,resp=NULL,cachefile=NULL,cachedir=NULL){
  if(is.null(resp)){
  	ret=regexp_token(url,'/([^/#\\?]+\\.\\w{1,5})(#.*)?$'); #%if no question mark, and looks like a file name with an extension, use it.
	  if(!isempty(ret)){ return(ret); }
  }
  if(is.null(resp)){
    if(is.null(cachedir)){ cachedir=sys_downloaddir(); }
    #check cache.
    cachefile=io_name(cachedir,paste0('rhttp_getfilename.',str_md5(url)));
  }
  if(!isempty(cachefile)&&io_isfileandnotempty(cachefile)){
    return(io_read(cachefile));
  }
	hs=http_getheaders(url,resp);
	ret=http_getfilenamefromheaders(url,hs);
	if(!isempty(cachefile)){
	  io_write(cachefile,ret);
	}
	return(ret);
}
#Default downloaddir is sys_downloaddir(), which you can control by config('userdownloaddir')
downloadurl=function(url,file='',overwrite=FALSE){
    #%if url is not a remote address, assume it is a local file.
    if(!io_isremote(url)){
        if(isempty(file)){ return(url)}
        if(io_issamename(url,file)){ return(file); }
        if(!overwrite&&io_isfileandnotempty(file)){ return(file); }
        file.copy(url,file);
        return(file);
      }

    if(isempty(file)||endsWith(file,'/')){
      filename=http_getfilename(url,cachedir=var_pick(str_issuffix(file,'/'),file,NULL));
      if(config('downloadurl.addmd5',default=T)){
        filename=io_addfilenamesuffix(filename,paste0('.',str_md5(url))); #make the download unique to this url, to avoid name conflicts.
      }
      if(isempty(file)){ file=io_name(sys_downloaddir(),filename); }
      else{
        assertf(str_issuffix(file,'/'));
        file=io_name(file,filename);
      #todo: get filename from url request. use a preliminary meta file based on the current filename.
      }
    }
    if(!overwrite&&io_isfileandnotempty(file)){ return(file); }

    file=gsub('//+','/',str_replace(file,'\\','/'));

    if(!str_contains(file,'/')){
        file=io_name(sys_downloaddir(),file);
        if(!overwrite&&io_isfileandnotempty(file)){ return(file); }
        file=gsub('//+','/',str_replace(file,'\\','/'));
    }

    msgf('--- NOTICE: Attempting to download & save url [ %s ] to file [ %s ] ...\n',url,file);
    
    tmpfile=io_addfilenameprefix(file,'.down.')
    http_httr2(url,path=tmpfile);
    if(io_isfile(file)){ file.remove(file); }
    file.rename(tmpfile,file);
    return(file);
}

###############################################################
#convert a date/time to a unix integer time.
time_numeric=function(t=Sys.time()){
  return(as.numeric(as.POSIXct(t)));
}
time_date=function(t=Sys.time(),format='%Y-%m-%d %H:%M:%S'){
  if(is.numeric(t)){ t=as.POSIXct(t); }
  return(format(t,format));
}

###############################################################
opt_set=function(...){
  o=list();
  args=list(...);
  if(!length(args)){ return(o); }
  argnames=names(args);
  for(i in 1:length(args)){
    arg=args[[i]];
    if(isempty(argnames[[i]])){
      if(isempty(arg)){ next; }
      if(!is.list(arg)){ stop('Unnnamed arguments can only be lists. Use argname=argvalue or list(argname=argvalue) as input.'); }
      #o[names(arg)]=arg; #this is what list_merge() does. #this does not work when a value is NULL and we want it stored.
      for(name in names(arg)){
        val=arg[[name]];
        if(is.null(val)){ o[name]=list(NULL); } #this is a special way to store a NULL in a list element; otherwise just using "=NULL" has the meaning of removing an element. https://stackoverflow.com/questions/7944809/assigning-null-to-a-list-element-in-r
        else{ o[[name]]=val; }
      }
    }
    else{
      if(is.null(arg)){ o[argnames[[i]]] = list(NULL); }
      else{ o[[argnames[[i]]]] = arg; }
    }
  }
  return(o);
}
###############################################################
d5=function(d){
  if(nrow(d)==0||ncol(d)==0){ return(d); }
  return(d[1:min(5,nrow(d)),1:min(5,ncol(d))])
}
d10=function(d){
  if(nrow(d)==0||ncol(d)==0){ return(d); }
  return(d[1:min(10,nrow(d)),1:min(5,ncol(d))])
}
###############################################################
#wb can be an existing connection.
xls_sheetnames=function(file,wb=NULL){
	if(!is.null(wb)){ return(names(wb))}
	if(io_isfile(file)){ return(openxlsx::getSheetNames(file)); }
	else{ return(c()); }
}
###############################################################
xls_issheet=function(file,sheet,wb=NULL){
	names=xls_sheetnames(file);
	if(is.character(sheet)){ return(sheet %in% names); }
	else{ return(sheet<=length(names)); }
}
###############################################################
#Write an Excel file, but keep the sheets that we are not writing to. Also keep the existing formatting.
#options are: rowNames,colNames
#styleheader defaults to true if the excel file or the sheet did not exist before.
#open: whether to open the file after written.
xls_write=function(d,file,sheet=1,asTable=F,styleheader=NULL,open=F,append=F,colNames=T,...){
	installpackageifmissing('openxlsx');
	
	wasfile=io_isfile(file);
	if(!wasfile){ #create a blank file
		if(is.character(sheet)){ openxlsx::write.xlsx(data.frame(),file,overwrite=F,sheetName=sheet); }
		else{                    openxlsx::write.xlsx(data.frame(),file,overwrite=F); }
	}
	
	w=openxlsx::loadWorkbook(file);
	wassheet=xls_issheet(file,sheet,w);
	if(!wassheet){
		if(is.character(sheet)){ openxlsx::addWorksheet(w,sheet); }
		else{
			for(i in (length(xls_sheetnames(file,w))+1):sheet){	openxlsx::addWorksheet(w,i); }
		}
	}

	o=opt_set(rowNames=NULL,styleheader=var_tobool(styleheader)||!wasfile||!wassheet,...);

	if(o$styleheader&&is.null(o$headerStyle)){ o$headerStyle=openxlsx::openxlsx_getOp('headerStyle'); }
	if(o$styleheader&&is.null(o$headerStyle)){ o$headerStyle=openxlsx::createStyle(border='bottom',borderStyle='thick',textDecoration='bold'); }
	
  #clear the old data.
	sheet_=sheet; #w$worksheets[] has numeric indices.
	if(is.character(sheet)){ sheet_=which(xls_sheetnames(file,w)==sheet); }
  Rs=w$worksheets[[sheet_]]$sheet_data$rows;
  Cs=w$worksheets[[sheet_]]$sheet_data$cols;
  R=var_pick(length(Rs),max(Rs),0);
  C=var_pick(length(Cs),max(Cs),0);
  if(!append&&(R>nrow(d) || C>ncol(d))){
    openxlsx::deleteData(w,sheet,rows=1:R,cols=1:C,gridExpand=TRUE);
  }
  startRow=var_pick(append,R+1,1);
  if(startRow>1){ colNames=F; }

  d[is.na(d)]=''; #openxlsx can't handle na's well.
	if(asTable){ openxlsx::writeDataTable(w,sheet,d,colNames=colNames,startRow=startRow,headerStyle=o$headerStyle,...); }
	else{ openxlsx::writeData(w,sheet,d,colNames=colNames,startRow=startRow,headerStyle=o$headerStyle,...); }
	if(o$styleheader&&var_tobool(colNames)&&ncol(d)){
		openxlsx::addFilter(w,sheet,1,1:(ncol(d)+var_pick(o$rowNames,1,0)));
	}
	if(o$styleheader&&(var_tobool(colNames)||var_tobool(o$rowNames))){
		openxlsx::freezePane(w,sheet,firstRow=var_tobool(colNames),firstCol=var_tobool(o$rowNames));
	}
	if(o$styleheader&&var_tobool(o$rowNames)){
		openxlsx::addStyle(w,sheet,openxlsx::createStyle(textDecoration='bold'),1:(nrow(d)+1),1);
	}
	openxlsx::saveWorkbook(w,file,overwrite=T);
  if(open){ sys_open(file); }
}
###############################################################
#perform exact or prefix-matched indexing. If prefix-matched, the prefix is required to have '_'.
#returns list(val=>...,found=>...,var=>...)
#' @export
struct_getcolentry=function(o,colname){
#We may rename columns
#% (e.g, add '_percentile' suffix) and would still want the user options
#% apply to these new column names. 
#% Copyright (C) 2022 by Ahmet Sacan

  var = fieldname_match(names(o), colname);
  if(is.null(var)){ return(list(val=NULL,found=F)); }
  return(list(val=o[[var]],found=T,var=var));  
}

###############################################################
#use as alternative to tictoc::toc()
tictoc_pretty=function(print=T,...){
    t=tictoc::toc(...,quiet=T);
    if(isempty(t)){ msgf('tictoc_pretty(): toc is empty. Did you call tictoc::tic() first?'); return(NULL); }
    t=as.numeric(t$toc-t$tic);
    hours=floor(t/3600); t=t%%3600;
    mins=floor(t/60);
    secs=t%%60;
    if(hours>0){ s=sprintf('%d:%02d:%02d',hours,mins,round(secs)); }
    else if(mins>0){ s=sprintf('%2d:%02d',mins,round(secs)); }
    else { s=sprintf('%.2fsecs',secs); }
    if(print){ msgf('%s elapsed.',s); return(invisible(NULL)); }
    else{ return(s); }

}
###############################################################
sys_cpuinfo=function(){
  installpackageifmissing('benchmarkme');
  return(benchmarkme::get_cpu())
}
###############################################################
sys_raminfo=function(){
  installpackageifmissing('benchmarkme');
  tryCatch({ return(benchmarkme::get_ram()); }, error=function(e){});
  return('Unknown GB');
}
###############################################################
sys_ischopserver=function(){
  return(grepl('^c-\\d+',sys_computername()));
}
###############################################################

#stk__=dbg_nicestack(1); message(sprintf('util.r sourced from: %s',stk__));
