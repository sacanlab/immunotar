#% Copyright (C) 2022 by Ahmet Sacan
#TODO: Rename this file as config.r. filenames aren't that important in R and don't create conflicts across different packages.
#source('util.r');

# Configuration variables (and their defaults) that affect how the configuration works:
# config.subconfig='' #provide a subconfig name. The 'default' subconfig is always loaded first, the parameters in this subconfig, when present, will override those loaded from the default subconfig. subconfig can be a csv of multiple subconfig names.
# config.useenv=T  #whether to use the environment variables R_CONFIG_FILE and R_CONFIG_SUBCONFIG when determining the value of config.file and config.subconfig
# config.file.usecomputername=T #whether we look for config.[computername].yml
# config.subconfig.usecomputername=T #if a config.subconfig is not already set, whether we use this computername as a subconfig.
# config.resetonchdir=F  # whether to reset the config (so a new config_file can be searched and loaded) if the cwd changes.
# config.resetcodedonreset=F  #whether to reset the variables that are set from code (by calls to config(varname,val)) when we do a reset on chdir or when we reload a config file.

# A configuration file can contain the following special options:
# config.importfile: ... #a single file or a list of files to import. For import statements, it's your responsibility to avoid recursion.
# config.importsubconfig: ... # a single subconfig or a list of subconfigs.

###############################################################
#Automatically replace {datadir}, {biodbdir}, and {userdatadir} with their config values.
config_autofill=function(s){
  if(is.character(s)&&grepl('{',s,fixed=T)){
    if (grepl('{datadir}',s,fixed=T)){
      s=sub('{datadir}',config('datadir'),s,fixed=T);
    }
    if (grepl('{biodbdir}',s,fixed=T)){
      s=sub('{biodbdir}',config('biodbdir'),s,fixed=T);
    }
    if (grepl('{userdatadir}',s,fixed=T)){
      s=sub('{userdatadir}',rappdirs::user_data_dir(),s,fixed=T );
    }
  }
  return(s);
}

###############################################################
#get/set the path of config.yml file.
#if a config_file has previously been set (or found), we return that.
#if no config_file is given or set before, we check R_CONFIG_FILE environment variable. If that is absent, we search current directory for config.yml file. (and also search parent folders if searchupward=T).
# searchupward, when not given, defaults to what we get from config_('config.file.searchupward')
# the config.yml resolution is borrowed from config::get.R source code: https://github.com/rstudio/config/blob/main/R/get.R
# if you want to reset a previously saved file and trigger a new search, use config_file("")
# if usecomputername=T, we use computer-specific config file (if found), e.g., config.sacanlap.yml. The default T/F is obtained from config_('config.file.usecomputername')
# if useenv=T, we use getenv('R_CONFIG_FILE'). The default for useenv is retrieved using config_('config.file.useenv')
#' @export
config_file = function(file=NULL, usecomputername=NULL, searchupward=NULL, useenv=NULL, dbg=F) {
   # attr(config_file,'file')=NULL; #reset for testing purposes. comment out later.
  if(!missing(file)){
    if(dbg&&!isempty(file)){ catf('Got file input: [%s]',file); }    
    #this does not work when we package this file into a library. R considers these functions to be locked when in library and throws error "cannot change value of locked binding for 'config.file'"
    #attr(config_file,'file')<<-file;  #"double arrow" is required for saving into an attr().
    config('config.file',file);
  }
  else{
    #file = attr(config_file,'file'); #static/persistent variable.
    file=config_('config.file');
  }
  if(!isempty(file)){
    if(!io_isfile(file)){ stop(paste0('The specified configuration file [',file,'] does not exist.')); }
     return(file); 
  }

  ## try useenv ?
  if(is.null(useenv)){ useenv=config_('config.useenv'); }
  if(useenv){
    file=Sys.getenv('R_CONFIG_FILE');
    if(!isempty(file)){ 
      if(!io_isfile(file)){ stop(paste0('The configuration file [',file,'] does not exist. It was obtained from R_CONFIG_FILE environment variable.')); }
      return(file);
    }
  }

  if(is.null(usecomputername)){ usecomputername=config_('config.file.usecomputername'); }
  if(usecomputername){ filenames=c(paste0('config.',sys_computername(),'.yml'),'config.yml'); }
  else{ filenames='config.yml'; }
 
  if(is.null(searchupward)){ searchupward=config_('config.file.searchupward'); }
  for(filename in filenames){
    if(searchupward){
      file=io_searchupward(filename,dieifnotfound=F,dbg=dbg); 
      if(!isempty(file)){ 
        if(dbg){ catf('Discovered file with searchupward: [%s]',file); }
        break;
      }
    }
    else{
      file=io_resolvepath(filename);
      if(!isempty(file)){ 
        if(dbg){ catf('Result of io_resolvepath(config.yml): [%s]',file); }
        break;
      }
    }
  }
  if(isempty(file)){
    pkg=mypackagename();
    if(!isempty(pkg)){
       file=system.file('config.yml',package=pkg);
       if(dbg){ catf('Detected that I am in-package [%s], got package config file [%s]',pkg,var_pick(var_tobool(file),file,'')); }
    }
  }

  if(isempty(file)||!io_isfile(file)){
    stop(paste0('Could not locate the config.yml file. You need to either create one in the current folder ',ifelse(searchupward,' or one of the parent folders',''),'. Alternatively, you can use R_CONFIG_FILE environment variable or call config_file() function to specify the location of the config file you want to use.'));
    #TODO: create a config file from source code.
  }
  #attr(config_file,'file')<<-file;
  config('config.file',file);
  return(file);
}

###############################################################
# set/get the subconfig name.
#' @export
config_subconfig=function(subconfig=NULL,useenv=NULL){
  if(!missing(subconfig)){
    config('config.subconfig',subconfig);
    return(subconfig);
  }
  subconfig=config_('config.subconfig');
  if(!is.null(subconfig)){ return(subconfig); }

  ## try useenv ?
  if(is.null(useenv)){ useenv=config_('config.useenv'); }
  if(useenv){
    subconfig=Sys.getenv('R_CONFIG_SUBCONFIG');
    if(!isempty(subconfig)){ return(subconfig); }
  }

  ## usecomputername?
  if(config_('config.subconfig.usecomputername')){
    return(sys_computername());
  }

  return(NULL);
}
###############################################################
#handle config.importfile and config.importsubconfig.
#srcfile is used to resolve relative filenames.
config_handleimports=function(y,srcfile){
  if('config.importfile' %in% names(y)){
    y2=list();
    for(name in names(y)){ #we go through one-by-one so the config.importfile options can take place where they appear, so configs appearing after that are not overridden.
      if(name=='config.importfile'){
        for(importfile in y$config.importfile){ #it can be a list of multiple files
          importfile=gsub('\\','/',importfile,fixed=T);
          if(!grepl('/',importfile,fixed=T)||startsWith(importfile,'./')||startsWith(importfile,'../')){ importfile=io_name(dirname(srcfile),importfile); }
          y2=modifyList(y2,config_readconfigfile(importfile));
        }
      }
      else{ y2[[name]]=y[[name]]; }
    }
    y=y2;
    #config_dbgmsg('after importfile : \n%s\n',str_indentlines(yaml::as.yaml(y)))
  }
  if('config.importsubconfig' %in% names(y)){
    #config_dbgmsg('before importsubconfig : \n%s\n',str_indentlines(yaml::as.yaml(y)))
    y2=list();
    for(name in names(y)){
      if(name=='config.importsubconfig'){
        for(subconfig in y$config.importsubconfig){ #it can be a list of multiple subconfig names
          if(subconfig %in% names(y)){
            #config_dbgmsg('applying subconfig %s',subconfig)
            y2=modifyList(y2,y[[subconfig]]);
          }
        }
      }
      else{ y2[[name]]=y[[name]]; }
    }
    y=y2;
    #config_dbgmsg('after importsubconfig : \n%s\n',str_indentlines(yaml::as.yaml(y)))
  }
  return(y);
}

###############################################################
#Read a config yml file, and handle any config.importfile statements.
#we do not check for recursion. it's your responsibility to make sure config.importfile statements (if any) do not produce recursion.
config_readconfigfile=function(file=NULL){
  if(is.null(file)){ file=config_file(); }
  if(is.null(file)||!io_isfile(file)){ stop(paste0('Config file [%s] does not exist.',file)); }
  y=yaml::read_yaml(file);
  y=config_handleimports(y,file);
  return(y);
}

###############################################################
#Read a config file, and handle subconfig selection (if any) and store it in the .GlobalEnv. subconfig selection may include config.importfile and config.importsubconfig statements.
config_loadconfigfile=function(file=NULL){
  y=config_readconfigfile(file);
  if('default' %in% names(y)){
    y=modifyList(y$default,y); y=config_handleimports(y,file);
  }
  subconfig=var_pickfirstnotnull(y$config.subconfig, config_subconfig());
  config_dbgmsg('Subconfig is [%s]',subconfig);
  if(!is.null(subconfig)){
    for(subconfig in arr_csv(subconfig)){
      if(subconfig %in% names(y)){
        y=modifyList(y,y[[subconfig]]); y=config_handleimports(y,file);
      }     
    }
  }
  .GlobalEnv$zoz.config=y;
  return(y);
}
#' @export
config_reset=function(resetcoded=NULL){
  .GlobalEnv$zoz.config=NULL;
  if(is.null(resetcoded)){ resetcoded=config_('config.resetcodedonreset'); }
  if(resetcoded){ .GlobalEnv$zoz.config.coded=NULL; }
}

###############################################################
config_dbg=function(newvalue){
  if(!missing(newvalue)){ return(config('config.dbg',newvalue)); }
  return(var_tobool(var_pickfirstnotnull(.GlobalEnv$zoz.config.coded$config.dbg,.GlobalEnv$zoz.config$config.dbg)));
}
#if config.dbg is ON, print a message using catf(...)
config_dbgmsg=function(...,cutlevels=1,withargs=F){
  if(!config_dbg()){ return(); }
  st=dbg_nicestack(cutlevels+1,withargs=withargs);
  if(isempty(c(...))){ catf(sprintf('%s\n',st)); }
  else{ cat(sprintf('%s: %s',st,str_ensuresuffix(sprintf(...),'\n'))); }
}
#shorthand for config_dbgmsg(...,withargs=T)
config_dbgmsg_=function(...,cutlevels=1){
  config_dbgmsg(cutlevels=cutlevels+1,withargs=T);
}
###############################################################
#shorthand for config(varname,useonlystored=T)
config_=function(varname){
  return(config(varname,useonlystored=T));
}
###############################################################
#useonlystored=T is used for internal purposes; when true, we use the stored or already loaded configuration and we do not use loadconfigfile.
#.GlobalEnv$zoz.config.coded is used for values stored with config(varname,newvalue)
#.GlobalEnv$zoz.config is used for values loaded from config file
#' @export
config = function(varname,newvalue,useonlystored=F){
  if(missing(varname)){
    if(is.null(.GlobalEnv$zoz.config)){ config_loadconfigfile(); }
    return(list_merge(.GlobalEnv$zoz.config,.GlobalEnv$zoz.config.coded));
  }
  ## A new coded value is given, save it and we are done.
  if(!missing(newvalue)){
    .GlobalEnv$zoz.config.coded[[varname]]=newvalue;
    return(invisible(config_autofill(newvalue))); #invisible allows capture but prevents automatic printout of returned results.
  }
  #config_dbgmsg_();
  ##check whether we need to keep track of pwd
  if(!useonlystored && config('config.resetonchdir',useonlystored=T)){
    if(is.null(.GlobalEnv$zoz.config.lastcd)){ .GlobalEnv$zoz.config.lastcd=getcd; }
    else if(.GlobalEnv$zoz.config.lastcd != getcd){
      config_reset();
      .GlobalEnv$zoz.config.lastcd=getcd;
    }
  }

  ## If a stored value is found, return it.
  if(!is.null(.GlobalEnv$zoz.config.coded)&&(varname %in% names(.GlobalEnv$zoz.config.coded))){
    return(config_autofill(.GlobalEnv$zoz.config.coded[[varname]]));
  }
  if(!is.null(.GlobalEnv$zoz.config)&&(varname %in% names(.GlobalEnv$zoz.config))){
    return(config_autofill(.GlobalEnv$zoz.config[[varname]]));
  }

  ## Handle some of the special config.XXX options. I
  #if they are already set, they would have been returned above. We return the defaults here:
  if(startsWith(varname,'config.')){
    defaults=list(
      config.useenv=T
      ,config.file.usecomputername=T
      ,config.file.searchupward=T
      ,config.subconfig.usecomputername=T
      ,config.resetonchdir=F
      ,config.resetcodedonreset=F
      ,config.dbg=F
    );
    if(varname %in% names(defaults)){ return(defaults[[varname]]); }
  }

  if(useonlystored){ return(NULL); }
  if(!is.null(.GlobalEnv$zoz.config)){ return(NULL); } #config file was already loaded.

  #config_dbgmsg('Calling loadconfigfile()...')
  config_loadconfigfile();
  if(!is.null(.GlobalEnv$zoz.config)&&(varname %in% names(.GlobalEnv$zoz.config))){
    return(config_autofill(.GlobalEnv$zoz.config[[varname]]));
  }
  return(NULL);
}

#TODO: I renamed config() with config(). After updating all calls, delete this file.
myconfig=function(...){ return(config(...)); }
