#!/usr/bin/env Rscript
#% Copyright (C) 2023 by Ahmet Sacan
#source('util.r');
#source('packager.r');

#TODO: Remove src.r/mysetwd.r or find an alternative solution.
# See available options in packager.r::makepackage()
immunotar_makepackage=function(o=list(),...,dryrun=F,quiet=F){
  o = list_merge(list(
  	pkgname='immunotar'
  	,rsync_exclude_more=arr_csv('Archive/,sandbox.r,sandbox.rmd,packager.r,packager_demo.rmd,mysetwd.r,config.*.yml,git.r')
  	,rsyncmore=list(
  		list(src=io_name(thisdir(),'../data/'),dest='inst/data/', include=c('demo*','optimized*','project_defaultparams.yml') )
  		,list(src=io_name(thisdir()),dest='inst/', include=c('config.yml') )
  		)
  ),o,list(...));
  packager_makepackage(o,dryrun=dryrun,quiet=quiet);
}


if (sys.nframe() == 0){
	o=argv2get(commandArgs(trailingOnly=T), mapnames=c('libpath'));
	immunotar_makepackage();
}
