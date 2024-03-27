#!/usr/bin/env Rscript
#% Copyright (C) 2023 by Ahmet Sacan
source('util.r');
source('packager.r');

#TODO: Remove src.r/mysetwd.r or find an alternative solution.
# See available options in packager.r::makepackage()
immunotar_makepackage=function(...,dryrun=F,echo=T){
  o = opt_set(
  	pkgname='immunotar'
  	,rsync_exclude_more=arr_csv('Archive/,shinyapp/,sandbox.r,sandbox.rmd,packager.r,packager_demo.rmd,mysetwd.r,config.*.yml,git.r')
  	,rsyncmore=list(
  		list(src=io_name(thisdir(),'../data/'),dest='inst/data/', include=c('demo*','optimized*','project_defaultparams.yml','project_optimizedparams.yml') )
  		,list(src=io_name(thisdir()),dest='inst/', include=c('config.yml') )
  		,list(src=io_name(thisdir(), '../vignettes/'), dest='vignettes/')
  		,list(src=io_name(thisdir(), '../img/'), dest='img/')
  	)
  ,...);
  return(packager_makepackage(o,dryrun=dryrun,echo=echo));
}


if (sys.nframe() == 0){
	o=argv2opt(commandArgs(trailingOnly=T), mapnames=c('pkgdir'));
	immunotar_makepackage(o);
}
