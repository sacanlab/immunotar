source_disabled__=function(...){invisible(NULL)}
#Immunotar Shiny App
# Copyright (C) 2022 by Ahmet Sacan
source_disabled__('util.r');
source_disabled__('project.r');

immunotar_runshiny=function(p){

stopfif(!exists('p')||is.null(p), 'You need to provide a project variable p before running this script.');

p=project_runonce(p);

sliders=list();
#type=weights|curves
proj_getsliders=function(p,type='weights'){
	vec=p[[type]];
	for(name in names(vec)){
		val=vec[[name]];
		minval=min(-10,val-abs(val));
		maxval=max(10,val+abs(val));
		slider=list(inputId=paste0(type,'_',name), label=name,min=minval,max=maxval,value=val);
		sliders[[length(sliders)+1]]=slider;
	}
	return(sliders);
}

#https://stackoverflow.com/questions/47045729/r-shiny-how-to-build-a-list-of-input-elements-and-then-return-them-as-a-single
buildsliders=function(sliders){
  inputs <- lapply(sliders,function(args) {
    do.call(sliderInput, args)
  })
  do.call(div,inputs)
}


installpackageifmissing('shiny,shinyjs')
library(shiny); library(ggplot2); library(shinyjs);

###############################
#Application Layout
#https://appsilon.com/shiny-application-layouts/
ui=fluidPage(
  useShinyjs(),
  fluidRow(column(width=12,  titlePanel("IMMUNOTAR"))),
  fluidRow(
  	column(width=12,
  				 sidebarLayout(
sidebarPanel(
			style = "height: 90vh; overflow-y: auto;", 
	  	tabsetPanel(
	  		selected='weights'
		    ,tabPanel('dataset',
					p('TODO: Add components to allow the user to upload a dataset or select all human/surface genes and selectors for enrichment types, depmapids, goids, etc..')
          #TODO: If you make this app available on a web server, turn off (or secure) file import functionalities.
		    ),
		    tabPanel('weights',
					buildsliders(proj_getsliders(p,'weights'))
		    ),
		    tabPanel('curves',
					buildsliders(proj_getsliders(p,'curves'))
		    )
	    ),
  	),
  mainPanel(
  	style = "height: 90vh; overflow-y: auto;"
   ,textOutput("mytext")
   ,dataTableOutput("mytable")
   ,plotOutput("myplot")
  )
)
  )),
  fluidRow( 
  	tags$footer(verbatimTextOutput('log', placeholder = TRUE),style='overflow-y: auto; position:fixed; bottom:11.5px; width:100%;height:80px; color: black; padding:0px; background-color: #f2f2ed;z-index: 100;') 
  )
)



server=function(input, output, session) {
	p=project_runonce(p);
	addlogf=function(...){ s=sprintfyaml(...); print(s); html(id='log',html=sprintf('%s<br>',s),add=T); }
	
	newp=reactive({
		for(name in names(p$weights)){ p$weights[[name]]=input[[paste0('weights_',name)]];	}
		for(name in names(p$curves)){ p$curves[[name]]=input[[paste0('curves_',name)]];	}
		ret=project_runfull(p);
		ret
	})
	output$mytable = renderDataTable({
		d=newp()$datawithscore;
		cbind(gene=rownames(d),d)
		}
		,options=list(pageLength = 5));
  output$myplot =renderPlot({ project_rankplot(newp())+ggtitle('RankPlot') });
	output$mytext <- renderText({s=''; rankeval=newp()$rankeval; if(!is.null(rankeval)){s=sprintf('Rankeval: %f',rankeval);}; s});
}

shinyApp(ui, server, options = list(height = 1080))

}

source_disabled__('util.r')

if (exists('sys_amiclimain')&&sys_amiclimain()){
	o=argv2opt(commandArgs(trailingOnly=T), mapnames=c('projectfile'));
  stopfif(isempty(o$projectfile),"Usage: projectfile.yml [options]");
	immunotar_runshiny(o$projectfile,o);
}else if(exists('sys_amirstudiomain')&&sys_amirstudiomain()){
	stopfif(!exists('p')||is.null(p), 'You need to make available a project variable p before running this script directly in Rstudio.');
  immunotar_runshiny(p);
}
