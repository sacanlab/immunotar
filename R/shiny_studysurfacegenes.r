source_disabled__=function(...){invisible(NULL)}
#Shiny App to study the overlap among different data sources.
# Copyright (C) 2022 by Ahmet Sacan
source_disabled__('util.r');
source_disabled__('biodb.r');
source_disabled__('theratardb.r');

immunotar_studysurfacegenes=function(...){
o=opt_set(...);

gsets=list();
d=cirfessdb_getspc();
d=data_renamecolumn(d,'spc','score');
gsets$cirfess=d;

d=compartmentsdb_getcellsurfacegenes();
d=data_renamecolumn(d,'confidence','score');
gsets$compartments=d;

d=uniprotdb_getecmtotallength();
d=data_renamecolumn(d,'ecmtotallength','score');
gsets$uniprot=d;

t=theratardb_disease2genesymbols('%',minstatusscore=1);
d=as.data.frame( dplyr::summarise(dplyr::group_by(t, genesymbol), max=max(higheststatus_,na.rm=T)) )
d=data_renamecolumn(d,'max','score');
gsets$theratar=d;

t=theratardb_disease2genesymbols('%', moa=c('CAR-T-Cell-Therapy%','adc_antigen'),minstatusscore=1);
d=as.data.frame( dplyr::summarise(dplyr::group_by(t, genesymbol), max=max(higheststatus_,na.rm=T)) )
d=data_renamecolumn(d,'max','score');
gsets$theratarimmuno=d;

d=data.frame(genesymbol=uniprotdb_allgenesymbols(9606));
gsets$allhumangenes=d;


defaultscores=list(cirfess=0,compartments=0,uniprot=1,theratar=1,theratarimmuno=1);

installpackageifmissing('shiny,shinyjs,ggVennDiagram')
library(shiny);
library(shinyjs);

inputs=list();
for(name in names(gsets)){
	d=gsets[[name]];
	inputs[[length(inputs)+1]]=checkboxInput(paste0(name,'_use'), label=name, value = TRUE)
	if(!('score' %in% colnames(d))){ next; }
	step=NULL;
	minval=min(d$score); maxval=max(d$score); value=defaultscores[[name]];
	if(name=='uniprot'){ maxval=1000; step=1; }

	if(is.null(value)||value<minval){ value=minval; }
	else if(value>maxval){ value=maxval; }

	inputs[[length(inputs)+1]]=sliderInput(inputId=paste0(name,'_score'),label=NULL,min=minval,max=maxval,value=value,step=step);
}

ui=fluidPage(
  useShinyjs(),
  fluidRow(column(width=12,  titlePanel("Immunotar"))),
  fluidRow(
  	column(width=12,
  				 sidebarLayout(
sidebarPanel(
			style = "height: 90vh; overflow-y: auto;", 
			do.call(div,inputs)
  	),
  mainPanel(
  	style = "height: 90vh; overflow-y: auto;"
   ,plotOutput("myplot",width = "100%")
   ,textOutput("mytext")
   ,dataTableOutput("mytable")
  )
)
  )),
  fluidRow( 
  	tags$footer(verbatimTextOutput('log', placeholder = TRUE),style='overflow-y: auto; position:fixed; bottom:11.5px; width:100%;height:80px; color: black; padding:0px; background-color: #f2f2ed;z-index: 100;') 
  )
)



server=function(input, output, session) {
	addlogf=function(...){ s=sprintfyaml(...); print(s); html(id='log',html=sprintf('%s<br>',s),add=T); }
	output$myplot=renderPlot(
		{
			sets=list();
			for(name in names(gsets)){
				if(!input[[paste0(name,'_use')]]){ next; }
				d=gsets[[name]];
				genes=d$genesymbol;
				if('score' %in% colnames(d)){
					score=input[[paste0(name,'_score')]];
		      genes=genes[which(d$score>=score)];
				}
				sets[[name]]=genes;
			}
			ggVennDiagram::ggVennDiagram(sets, label_alpha=0,set_size=8, label_size = 6) +
			 ggplot2::scale_fill_distiller(palette = "Pastel1", direction = 1) +
			 ggplot2::theme(text = ggplot2::element_text(size = 20)) 
		}
		,width=800,height=800
	)
}

shinyApp(ui, server)
}


source_disabled__('util.r')

if (exists('sys_amiclimain')&&sys_amiclimain()){
	o=argv2opt(commandArgs(trailingOnly=T));
	immunotar_studysurfacegenes(o);
}else if(exists('sys_amirstudiomain')&&sys_amirstudiomain()){
  immunotar_studysurfacegenes();
}
