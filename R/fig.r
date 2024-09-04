#% Copyright (C) 2024 by Ahmet Sacan
library(ggplot2)
library(dplyr)

fig_frequencybarplot=function(ss,...){
  o=opt_set(
    showlegend=F
    ,...)
  d=data.frame(Group=unlist(ss)) |> group_by(Group) |> summarize(N=n())
  d=d[order(-d$N),];
  d$Group <- factor(d$Group, levels=d$Group) #this is to force geom_bar to follow our order; otherwise it reorders the x axis.
  
  #https://stackoverflow.com/questions/64392990/how-to-create-a-barplot-in-r-with-frequencies-on-the-y-axis-not-the-densities
  fig=d |>
  ggplot(aes(x=Group,y=N,fill=Group))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  geom_text(aes(label=N),vjust=-0.25,fontface='bold')+
  theme_bw()+
  theme(axis.text = element_text(angle=90,hjust=0.95,color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))
  if(!o$showlegend){ fig + theme(legend.position="none"); }
}

#xcol is the column name that is represented as the x axis.
fig_densityandhistogram=function(d,xcol=NULL,...){
  o=opt_set(
    fill="#FF6666"
    ,...)
  d=as.data.frame(d);
  if(is.null(xcol)){ xcol=colnames(d)[[1]]; }
  ggplot(d, aes(x=.data[[xcol]])) + 
  geom_histogram(aes(y = after_stat(count)), colour="black", fill=o$fill)+
  geom_density(aes(y = after_stat(count)),alpha=.2, fill=o$fill) 
}