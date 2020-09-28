library(shiny)
library(calibrate)
library(gplots)
library(fields)
library(ggrepel)
library(ggplot2)

library(readxl)
library(httr)

#library(scales)
#library(lubridate)
#library(tidyr)
#library(dplyr)

#packageVersion("readxl")

#quad.plot.WC <- data.frame(read.csv("C:/Users/Guest_a/Downloads/WC_quadplot_2019.csv"))

#quad.plot.WC <- data.frame(read.csv("https://raw.githubusercontent.com/Cave42/Fisheries-Shiny-App/main/WC_quadplot_2019.csv"))

#quad.plot.WC <- data.frame(read.csv("https://raw.githubusercontent.com/Cave42/Fisheries-Shiny-App/main/WC_quadplot_2019.csv"))

quad.plot.WC <- data.frame(read.csv("https://raw.githubusercontent.com/Cave42/Fisheries-Shiny-App/main/Fisheries_Updated_File.csv"))

function(input, output){
 
  output$distPlot <- renderPlot({
  
  ggplot(subset(quad.plot.WC,Spp_type %in% c(input$checkGroup) & Assessment_Year %in% c(input$checkGroup2)),aes(B.Bmsy,F.Fmsy, label=Abb1,color=Spp_type,shape=Spp_type)) + geom_point()+
    xlim(0,4)+
    ylim(0,1.5)+
    geom_text_repel(show.legend = FALSE,aes(label = Abb1))+
    theme_light()+
    theme(legend.title=element_blank())+
    labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity")))+
    geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
    geom_hline(yintercept = 1,lty=2)+
    guides(shape = guide_legend(override.aes = list(size = 4)))

  })

}
