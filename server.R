library(shiny)
library(calibrate)
library(gplots)
library(fields)
library(ggrepel)
library(ggplot2)

library(readxl)
library(httr)

library(shinyWidgets)
library(shinythemes)

quad.plot.WC <- data.frame(read.csv(paste0(getwd(),"/Fisheries_Updated_File.csv")))

function(input, output, session){
  
observe({
  
  if (input$Switch1 == 1){ updateCheckboxGroupInput(session = session, inputId = "checkGroup4", choices = unique(quad.plot.WC$Abb2), selected = unique(quad.plot.WC$Abb2)) }
  
  else if (input$Switch1 == 0){updateCheckboxGroupInput(session = session, inputId = "checkGroup4", choices = unique(quad.plot.WC$Abb2), selected = NULL)}
  
  if (input$Switch2 == 1) { updateCheckboxGroupInput(session = session, inputId = "checkGroup2", choices = unique(quad.plot.WC$Assessment_Year), selected = unique(quad.plot.WC$Assessment_Year))}
  
  else if (input$Switch2 == 0){ updateCheckboxGroupInput(session = session, inputId = "checkGroup2", choices = unique(quad.plot.WC$Assessment_Year), selected = NULL) }
 
})

output$distPlot <- renderPlot({
      
    if (input$Id073 == "F/Fmsy"){
        
      if (input$YearSpecies1 == "No"){
        ggplot(subset(quad.plot.WC, Abb2 %in% c(input$checkGroup4) & Spp_type %in% c(input$checkGroup) & Assessment_Year %in% c(input$checkGroup2)), aes(B.Bmsy, F.Fmsy, label=Abb2 ,color=Spp_type,shape=Spp_type)) + geom_point()+
          xlim(0,4)+
          ylim(0,1.5)+
          geom_text_repel(show.legend = FALSE,aes(label = Abb2))+
          theme_light()+
          theme(legend.title=element_blank())+
          labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (F/Fmsy)")))+
          geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
          geom_hline(yintercept = 1,lty=2)+
          guides(shape = guide_legend(override.aes = list(size = 4)))
       
      }
      
      else {
      
        ggplot(subset(quad.plot.WC, Abb2 %in% c(input$checkGroup4) & Spp_type %in% c(input$checkGroup) & Assessment_Year %in% c(input$checkGroup2)), aes(B.Bmsy, F.Fmsy, label=Abb2 ,color=Spp_type,shape=Spp_type)) + geom_point()+
          xlim(0,4)+
          ylim(0,1.5)+
          geom_path(aes(linetype=Abb2))+
          geom_text_repel(show.legend = FALSE,aes(label = paste("(",Abb2,",",Assessment_Year,")")))+
          theme_light()+
          theme(legend.title=element_blank())+
          labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (F/Fmsy)")))+
          geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
          geom_hline(yintercept = 1,lty=2)+
          guides(shape = guide_legend(override.aes = list(size = 4)))
      }
    }

  else if (input$Id073 == "TM_ABC"){
    
    if (input$YearSpecies1 == "No"){
      ggplot(subset(quad.plot.WC, Abb2 %in% c(input$checkGroup4) & Spp_type %in% c(input$checkGroup) & Assessment_Year %in% c(input$checkGroup2)), aes(B.Bmsy, TM_ABC, label=Abb2 ,color=Spp_type,shape=Spp_type)) + geom_point()+
        xlim(0,4)+
        ylim(0,1.5)+
        geom_text_repel(show.legend = FALSE,aes(label = Abb2))+
        theme_light()+
        theme(legend.title=element_blank())+
        labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (TM_ABC)")))+
        geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
        geom_hline(yintercept = 1,lty=2)+
        guides(shape = guide_legend(override.aes = list(size = 4)))
      
    }
    
    else {
      
      ggplot(subset(quad.plot.WC, Abb2 %in% c(input$checkGroup4) & Spp_type %in% c(input$checkGroup) & Assessment_Year %in% c(input$checkGroup2)), aes(B.Bmsy, TM_ABC, label=Abb2 ,color=Spp_type,shape=Spp_type)) + geom_point()+
        xlim(0,4)+
        ylim(0,1.5)+
        geom_path(aes(linetype=Abb2))+
        geom_text_repel(show.legend = FALSE,aes(label = paste("(",Abb2,",",Assessment_Year,")")))+
        theme_light()+
        theme(legend.title=element_blank())+
        labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (TM_ABC)")))+
        geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
        geom_hline(yintercept = 1,lty=2)+
        guides(shape = guide_legend(override.aes = list(size = 4)))
    }
  }    
    
  })

}
