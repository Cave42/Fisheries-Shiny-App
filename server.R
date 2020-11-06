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
library(plotly)
library(ggrepel)
library(gganimate)
library(gifski)
library(tidyverse)
library(animation)
library(transformr)
library(dplyr)

library(ggimage)
library(png)
library(grid)

quad.plot.WC <- data.frame(read.csv(paste0(getwd(),"/Fisheries_Updated_File.csv")))

function(input, output, session){

observe({

  if (is.null(input$Flatfishes1) == FALSE){updateAwesomeCheckbox(session = session, inputId = "Flatfishes2", value = list("Arrowtooth", "Dover", "English", "Petrale", "Rex", "StarryF")) }

  else if (is.null(input$Flatfishes1) == TRUE){updateAwesomeCheckbox(session = session, inputId = "Flatfishes2", value = list())}

  if (is.null(input$Scorpaenids1) == FALSE){updateAwesomeCheckbox(session = session, inputId = "Scorpaenids2", value = list("Aurora", "Black", "Black_CA", "Black_OR", "Black_WA", "Blackgill", "Blu_Dec_CA", "Blue_CA", "Bocaccio", "Brown", "CA_scorp", "Canary", "Chilipepper", "China_C" ,"China_N" ,"China_S", "Copper", "Cowcod", "Darkblotched", "Gopher", "Gopher_BLKYL",                                                                                                                           "Greenstriped", "LST", "POP", "Rougheye", "Scorpionfish", "Sharpchin", "Shortbelly", "Splitnose", "SST", "Widow","Yelloweye")) }

  else if (is.null(input$Scorpaenids1) == TRUE){updateAwesomeCheckbox(session = session, inputId = "Scorpaenids2", value = list()) }

  if (is.null(input$Other1) == FALSE){updateAwesomeCheckbox(session = session, inputId = "Other2", value = list("Big_skate", "Cabezon_CA", "Cabezon_NCA", "Cabezon_OR", "Cabezon_SCA", "Dogfish", "Kelp_greenling", "Lingcod", "Lingcod_N", "Lingcod_S", "Longnose", "P. Hake", "Sablefish", "SpinyD")) }

  else if (is.null(input$Other1) == TRUE){updateAwesomeCheckbox(session = session, inputId = "Other2", value = list())}
  
  if (input$Switch2 == TRUE){updateAwesomeCheckbox(session = session, inputId = "checkGroup2", value = list("1999", "2000", "2001", "2005", "2007", "2009", "2010", "2011", "2013", "2015","2017", "2019")) }
  
  else if (input$Switch2 == FALSE){updateAwesomeCheckbox(session = session, inputId = "checkGroup2", value = list())}

})

  quad.plot.WC2 <- reactive({

    a <- subset(quad.plot.WC,Abb2 %in% c(input$Flatfishes2, input$Scorpaenids2, input$Other2) & Assessment_Year %in% c(input$checkGroup2))
    
    return(a)

  })

  output$distPlot <- renderPlot({
  
   # outfile <- tempfile(fileext='.gif')
    
    if (input$Id073 == "F/Fmsy"){
      
      if (input$YearSpecies1 == 0){
        
      ggplot(quad.plot.WC2())+
        aes(B.Bmsy, F.Fmsy, label=Abb2, image = Image)+
          #color=Spp_type,shape=Spp_type)) + 
          #geom_point()+
        geom_image(size= .05, asp = 30 / 9) + 
          scale_size_identity()+
          xlim(0,4)+
          ylim(0,1.5)+
          theme_light()+
          geom_text_repel(show.legend = FALSE,aes(label = Abb2, color = Spp_type))+
          theme(legend.title=element_blank())+
          labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (F/Fmsy)")))+
          geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
          geom_hline(yintercept = 1,lty=2)+
          guides(shape = guide_legend(override.aes = list(size = 4)))
      }
      
      else if (input$YearSpecies1 == 1){
        
        ggplot(quad.plot.WC2())+
          aes(B.Bmsy, F.Fmsy, label=Abb2, image = Image)+
          #color=Spp_type,shape=Spp_type)) + 
          #geom_point()+
          geom_image(size= .05, asp = 30 / 9) + 
          scale_size_identity()+
          xlim(0,4)+
          ylim(0,1.5)+
          theme_light()+
          geom_text_repel(show.legend = FALSE,aes(label = paste("(",Abb2,",",Assessment_Year,")"), color = Spp_type))+
          theme(legend.title=element_blank())+
          labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (F/Fmsy)")))+
          geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
          geom_hline(yintercept = 1,lty=2)+
          guides(shape = guide_legend(override.aes = list(size = 4)))+
          geom_path(aes(linetype=Abb2, group = (Abb2), color = Spp_type))
       
      }
    }
  
      else if (input$Id073 == "TM_ABC"){
        
        if (input$YearSpecies1 == 0){
          
          ggplot(quad.plot.WC2())+
            aes(B.Bmsy, TM_ABC, label=Abb2, image = Image)+
            #color=Spp_type,shape=Spp_type)) + 
            #geom_point()+
            geom_image(size= .05, asp = 30 / 9) + 
            scale_size_identity()+
            xlim(0,4)+
            ylim(0,1.5)+
            theme_light()+
            geom_text_repel(show.legend = FALSE,aes(label = Abb2, color = Spp_type))+
            theme(legend.title=element_blank())+
            labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (TM_ABC)")))+
            geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
            geom_hline(yintercept = 1,lty=2)+
            guides(shape = guide_legend(override.aes = list(size = 4)))
        }
        
        else if (input$YearSpecies1 == 1){
          
          ggplot(quad.plot.WC2())+
            aes(B.Bmsy, TM_ABC, label=Abb2, image = Image)+
            #color=Spp_type,shape=Spp_type)) + 
            #geom_point()+
            geom_image(size= .05, asp = 30 / 9) + 
            scale_size_identity()+
            xlim(0,4)+
            ylim(0,1.5)+
            theme_light()+
            geom_text_repel(show.legend = FALSE,aes(label = paste("(",Abb2,",",Assessment_Year,")"), color = Spp_type))+
            theme(legend.title=element_blank())+
            labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (TM_ABC)")))+
            geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
            geom_hline(yintercept = 1,lty=2)+
            guides(shape = guide_legend(override.aes = list(size = 4)))+
            geom_path(aes(linetype=Abb2, group = (Abb2), color = Spp_type)) 
          
        }
      
}
  
})

output$distPlot2 <- renderPlotly({
  
  if (input$Id073 == "F/Fmsy"){
    
    if (input$YearSpecies1 == 0){
      
    p <- ggplot(quad.plot.WC2())+
      aes(B.Bmsy, F.Fmsy, label=Abb2, color=Spp_type,shape=Spp_type) + 
      #geom_point(aes(frame = Assessment_Year))+
      geom_point()+
      #geom_image(size= .05, asp = 30 / 9) + 
      #scale_size_identity()+
      xlim(0,4)+
      ylim(0,1.5)+
      geom_text(show.legend = FALSE,aes(label = Abb2))+
      theme_light()+
      theme(legend.title=element_blank())+
      #labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (F/Fmsy)")))+
      geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
      geom_hline(yintercept = 1,lty=2)+
      guides(shape = guide_legend(override.aes = list(size = 4)))
 
    ggplotly(p)
    }
    
    else if (input$YearSpecies1 == 1){
      
      p <- ggplot(quad.plot.WC2())+
        aes(B.Bmsy, F.Fmsy, label=Abb2, color=Spp_type,shape=Spp_type, frame = Assessment_Year) + 
        #geom_point(aes(frame = Assessment_Year))+
        geom_point()+
        geom_path(aes(linetype=Abb2, group = (Abb2), color = Spp_type))+
        #geom_image(size= .05, asp = 30 / 9) + 
        #scale_size_identity()+
        #frame = aes(Assessment_Year) +
        xlim(0,4)+
        ylim(0,1.5)+
        geom_text(show.legend = FALSE,aes(label = Abb2))+
        theme_light()+
        theme(legend.title=element_blank())+
        #labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (F/Fmsy)")))+
        geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
        geom_hline(yintercept = 1,lty=2)+
        guides(shape = guide_legend(override.aes = list(size = 4)))

      ggplotly(p)
    }
    
  }
  
  else if (input$Id073 == "TM_ABC"){
    
    if (input$YearSpecies1 == 0){
      
    p <- ggplot(quad.plot.WC2())+
      aes(B.Bmsy, TM_ABC, label=Abb2, color=Spp_type,shape=Spp_type) + 
      #geom_point(aes(frame = Assessment_Year))+
      geom_point()+
      #geom_image(size= .05, asp = 30 / 9) + 
      #scale_size_identity()+
      xlim(0,4)+
      ylim(0,1.5)+
      geom_text(show.legend = FALSE,aes(label = Abb2))+
      theme_light()+
      theme(legend.title=element_blank())+
      #labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (F/Fmsy)")))+
      geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
      geom_hline(yintercept = 1,lty=2)+
      guides(shape = guide_legend(override.aes = list(size = 4)))
    
    ggplotly(p)
  
      }
  
    else if (input$YearSpecies1 == 1){
      
      p <- ggplot(quad.plot.WC2())+
        aes(B.Bmsy, TM_ABC, label=Abb2, color=Spp_type,shape=Spp_type, frame = Assessment_Year) + 
        #geom_point(aes(frame = Assessment_Year))+
        geom_point()+
        geom_path(aes(linetype=Abb2, group = (Abb2), color = Spp_type))+
        #geom_image(size= .05, asp = 30 / 9) + 
        #scale_size_identity()+
        xlim(0,4)+
        ylim(0,1.5)+
        geom_text(show.legend = FALSE,aes(label = Abb2))+
        theme_light()+
        theme(legend.title=element_blank())+
        #labs(x=expression(bold("Relative Stock Status")),y=expression(bold("Fishing Intensity (F/Fmsy)")))+
        geom_vline(xintercept = c(0.5,0.62,1),lty=c(1,1,2),col=c("red","orange","black"),lwd=c(1.25,1.25,1))+
        geom_hline(yintercept = 1,lty=2)+
        guides(shape = guide_legend(override.aes = list(size = 4)))
      
      
      
      ggplotly(p)
    }
    
    }
  
  


})
}
 
