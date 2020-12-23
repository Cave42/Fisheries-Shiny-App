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
library(tidyverse)
library(transformr)
library(dplyr)
library(ggimage)
library(png)
library(shinycssloaders)
library(grid)
library(jpeg)

quad.plot.WC <- data.frame(read.csv(paste0(getwd(), "/Fisheries_Updated_File.csv")))

quad.plot.WCAnimation <- data.frame(read.csv(paste0(getwd(), "/Animation_Excel_File.csv")))

function(input, output, session) {

  observe({
    if (is.null(input$Flatfishes1) == FALSE) {
      updateAwesomeCheckbox(
        session = session,
        inputId = "Flatfishes2",
        value = list("Arrowtooth", "Dover", "English", "Petrale", "Rex", "StarryF"),
      )
    }

    else if (is.null(input$Flatfishes1) == TRUE) {
      updateAwesomeCheckbox(session = session,
        inputId = "Flatfishes2",
        value = list())
      }

    })

    observe({
      if (is.null(input$Flatfishes3) == FALSE) {
        updateAwesomeCheckbox(
          session = session,
          inputId = "Flatfishes4",
          value = list( "Arrowtooth", "Dover", "English", "Petrale", "Rex", "StarryF")
        )
      }

      else if (is.null(input$Flatfishes3) == TRUE) {
        updateAwesomeCheckbox(session = session,
          inputId = "Flatfishes4",
          value = list())
        }

      })

      observe({
        if (is.null(input$Scorpaenids1) == FALSE) {
          updateAwesomeCheckbox(
            session = session,
            inputId = "Scorpaenids2",
            value = list( "Aurora", "Black", "Black_CA", "Black_OR", "Black_WA", "Blackgill", "Blu_Dec_CA", "Blue_CA", "Bocaccio", "Brown", "CA_scorp", "Canary", "Chilipepper", "China_C" , "China_N" , "China_S", "Copper", "Cowcod", "Darkblotched", "Gopher", "Gopher_BLKYL", "Greenstriped", "LST", "POP", "Rougheye", "Scorpionfish", "Sharpchin", "Shortbelly", "Splitnose", "SST", "Widow", "Yelloweye", "Yellowtail" )
          )
        }

        else if (is.null(input$Scorpaenids1) == TRUE) {
          updateAwesomeCheckbox(session = session,
            inputId = "Scorpaenids2",
            value = list())
          }

        })

        observe({
          if (is.null(input$Scorpaenids3) == FALSE) {
            updateAwesomeCheckbox(
              session = session,
              inputId = "Scorpaenids4",
              value = list( "Aurora", "Black", "Black_CA", "Black_OR", "Black_WA", "Blackgill", "Blu_Dec_CA", "Blue_CA", "Bocaccio", "Brown", "CA_scorp", "Canary", "Chilipepper", "China_C" , "China_N" , "China_S", "Copper", "Cowcod", "Darkblotched", "Gopher", "Gopher_BLKYL", "Greenstriped", "LST", "POP", "Rougheye", "Scorpionfish", "Sharpchin", "Shortbelly", "Splitnose", "SST", "Widow", "Yelloweye", "Yellowtail" )
            )
          }

          else if (is.null(input$Scorpaenids3) == TRUE) {
            updateAwesomeCheckbox(session = session,
              inputId = "Scorpaenids4",
              value = list())
            }

          })

          observe({
            if (is.null(input$Other1) == FALSE) {
              updateAwesomeCheckbox(
                session = session,
                inputId = "Other2",
                value = list( "Big_skate", "Cabezon_CA", "Cabezon_NCA", "Cabezon_OR", "Cabezon_SCA", "Dogfish", "Kelp_greenling", "Lingcod", "Lingcod_N", "Lingcod_S", "Longnose", "P. Hake", "Sablefish", "SpinyD" )
              )
            }

            else if (is.null(input$Other1) == TRUE) {
              updateAwesomeCheckbox(session = session,
                inputId = "Other2",
                value = list())
              }
            })

            observe({
              if (is.null(input$Other3) == FALSE) {
                updateAwesomeCheckbox(
                  session = session,
                  inputId = "Other4",
                  value = list( "Big_skate", "Cabezon_CA", "Cabezon_NCA", "Cabezon_OR", "Cabezon_SCA", "Dogfish", "Kelp_greenling", "Lingcod", "Lingcod_N", "Lingcod_S", "Longnose", "P. Hake", "Sablefish", "SpinyD" )
                )
              }

              else if (is.null(input$Other3) == TRUE) {
                updateAwesomeCheckbox(session = session,
                  inputId = "Other4",
                  value = list())
                }
              })

              observe({
                if (input$Switch2 == TRUE) {
                  updateAwesomeCheckbox(
                    session = session,
                    inputId = "checkGroup2",
                    value = list( "1999", "2000", "2001", "2005", "2007", "2009", "2010", "2011", "2013", "2015", "2017", "2019" )
                  )
                }

                else if (input$Switch2 == FALSE) {
                  updateAwesomeCheckbox(session = session,
                    inputId = "checkGroup2",
                    value = list())
                  }

                })

                quad.plot.WC2 <- reactive({
                  a <-
                  subset(
                    quad.plot.WC,
                    Abb2 %in% c(input$Flatfishes2, input$Scorpaenids2, input$Other2) &
                    Assessment_Year %in% c(input$checkGroup2)

                    # group_by(Assessment_Year) %>%
                    #  filter(x == min(x))

                    #group_by(variable) %>%
                    #  mutate(color = (min(value) == value | max(value) == value))
                  )

                  return(a)

                })

                quad.plot.WCAnimation2 <- reactive({
                  b <-
                  subset(
                    quad.plot.WC,
                    Abb2 %in% c(input$Flatfishes2, input$Scorpaenids2, input$Other2) &
                    Assessment_Year %in% c(input$checkGroup2)
                  )

                  return(b)

                })

                output$distPlot <- renderPlot({

                  #quad.plot.WC3 <- quad.plot.WC2() %>% group_by(Abb2) %>%
                  #filter(Assessment_Year == min(Assessment_Year), max(Assessment_Year) == Assessment_Year)
                  #mutate(color = (min(Assessment_Year) == Assessment_Year | max(Assessment_Year) == Assessment_Year))
                  #mutate(color = (min(value) == value | max(value) == value))
                  #filter(
                  # MaxMassByGender = max(Assessment_Year, na.rm = T),
                  #  MinMassByGender = min(Assessment_Year, na.rm = T)
                  #) %>%
                  #  arrange(Abb2)

                  #quad.plot.WC3 <- quad.plot.WC2() %>% group_by(Abb2) %>%
                  #subset(min(Assessment_Year) == Assessment_Year | max(Assessment_Year) == Assessment_Year)
                  #filter(Assessment_Year == min(Assessment_Year))``

                  #quad.plot.WC3 <-
                  #subset(
                  #  quad.plot.WC2(),
                  #  Abb2 %in% (min(Assessment_Year)))

                    #g <- ggplot(quad.plot.WC3) +
                    g <- ggplot(quad.plot.WC2()) +
                    xlim(0, 4) +
                    ylim(0, 1.5) +
                    theme_light() +
                    geom_hline(yintercept = 1, lty = 2) +
                    theme(legend.title = element_blank()) +
                    guides(shape = guide_legend(override.aes = list(size = 4)))

                    #g <- g + geom_vline(
                    #  xintercept = c(0.5, 0.62, 1),
                    #    lty = c(1, 1, 2),
                    #    col = c("red", "orange", "black"),
                    #    lwd = c(1.25, 1.25, 1)
                    #  )
                    #  geom_hline(yintercept = 1, lty = 2) +
                    #  geom_text(aes(x=.5, label="\nFlatfish", y=.75), colour="red", angle=90, text=element_text(size=11)) +
                    #  geom_text(aes(x=.62, label="\nScorpaenids and Other", y=.75), colour="orange", angle=90, text=element_text(size=11))

                    vlineVariables <- as.list(strsplit(input$userInput, ",")[[1]])
                    vlineColors <- as.list(strsplit(input$userInput2, ",")[[1]])
                    vlineNames <- as.list(strsplit(input$userInput3, ",")[[1]])

                    g <- g + geom_vline(
                      xintercept = c(as.numeric(unlist(strsplit(input$userInput, ","))),1),
                      #lty = c(1, 1, 2),
                      col = c(unlist(strsplit(input$userInput2, ",")), "black"),
                      #c(unlist(strsplit(input$userInput2, ",")))

                      #input$userInput
                      #lwd = c(1.25, 1.25, 1)
                    ) +

                    #for (i in 1:2) {

                    #g <- g + geom_text(aes(x= as.numeric(vlineVariables[[i]]), y= .75), label= paste("test",i))

                    #}
                    #print(as.numeric(vlineVariables[[i]]))

                    #, colour="red", angle=90, text=element_text(size=11))


                    geom_text(aes(x= as.numeric(vlineVariables[[1]])+.01, label=vlineNames[[1]], y=.75), colour=vlineColors[[1]], angle=90, text=element_text(size=11)) +
                    geom_text(aes(x= as.numeric(vlineVariables[[2]])+.01, label=vlineNames[[2]], y=.75), colour=vlineColors[[2]], angle=90, text=element_text(size=11))+
                    if(vlineVariables[[3]] != "$"){
                      geom_text(aes(x= as.numeric(vlineVariables[[3]])+.01, label=vlineNames[[3]], y=.75), colour=vlineColors[[3]], angle=90, text=element_text(size=11))
                    }

                    if(vlineVariables[[4]] != "$"){
                      g <- g + geom_text(aes(x= as.numeric(vlineVariables[[4]])+.01, label=vlineNames[[4]], y=.75), colour=vlineColors[[4]], angle=90, text=element_text(size=11))
                    }

                    if(vlineVariables[[5]] != "$"){
                      g <- g + geom_text(aes(x= as.numeric(vlineVariables[[5]])+.01, label=vlineNames[[5]], y=.75), colour=vlineColors[[5]], angle=90, text=element_text(size=11))
                    }

                    #geom_text(aes(inherit.aes = FALSE, x= c(as.numeric(unlist(strsplit(input$userInput, ",")))+.01), label= c((unlist(strsplit(input$userInput3, ",")))), y=.75), color = c(unlist(strsplit(input$userInput2, ","))), angle= 90, text=element_text(size=11))
                    #geom_text(aes(x=.62, label="\nScorpaenids and Other", y=.75), colour="orange", angle=90, text=element_text(size=11))


                    if (input$Id073 == "F/Fmsy") {

                      g <- g + aes(B.Bmsy, F.Fmsy) + labs(x = expression(bold("Relative Stock Status")), y = expression(bold("Fishing Intensity (F/Fmsy)")))
                    }

                    else if (input$Id073 == "TM_ABC") {

                      g <- g + aes(B.Bmsy, TM_ABC) + labs(x = expression(bold("Relative Stock Status")), y = expression(bold("Fishing Intensity (TM_ABC)")))
                    }

                    if (all(c("Images") %in% input$ImageOptions)) {

                      if(input$imageSize == "S"){
                        g <- g + geom_image(aes(image = Image), size = .01, asp = 30 / 9) + scale_size_identity()
                      }

                      if(input$imageSize == "M"){
                        g <- g + geom_image(aes(image = Image), size = .02, asp = 30 / 9) + scale_size_identity()
                      }

                      if(input$imageSize == "L"){
                        g <- g + geom_image(aes(image = Image), size = .03, asp = 30 / 9) + scale_size_identity()
                      }

                    }

                    else{

                      if(input$pointSize == "S"){
                        g <- g + geom_point(aes(color = Spp_type, shape = Spp_type), size = .1)
                      }

                      if(input$pointSize == "M"){
                        g <- g + geom_point(aes(color = Spp_type, shape = Spp_type), size = 3)
                      }

                      if(input$pointSize == "L"){
                        g <- g + geom_point(aes(color = Spp_type, shape = Spp_type), size = 5)
                      }

                    }

                    if (all(c("Year Labels") %in% input$ImageOptions) && all(c("Species labels") %in% input$ImageOptions)) {
                      g <-
                      g + aes(label = Abb2) + geom_text_repel(show.legend = FALSE, aes(label = paste(Abb2, ";", Assessment_Year), color = Spp_type))
                    }

                    else if (all(c("Species labels") %in% input$ImageOptions)) {
                      g <-
                      g + aes(label = Abb2) + geom_text_repel(show.legend = FALSE, aes(label = Abb2, color = Spp_type))
                    }

                    else if (all(c("Year Labels") %in% input$ImageOptions)) {
                      g <-
                      g + aes(label = Abb2) + geom_text_repel(show.legend = FALSE, aes(label = Assessment_Year, color = Spp_type))
                    }

                    if (input$YearSpecies1 == 1) {
                      g <-
                      g + geom_path(aes(
                        linetype = Abb2,
                        group = (Abb2),
                        color = Spp_type
                      ))
                    }

                    if(input$squarePlot == 1){
                      g <- g + coord_fixed(ratio = 2.5)
                    }

                    if(input$DownloadPlot == 1){
                      ggsave(filename="myPlot.png", plot=g)
                    }

                    g

                  })

                  output$distPlot2 <- renderPlotly({

                    if (input$Id074 == "F/Fmsy") {
                      Var = ~F.Fmsy
                      var4 = ~paste(Abb2,";", Assessment_Year2)
                    }

                    else if (input$Id074 == "TM_ABC") {
                      Var = ~TM_ABC
                      var4 = ~paste(Abb2,",", Assessment_Year3)
                    }

                    if (all(c("Legend") %in% input$graphOptions)){

                      Var2 = T

                    }

                    else {Var2 = F}

                    if(input$pointSize2 == "S"){
                      pointSze =5
                    }
                    else if(input$pointSize2 == "M"){
                      pointSze =10
                    }
                    else if(input$pointSize2 == "L"){
                      pointSze =15
                    }

                    p <-
                    #quad.plot.WCAnimation %>%
                    plot_ly(
                      #quad.plot.WCAnimation,
                      subset(quad.plot.WCAnimation, Abb2 %in% c(input$Flatfishes4, input$Scorpaenids4, input$Other4)),
                      x = ~B.Bmsy,
                      y = Var,
                      #size = ~pop,
                      color = ~Abb2,
                      text = var4,
                      frame = ~Assessment_Year,

                      marker = list(
                        size = pointSze,
                        line = list(color = 'black', width = 2)),
                        #hoverinfo = "text",
                        type = 'scatter',
                        mode = 'markers',
                        showlegend = Var2,
                        symbol = ~Spp_type
                      )

                      if (input$Id074 == "F/Fmsy") {
                        p <-
                        p %>% layout(
                          xaxis = list(title = "Relative Stock Status"),
                          yaxis = list(title = "Fishing Intensity (F/Fmsy)")
                        )

                      }

                      else if (input$Id074 == "TM_ABC") {
                        p <-
                        p %>% layout(
                          xaxis = list(title = "Relative Stock Status"),
                          yaxis = list(title = "Fishing Intensity (TM_ABC)")
                        )

                      }

                      if (all(c("Species labels") %in% input$graphOptions)){

                        p <- p %>% add_text(textposition = "top right", color = ~Abb2)

                      }



                      p <- p %>% layout(xaxis = list(range = c(0, 4)),
                      yaxis = list(range = c(0, 1.5)))

                      p <- p %>%
                      layout(
                        xaxis = list(
                          dtick = 1,
                          tick0 = 0,
                          tickmode = "linear"
                        ),

                        yaxis = list(
                          dtick = 0.5,
                          tick0 = 0,
                          tickmode = "linear"
                        )
                      )

                      m <- list(
                        l = 50,
                        r = 50,
                        b = 100,
                        t = 100,
                        pad = 4
                      )

                      if (input$squarePlot2 == T){
                        p <- p %>% layout(autosize = F, width = 400, height = 400)
                      }

                      p <- p %>% add_annotations(x = .51, y = .75, text = "Flatfish", font = list(color = "red"), showarrow = F, textangle = 270)

                      p <- p %>% add_annotations(x = .63, y = .75, text = "Scorpaenids and Other", font = list(color = "orange"), showarrow = F, textangle = 270)

                      vline1 <- function(x = 0, color = "orange") {
                        list(
                          type = "line",
                          y0 = 0,
                          y1 = 1,
                          yref = "paper",
                          x0 = .62,
                          x1 = .62,
                          line = list(color = color)
                        )
                      }

                      vline2 <- function(x = 0, color = "red") {
                        list(
                          type = "line",
                          y0 = 0,
                          y1 = 1,
                          yref = "paper",
                          x0 = .5,
                          x1 = .5,
                          line = list(color = color)
                        )
                      }

                      vline <- function(x = 0, color = "black") {
                        list(
                          type = "line",
                          y0 = 0,
                          y1 = 1,
                          yref = "paper",
                          x0 = 1,
                          x1 = 1,
                          line = list(color = color)
                        )
                      }

                      hline <- function(y = 0, color = "black") {
                        list(
                          type = "line",
                          x0 = 0,
                          x1 = 1,
                          xref = "paper",
                          y0 = 1,
                          y1 = 1,
                          line = list(color = color)
                        )

                        #  g <- g + geom_vline(
                        #xintercept = c(as.numeric(unlist(strsplit(input$userInput, ","))),1),
                        #lty = c(1, 1, 2),
                        # col = c(unlist(strsplit(input$userInput2, ",")), "black"),
                        #c(unlist(strsplit(input$userInput2, ",")))

                        #input$userInput2
                        #lwd = c(1.25, 1.25, 1)
                        #)

                        #geom_text(aes(x=.5, label="\nFlatfish", y=.75), colour="red", angle=90, text=element_text(size=11)) +
                        #geom_text(aes(x=.62, label="\nScorpaenids and Other", y=.75), colour="orange", angle=90, text=element_text(size=11))

                        #geom_text(aes(x= c(as.numeric(unlist(strsplit(input$userInput, ",")))+.01), label= c((unlist(strsplit(input$userInput3, ",")))), y=.75), color = c(unlist(strsplit(input$userInput2, ","))), angle= 90, text=element_text(size=11))
                        #geom_text(aes(x=.62, label="\nScorpaenids and Other", y=.75), colour="orange", angle=90, text=element_text(size=11))

                      }

                      p <- p %>%
                      layout(shapes = list(vline1(4), vline2(4), vline(4), hline(5)))

                      p <- p %>%
                      animation_opts(1000, redraw = FALSE)
                      # easing = "linear",

                      htmlwidgets::saveWidget(p, "index.html")

                      p
                    })
                  }
