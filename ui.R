library(shiny)
library(shinyWidgets)
#library(shinythemes)
#library(shinydashboard)

#quad.plot.WC <- data.frame(read.csv("https://raw.githubusercontent.com/Cave42/Fisheries-Shiny-App/main/Fisheries_Updated_File.csv"))
quad.plot.WC <- data.frame(read.csv(paste0(getwd(),"/Fisheries_Updated_File.csv")))

#quad.plot.WC <- data.frame(read.csv("https://raw.githubusercontent.com/Cave42/Fisheries-Shiny-App/main/Fisheries_Updated_File.csv"))

fluidPage(

  #theme = shinytheme("slate"),
  
  #titlePanel("Fisheries Shiny App Draft"),
  #headerPanel("Fisheries Shiny App", windowTitle = "Fisheries Shiny App"),

    #sidebarLayout(
    
    #tags$h2("Dropdown Button"),
    br(),
    dropdownButton(
        
    #tags$h3("Graph Variables"),
        
    #sidebarPanel(
        

    # Species Type options for graph
    #checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
    #                   choices = list("Flatfishes", "Other", "Scorpaenids"), selected = list("Flatfishes", "Other", "Scorpaenids")),
    
    #Species Type options for graph
    #checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
     #                  choices = unique(quad.plot.WC$Spp_type), selected = unique(quad.plot.WC$Spp_type)),
      
    #Year options for graph
    #checkboxGroupInput("checkGroup2", label = h3("Checkbox group"), 
    #                   choices = unique(quad.plot.WC$Assessment_Year), selected = unique(quad.plot.WC$Assessment_Year)),

    #Species Type options for graph
    #checkboxGroupInput("checkGroup3", label = h3("Checkbox group"), 
     #                  choices = list("F.Fmsy", 
      #                                "TM_ABC"), selected = list("F.Fmsy"
       #                                                       )),
     
      radioGroupButtons(
        inputId = "Id073",
        label = "Y axis",
        choices = c("F/Fmsy", 
                    "TM_ABC"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: steelblue"))
      ),
      
      checkboxGroupButtons(
        inputId = "checkGroup",
        label = "Species Group",
        choices = unique(quad.plot.WC$Spp_type),
        selected = unique(quad.plot.WC$Spp_type),
        status = "primary",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon"))
      ),
      
    #Species Type options for graph
    #checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
    #                   choices = unique(quad.plot.WC$Spp_type), selected = unique(quad.plot.WC$Spp_type)),
    
    awesomeCheckboxGroup(
      inputId = "checkGroup2",
      label = "Year", 
      choices = unique(quad.plot.WC$Assessment_Year),
      selected = unique(quad.plot.WC$Assessment_Year)),
    #),
    
    circle = TRUE, status = "danger",
    icon = icon("gear"), width = "300px",
    
    tooltip = tooltipOptions(title = "Click to see inputs !")

    ),
    
    #Year options for graph
    #checkboxGroupInput("checkGroup2", label = h3("Checkbox group"), 
    #                   choices = unique(quad.plot.WC$Assessment_Year), selected = unique(quad.plot.WC$Assessment_Year)),
    #),
    
    mainPanel(
      
  #Print the ggplot
  plotOutput(outputId = "distPlot")
  
  )
  )