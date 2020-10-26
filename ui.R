library(shiny)
library(shinyWidgets)
library(shinythemes)

quad.plot.WC <- data.frame(read.csv(paste0(getwd(),"/Fisheries_Updated_File.csv")))

fluidPage(theme = shinytheme("sandstone"),

  navbarPage("Fisheries Shiny App"),
   
  plotOutput(outputId = "distPlot"),
  
  
  fluidPage(style='padding-left:30px; padding-right:0px; padding-top:50px; padding-bottom:0px',
            
      radioGroupButtons(
        inputId = "Id073",
        label = "Fishing Intensity",
        choices = c("F/Fmsy", 
                    "TM_ABC"),
        individual = TRUE,
        size = "lg",
        status = "Secondary",
        
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: steelblue"))
      ),

      fluidRow(style='padding-left:15px; padding-right:0px; padding-top:0px; padding-bottom:10px',
                
      column(width = 1, offset = 0, style='padding-left:0px; padding-right:205px; padding-top:0px; padding-bottom:0px',
               
      dropdown(
  
      checkboxGroupButtons(
        inputId = "checkGroup",
        label = "Species Group",
        choices = unique(quad.plot.WC$Spp_type),
        selected = unique(quad.plot.WC$Spp_type),
        status = "Secondary",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"),
          
          no = icon("remove",
                    lib = "glyphicon"))
      ),
      
      status = "Secondary",
      style = "default",
      size = "lg",
      icon = NULL,
      label = "Species Type",
      tooltip = FALSE,
      right = FALSE,
      up = FALSE,
      width = NULL,
      animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.25),
      inputId = NULL
    ),
    
        ),
    
    #column(1, offset = 1,), 
    
    column(width = 1, offset = 0, style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
    
    dropdown(
    
      switchInput(
        inputId = "Switch1",
        onLabel = "All",
        offLabel = "None"
      ),
    
    awesomeCheckboxGroup(
      inputId = "checkGroup4",
      label = "Species", 
      choices = unique(quad.plot.WC$Abb2),
      selected = unique(quad.plot.WC$Abb2)),
    
    status = "Secondary",
    style = "default",
    size = "lg",
    icon = NULL,
    label = "Species",
    tooltip = FALSE,
    right = FALSE,
    up = FALSE,
    width = NULL,
    animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.25),
    inputId = NULL
    ),
    
    ),
    
    column(width = 1, offset = 0, style= 'padding-left:20px; padding-right:0px; padding-top:0px; padding-bottom:0px',
           
    dropdown(
      
      switchInput(
        inputId = "Switch2",
        onLabel = "All",
        offLabel = "None"
      ),

    awesomeCheckboxGroup(
      inputId = "checkGroup2",
      label = "Year", 
      choices = unique(quad.plot.WC$Asseslgent_Year),
      selected = "2019"),
    
    status = "Secondary",
    style = "default",
    size = "lg",
    icon = NULL,
    label = "Year",
    tooltip = FALSE,
    right = FALSE,
    up = FALSE,
    width = NULL,
    animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.25),
    inputId = NULL
    ),
    )),
   
              
  radioGroupButtons(
    inputId = "YearSpecies1",
    label = "Show Year Linkage",
    choices = c("Yes", 
                "No"),
    individual = TRUE,
    selected = "No",
    size = "lg",
    status = "Secondary",
    
    checkIcon = list(
      yes = tags$i(class = "fa fa-circle", 
                   style = "color: steelblue"),
      no = tags$i(class = "fa fa-circle-o", 
                  style = "color: steelblue"))
  
  )    
  )

  )