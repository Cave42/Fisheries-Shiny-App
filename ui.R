library(shiny)
library(shinyWidgets)

quad.plot.WC <- data.frame(read.csv(paste0(getwd(),"/Fisheries_Updated_File.csv")))

fluidPage(

  #theme = shinytheme("slate"),
  
  #titlePanel("Fisheries Shiny App Draft"),
  headerPanel("Fisheries Shiny App", windowTitle = "Fisheries Shiny App"),
        
    sidebarPanel(
        
      radioGroupButtons(
        inputId = "Id073",
        label = "Fishing Intensity",
        choices = c("F/Fmsy", 
                    "TM_ABC"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: steelblue"))
      ),
      
      radioGroupButtons(
        inputId = "YearSpecies1",
        label = "Show Year Linkage",
        choices = c("Yes", 
                    "No"),
        individual = TRUE,
        selected = "No",
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: steelblue"))
      ),
      
      dropdown(
      
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
      
      style = "default",
      status = "default",
      size = "md",
      icon = NULL,
      label = "Species Type",
      tooltip = FALSE,
      right = FALSE,
      up = FALSE,
      width = NULL,
      animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.25),
      inputId = NULL
    ),
    
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
    
    style = "default",
    status = "default",
    size = "md",
    icon = NULL,
    label = "Species",
    tooltip = FALSE,
    right = FALSE,
    up = FALSE,
    width = NULL,
    animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.25),
    inputId = NULL
    ),
    
    dropdown(
      
      switchInput(
        inputId = "Switch2",
        onLabel = "All",
        offLabel = "None"
      ),

    awesomeCheckboxGroup(
      inputId = "checkGroup2",
      label = "Year", 
      choices = unique(quad.plot.WC$Assessment_Year),
      selected = "2019"),
    
    style = "default",
    status = "default",
    size = "md",
    icon = NULL,
    label = "Year",
    tooltip = FALSE,
    right = FALSE,
    up = FALSE,
    width = NULL,
    animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.25),
    inputId = NULL
    )
    
                 
    ),
    
    mainPanel(
      
  #Print the ggplot
  plotOutput(outputId = "distPlot")
  
  )
  )