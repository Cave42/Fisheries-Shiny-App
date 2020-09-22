library(shiny)

fluidPage(
  
  titlePanel("Fisheries Shiny App Draft"),
  
    sidebarLayout(
      
    sidebarPanel(
        
    # Species Type options for graph
    checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                       choices = list("Flatfishes", "Other", "Scorpaenids"), selected = list("Flatfishes", "Other", "Scorpaenids")),
    
    #Year options for graph
    checkboxGroupInput("checkGroup2", label = h3("Checkbox group"), 
                       choices = list("2017", "2013", "2019"), selected = list("2017", "2013", "2019")),
    ),
    
    mainPanel(
      
  #Print the ggplot
  plotOutput(outputId = "distPlot")
  
  )
  )
  )