library(shiny)

quad.plot.WC <- data.frame(read.csv("https://raw.githubusercontent.com/Cave42/Fisheries-Shiny-App/main/Fisheries_Updated_File.csv"))

fluidPage(
  
  titlePanel("Fisheries Shiny App Draft"),
  
    sidebarLayout(
      
    sidebarPanel(
        
    # Species Type options for graph
    #checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
    #                   choices = list("Flatfishes", "Other", "Scorpaenids"), selected = list("Flatfishes", "Other", "Scorpaenids")),
    
    #Species Type options for graph
    checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                       choices = unique(quad.plot.WC$Spp_type), selected = unique(quad.plot.WC$Spp_type)),
      
    #Year options for graph
    checkboxGroupInput("checkGroup2", label = h3("Checkbox group"), 
                       choices = unique(quad.plot.WC$Assessment_Year), selected = unique(quad.plot.WC$Assessment_Year)),
    ),
    
    mainPanel(
      
  #Print the ggplot
  plotOutput(outputId = "distPlot")
  
  )
  )
  )