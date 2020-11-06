library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)

quad.plot.WC <- data.frame(read.csv(paste0(getwd(),"/Fisheries_Updated_File.csv")))

fluidPage(theme = shinytheme("sandstone"),

  navbarPage("Fisheries Shiny App"),
  
  #imageOutput("plot1"))

  plotOutput(outputId = "distPlot"),
  plotlyOutput(outputId = "distPlot2"),
  
  fluidPage(style='padding-left:35px; padding-right:0px; padding-top:0px; padding-bottom:0px',
    
    column(width = 1, offset = 0, style='padding-left:0px; padding-right:0px; padding-top:25px; padding-bottom:0px',
    
    dropdown(
    
      style = "default",
      size = "normal",
      icon = NULL,
      label = "Species",
      tooltip = FALSE,
      right = FALSE,
      up = FALSE,
      width = NULL,
      animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.25),
      inputId = NULL,
        
      column(width = 1, offset = 0, style='padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',
             
        checkboxGroupButtons(
        inputId = "Flatfishes1",
        #label = "Flatfishes",
          choices = "Flatfishes",
          selected = NULL,
          status = "Secondary",
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"),
            
            no = icon("remove",
                      lib = "glyphicon"))
        ),
    
      awesomeCheckboxGroup(
        inputId = "Flatfishes2",
        label = "Flatfishes", 
        choices = list("Arrowtooth", "Dover", "English", "Petrale", "Rex", "StarryF"),
        selected = NULL),
      ),
      
      column(width = 1, offset = 0, style='padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',
             
      checkboxGroupButtons(
        inputId = "Scorpaenids1",
        choices = "Scorpaenids",
        selected = NULL,
        status = "Secondary",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"),
          
          no = icon("remove",
                    lib = "glyphicon"))
      ),
      
      awesomeCheckboxGroup(
        inputId = "Scorpaenids2",
        label = "Scorpaenids", 
        choices = list("Aurora", "Black", "Black_CA", "Black_OR", "Black_WA", "Blackgill", "Blu_Dec_CA", "Blue_CA", "Bocaccio", "Brown", "CA_scorp", "Canary", "Chilipepper", "China_C" ,"China_N" ,"China_S", "Copper", "Cowcod", "Darkblotched", "Gopher", "Gopher_BLKYL",
                       "Greenstriped", "LST", "POP", "Rougheye", "Scorpionfish", "Sharpchin", "Shortbelly", "Splitnose", "SST", "Widow","Yelloweye", "Yellowtail"),
        selected = NULL),
      
      ),
      
      column(width = 1, offset = 0, style='padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',
             
      checkboxGroupButtons(
        inputId = "Other1",
        choices = "Other",
        selected = NULL,
        status = "Secondary",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"),
          
          no = icon("remove",
                    lib = "glyphicon"))
      ),
      
      awesomeCheckboxGroup(
      inputId = "Other2",
      label = "Other", 
      choices = list("Big_skate", "Cabezon_CA", "Cabezon_NCA", "Cabezon_OR", "Cabezon_SCA", "Dogfish", "Kelp_greenling", "Lingcod", "Lingcod_N", "Lingcod_S", "Longnose", "P. Hake", "Sablefish", "SpinyD"),
      selected = NULL)
      )
    )
    ),
    
    
    column(width = 1, offset = 0, style= 'padding-left:15px; padding-right:0px; padding-top:25px; padding-bottom:0px',
           
    dropdown(
      
      switchInput(
        inputId = "Switch2",
        onLabel = "All",
        offLabel = "None"
      ),

    awesomeCheckboxGroup(
      inputId = "checkGroup2",
      label = "Year", 
      choices = list("1999", "2000", "2001", "2005", "2007", "2009", "2010", "2011", "2013", "2015","2017", "2019"),
      selected = NULL),
    
    style = "default",
    size = "normal",
    icon = NULL,
    label = "Year",
    tooltip = FALSE,
    right = FALSE,
    up = FALSE,
    width = NULL,
    animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.25),
    inputId = NULL
    ),
    ),
   
             
    column(width = 1, offset = 0, style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
    
    pickerInput(
      inputId = "Id073",
      label = "Y Axis", 
      choices = c("F/Fmsy", "TM_ABC"),
    ),
    ),
    
  column(width = 5, offset = 0, style='padding-left:80px; padding-right:0px; padding-top:37px; padding-bottom:0px',           
              
  prettyCheckbox(
    inputId = "YearSpecies1",
    label = "Year Species Linkage", 
    value = FALSE,
    shape = c("curve"),
    animation = "smooth",
    bigger = TRUE,

  )
  
  )
  
  )) 