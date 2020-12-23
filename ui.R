library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(plotly)

quad.plot.WC <- data.frame(read.csv(paste0(getwd(), "/Fisheries_Updated_File.csv")))

fluidPage(
  theme = shinytheme("sandstone"),

  navbarPage("Fisheries Shiny App"),

  tabsetPanel( type = "tabs", tabPanel("Plot", plotOutput(outputId = "distPlot") %>% withSpinner(color="#0dc5c1"),

  fluidPage( style = 'padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',

  column( width = 1, offset = 0, style = 'padding-left:35px; padding-right:0px; padding-top:25px; padding-bottom:0px',

  dropdown(
    style = "default",
    size = "normal",
    icon = NULL,
    label = "Species",
    tooltip = FALSE,
    right = FALSE,
    up = FALSE,
    width = NULL,
    animate = animateOptions(
      enter = "fadeInDown",
      exit = "fadeOutUp",
      duration = 0.25
    ),
    inputId = NULL,

    column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

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
        lib = "glyphicon")
      )
    ),

    awesomeCheckboxGroup(
      inputId = "Flatfishes2",
      label = "Flatfishes",
      choices = list("Arrowtooth", "Dover", "English", "Petrale", "Rex", "StarryF"),
      selected = NULL
    ),
  ),

  column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

  checkboxGroupButtons(
    inputId = "Scorpaenids1",
    choices = "Scorpaenids",
    selected = NULL,
    status = "Secondary",
    checkIcon = list(
      yes = icon("ok",
      lib = "glyphicon"),

      no = icon("remove",
      lib = "glyphicon")
    )
  ),

  awesomeCheckboxGroup(
    inputId = "Scorpaenids2",
    label = "Scorpaenids",
    choices = list( "Aurora", "Black", "Black_CA", "Black_OR", "Black_WA", "Blackgill", "Blu_Dec_CA", "Blue_CA", "Bocaccio", "Brown", "CA_scorp", "Canary", "Chilipepper", "China_C" , "China_N" , "China_S", "Copper", "Cowcod", "Darkblotched", "Gopher", "Gopher_BLKYL", "Greenstriped", "LST", "POP", "Rougheye", "Scorpionfish", "Sharpchin", "Shortbelly", "Splitnose", "SST", "Widow", "Yelloweye", "Yellowtail" ),
    selected = NULL
  ),

),

column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

checkboxGroupButtons(
  inputId = "Other1",
  choices = "Other",
  selected = NULL,
  status = "Secondary",
  checkIcon = list(
    yes = icon("ok",
    lib = "glyphicon"),

    no = icon("remove",
    lib = "glyphicon")
  )
),

awesomeCheckboxGroup(
  inputId = "Other2",
  label = "Other",
  choices = list( "Big_skate", "Cabezon_CA", "Cabezon_NCA", "Cabezon_OR", "Cabezon_SCA", "Dogfish", "Kelp_greenling", "Lingcod", "Lingcod_N", "Lingcod_S", "Longnose", "P. Hake", "Sablefish", "SpinyD" ),
  selected = NULL
)
)
)
),


column( width = 1, offset = 0, style = 'padding-left:15px; padding-right:0px; padding-top:25px; padding-bottom:0px',

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
    selected = NULL
  ),

  style = "default",
  size = "normal",
  icon = NULL,
  label = "Year",
  tooltip = FALSE,
  right = FALSE,
  up = FALSE,
  width = NULL,
  animate = animateOptions(
    enter = "fadeInDown",
    exit = "fadeOutUp",
    duration = 0.25
  ),
  inputId = NULL
),
),

column( width = 2, offset = 0, style = 'padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',

pickerInput(
  inputId = "Id0731",
  label = "X Axis",
  choices = c("B/BMSY proxy (Btarget)", "B/BMSY", "B/SPR target", "B/B0")
),
),

column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',

pickerInput(
  inputId = "Id073",
  label = "Y Axis",
  choices = c("F/Fmsy", "TM_ABC", "TM/OFL", "F/FMSY proxy", "(1-SPR)/(1-SPRtarget)"),
),
),

column( width = 1, offset = 0, style = 'padding-left:60px; padding-right:0px; padding-top:25px; padding-bottom:0px',

dropdown(
  awesomeCheckboxGroup(
    inputId = "ImageOptions",
    label = "Label Options",
    choices = list("Images", "Species labels", "Year Labels"),
    selected = list("Species labels")
  ),

  radioGroupButtons(
    inputId = "imageSize",
    label = "Image Size",
    choices = c("S",
    "M", "L"),
    selected = "M"
  ),

  radioGroupButtons(
    inputId = "pointSize",
    label = "Point Size",
    choices = c("S","M", "L"),
    selected = "M"
  ),

  style = "default",
  size = "normal",
  icon = NULL,
  label = "Label Options",
  tooltip = FALSE,
  right = FALSE,
  up = FALSE,
  width = NULL,
  animate = animateOptions(
    enter = "fadeInDown",
    exit = "fadeOutUp",
    duration = 0.25
  ),
  inputId = NULL
),
),

column( width = 1, offset = 0, style = 'padding-left:80px; padding-right:0px; padding-top:37px; padding-bottom:0px',

prettyCheckbox(
  inputId = "YearSpecies1",
  label = "Year Species Linkage",
  value = FALSE,
  shape = c("curve"),
  animation = "smooth",
  bigger = TRUE,

)
),

column( width = 2, offset = 0, style = 'padding-left:120px; padding-right:0px; padding-top:0px; padding-bottom:0px',
textInput(inputId = "userInput", label = "Position of Vline", value = "0.5, .62, $, $, $")
),

column( width = 2, offset = 0, style = 'padding-left:25px; padding-right:0px; padding-top:0px; padding-bottom:0px',
textInput(inputId = "userInput2", label = "Color of vline", value = " red, orange, blue, blue, blue ")
),

column( width = 3, offset = 0, style = 'padding-left:35px; padding-right:0px; padding-top:0px; padding-bottom:0px',
textInput(inputId = "userInput3", label = "Label of vline", value = "Flatfish, Scorpaenids and Other, $, $, $ ")

),

column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:0px; padding-top:37px; padding-bottom:0px',

prettyCheckbox(
  inputId = "DownloadPlot",
  label = "Download Plot",
  value = FALSE,
  shape = c("curve"),
  animation = "smooth",
  bigger = TRUE,

)
),

column( width = 1, offset = 0, style = 'padding-left:20px; padding-right:0px; padding-top:37px; padding-bottom:0px',

prettyCheckbox(
  inputId = "squarePlot",
  label = "Square Plot",
  value = FALSE,
  shape = c("curve"),
  animation = "smooth",
  bigger = TRUE,

)
),

)

),

tabPanel("Animation", plotlyOutput(outputId = "distPlot2") %>% withSpinner(color="#0dc5c1"),

fluidPage( style = 'padding-left:35px; padding-right:0px; padding-top:0px; padding-bottom:0px',

column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:0px; padding-top:25px; padding-bottom:0px',

dropdown(
  style = "default",
  size = "normal",
  icon = NULL,
  label = "Species",
  tooltip = FALSE,
  right = FALSE,
  up = FALSE,
  width = NULL,
  animate = animateOptions(
    enter = "fadeInDown",
    exit = "fadeOutUp",
    duration = 0.25
  ),
  inputId = NULL,

  column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

  checkboxGroupButtons(
    inputId = "Flatfishes3",
    #label = "Flatfishes",
    choices = "Flatfishes",
    selected = NULL,
    status = "Secondary",
    checkIcon = list(
      yes = icon("ok",
      lib = "glyphicon"),

      no = icon("remove",
      lib = "glyphicon")
    )
  ),

  awesomeCheckboxGroup(
    inputId = "Flatfishes4",
    label = "Flatfishes",
    choices = list("Arrowtooth", "Dover", "English", "Petrale", "Rex", "StarryF"),
    selected = NULL
  ),
),

column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

checkboxGroupButtons(
  inputId = "Scorpaenids3",
  choices = "Scorpaenids",
  selected = NULL,
  status = "Secondary",
  checkIcon = list(
    yes = icon("ok",
    lib = "glyphicon"),

    no = icon("remove",
    lib = "glyphicon")
  )
),

awesomeCheckboxGroup(
  inputId = "Scorpaenids4",
  label = "Scorpaenids",
  choices = list( "Aurora", "Black", "Black_CA", "Black_OR", "Black_WA", "Blackgill", "Blu_Dec_CA", "Blue_CA", "Bocaccio", "Brown", "CA_scorp", "Canary", "Chilipepper", "China_C" , "China_N" , "China_S", "Copper", "Cowcod", "Darkblotched", "Gopher", "Gopher_BLKYL", "Greenstriped", "LST", "POP", "Rougheye", "Scorpionfish", "Sharpchin", "Shortbelly", "Splitnose", "SST", "Widow", "Yelloweye", "Yellowtail" ),
  selected = NULL
),

),

column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

checkboxGroupButtons(
  inputId = "Other3",
  choices = "Other",
  selected = NULL,
  status = "Secondary",
  checkIcon = list(
    yes = icon("ok",
    lib = "glyphicon"),

    no = icon("remove",
    lib = "glyphicon")
  )
),

awesomeCheckboxGroup(
  inputId = "Other4",
  label = "Other",
  choices = list( "Big_skate", "Cabezon_CA", "Cabezon_NCA", "Cabezon_OR", "Cabezon_SCA", "Dogfish", "Kelp_greenling", "Lingcod", "Lingcod_N", "Lingcod_S", "Longnose", "P. Hake", "Sablefish", "SpinyD" ),
  selected = NULL
)
)
)
),

column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',

pickerInput(
  inputId = "Id074",
  label = "Y Axis",
  choices = c("F/Fmsy", "TM_ABC", "TM/OFL", "F/FMSY proxy", "(1-SPR)/(1-SPRtarget)"),
),
),

column( width = 2, offset = 0, style = 'padding-left:50px; padding-right:0px; padding-top:0px; padding-bottom:0px',

pickerInput(
  inputId = "Id0731",
  label = "x Axis",
  choices = c("B/BMSY proxy (Btarget)", "B/BMSY", "B/SPR target", "B/B0")
),
),

column( width = 2, offset = 0, style = 'padding-left:20px; padding-right:0px; padding-top:0px; padding-bottom:0px',
textInput(inputId = "userInput12", label = "Position of Vline", value = "0.5, .62")
),

column( width = 2, offset = 0, style = 'padding-left:20px; padding-right:0px; padding-top:0px; padding-bottom:0px',
textInput(inputId = "userInput22", label = "Color of vline", value = " red, orange ")
),

column( width = 2, offset = 0, style = 'padding-left:20; padding-right:0px; padding-top:0px; padding-bottom:0px',
textInput(inputId = "userInput33", label = "Label of vline", value = "Flatfish, Scorpaenids and Other ")),


column( width = 1, offset = 0, style = 'padding-left:20px; padding-right:0px; padding-top:25px; padding-bottom:0px',

dropdown(

  awesomeCheckboxGroup(
    inputId = "graphOptions",
    label = "Graph Options",
    choices = list("Species labels", "Legend"),
    selected = list("Species labels")
  ),

  style = "default",
  size = "normal",
  icon = NULL,
  label = "Label Options",
  tooltip = FALSE,
  right = FALSE,
  up = FALSE,
  width = NULL,
  animate = animateOptions(
    enter = "fadeInDown",
    exit = "fadeOutUp",
    duration = 0.25
  ),
  inputId = NULL,

  radioGroupButtons(
    inputId = "pointSize2",
    label = "Point Size",
    choices = c("S","M", "L"),
    selected = "M"
  ),

),

),

column( width = 1, offset = 0, style = 'padding-left:11px; padding-right:0px; padding-top:35px; padding-bottom:0px',
prettyCheckbox(
  inputId = "squarePlot2",
  label = "Square Plot",
  value = FALSE,
  shape = c("curve"),
  animation = "smooth",
  bigger = TRUE,
),

) ) ) ) )
