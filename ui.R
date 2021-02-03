library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(plotly)

quad.plot.WC <-
data.frame(read.csv(paste0(getwd(), "/Fisheries_Updated_File.csv")))

fluidPage(
  theme = shinytheme("sandstone"),

  titlePanel("Fisheries Stock Status Tool"),
  
  navbarPage(
    "Stock Staus Quadplot-- measuring overfished and overfishing conditions across stocks and time"
  ),

  h5(
    "This tool plots relative stock biomass vs fishing intensity across stock assessments. "
  ),

  h5(
    "The default data file shows the results for Pacific Fishery Management Council approved stock assessments for west coast groundfishes."
  ),

  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Plot",
      plotOutput(outputId = "distPlot") %>% withSpinner(color = "#0dc5c1"),

      fluidPage(
        style = 'padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',

        column(
          width = 1,
          offset = 0,
          style = 'padding-left:35px; padding-right:0px; padding-top:25px; padding-bottom:0px',

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

            column(
              width = 1,
              offset = 0,
              style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

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

            column(
              width = 1,
              offset = 0,
              style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

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
                choices = list(
                  "Aurora",
                  "Black",
                  "Black_CA",
                  "Black_OR",
                  "Black_WA",
                  "Blackgill",
                  "Blu_Dec_CA",
                  "Blue_CA",
                  "Bocaccio",
                  "Brown",
                  "CA_scorp",
                  "Canary",
                  "Chilipepper",
                  "China_C" ,
                  "China_N" ,
                  "China_S",
                  "Copper",
                  "Cowcod",
                  "Darkblotched",
                  "Gopher",
                  "Gopher_BLKYL",
                  "Greenstriped",
                  "LST",
                  "POP",
                  "Rougheye",
                  "Scorpionfish",
                  "Sharpchin",
                  "Shortbelly",
                  "Splitnose",
                  "SST",
                  "Widow",
                  "Yelloweye",
                  "Yellowtail"
                ),
                selected = NULL
              ),

            ),

            column(
              width = 1,
              offset = 0,
              style = 'padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',

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
                choices = list(
                  "Big_skate",
                  "Cabezon_CA",
                  "Cabezon_NCA",
                  "Cabezon_OR",
                  "Cabezon_SCA",
                  "Dogfish",
                  "Kelp_greenling",
                  "Lingcod",
                  "Lingcod_N",
                  "Lingcod_S",
                  "Longnose",
                  "P. Hake",
                  "Sablefish",
                  "SpinyD"
                ),
                selected = NULL
              )
            )
          )
        ),


        column(
          width = 1,
          offset = 0,
          style = 'padding-left:25px; padding-right:90px; padding-top:25px; padding-bottom:0px',

          dropdown(
            radioGroupButtons(
              inputId = "pointDelete",
              label = "Stock Assessment Years",
              choices = c("Latest",
              "First & Last", "Custom", "All"),
              direction = "horizontal",
              selected = "Custom"
            ),

            prettyCheckbox(
              inputId = "YearSpecies1",
              label = "Year Species Linkage",
              value = FALSE,
              shape = c("curve"),
              animation = "smooth",
              bigger = TRUE,

            ),

            conditionalPanel(
              condition = "input.pointDelete == 'Custom'",

              awesomeCheckboxGroup(
                inputId = "checkGroup2",
                label = "Year",
                choices = unique(quad.plot.WC$Assessment_Year),
                selected = NULL
              ),

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

        column(
          width = 1,
          offset = 0,
          style = 'padding-left:0px; padding-right:250px; padding-top:25px; padding-bottom:0px',

          dropdown(
            pickerInput(
              inputId = "Id0731",
              #label = "Relative biomass options",
              choices = c("B/BMSY proxy (Btarget)", "B/BMSY", "B/SPR target", "B/B0")
            ),

            (
              textInput(
                inputId = "userInputX1",
                label = "Min and max values",
                value = "0, 4"
              )
            ),

            #dropdown(

            h4("\nBiomass Limit Reference Point (LRP)"),

            h5(tags$i("\nUp to 5 LRPs can be specified")),

            #column( width = 2, offset = 0, style = 'padding-left:120px; padding-right:0px; padding-top:0px; padding-bottom:0px',
            (
              textInput(
                inputId = "userInput",
                label = "LRP position relative to the target RP",
                value = "0.5, .62, , , "
              )
            ),

            #column( width = 2, offset = 0, style = 'padding-left:25px; padding-right:0px; padding-top:0px; padding-bottom:0px',
            (
              textInput(
                inputId = "userInput2",
                label = "Color",
                value = " red, orange, blue, blue, blue "
              )
            ),

            #column( width = 3, offset = 0, style = 'padding-left:35px; padding-right:0px; padding-top:0px; padding-bottom:0px',
            (
              textInput(
                inputId = "userInput3",
                label = "Label",
                value = "Flatfish, Scorpaenids and Other, , ,  "
              )

            ),

            style = "default",
            size = "normal",
            icon = NULL,
            label = "Relative biomass options",
            tooltip = FALSE,
            right = FALSE,
            up = FALSE,
            width = NULL,
            animate = animateOptions(
              enter = "fadeInDown",
              exit = "fadeOutUp",
              duration = 0.25
            ),
            #inputId = NULL
          )
        ),

        column(
          width = 1,
          offset = 0,
          style = 'padding-left:0px; padding-right:250px; padding-top:25px; padding-bottom:0px',

          dropdown(
            pickerInput(
              inputId = "Id073",
              #label = "Fishing intensity options",
              choices = c(
                "F/Fmsy",
                "TM_ABC",
                "TM/OFL",
                "F/FMSY proxy",
                "(1-SPR)/(1-SPRtarget)"
              ),
            ),

            (
              textInput(
                inputId = "userInputY1",
                label = "Min and max values",
                value = "0, 1.5"
              )
            ),

            style = "default",
            size = "normal",
            icon = NULL,
            label = "Fishing intensity options",
            tooltip = FALSE,
            right = FALSE,
            up = FALSE,
            width = NULL,
            animate = animateOptions(
              enter = "fadeInDown",
              exit = "fadeOutUp",
              duration = 0.25
            ),

          )

        ),

        column(
          width = 1,
          offset = 0,
          style = 'padding-left:0px; padding-right:165px; padding-top:25px; padding-bottom:0px',

          dropdown(
            awesomeCheckboxGroup(
              inputId = "ImageOptions",
              label = "Plot Options",
              choices = list("Images", "Species labels", "Year Labels"),
              selected = list("Species labels")
            ),

            prettyCheckbox(
              inputId = "squarePlot",
              label = "Square Plot",
              value = FALSE,
              shape = c("curve"),
              animation = "smooth",
              bigger = TRUE,

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
              choices = c("S", "M", "L"),
              selected = "M"
            ),

            style = "default",
            size = "normal",
            icon = NULL,
            label = "Plot Options",
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

        #column( width = 3, offset = 0, style = 'padding-left:0px; padding-right:0px; padding-top:25px; padding-bottom:0px',


        #),

        column(
          width = 1,
          offset = 0,
          style = 'padding-left:0px; padding-right:0px; padding-top:25px; padding-bottom:0px',

          downloadButton('downloadData', 'Download Plot')

        ),
      ),

      column(
        width = 2,
        offset = 0,
        style = 'padding-left:35px; padding-right:0px; padding-top:25px; padding-bottom:0px',
        h6("Choose custom species file"),
        fileInput(
          inputId = "fileInput1",
          label =  NULL,
          multiple = FALSE,
          accept = NULL,
          width = NULL,
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        )
      ),

      column(
        width = 2,
        offset = 0,
        style = 'padding-left:60px; padding-right:0px; padding-top:55px; padding-bottom:0px',

        conditionalPanel(condition = "input.customFile1 == false",
        h5("Use Default File"),),

        conditionalPanel(condition = "input.customFile1 == true",
        h5("Use Custom File"),),

        uiOutput('ui.action2'),
      ),

      column(
        width = 2,
        offset = 0,
        style = 'padding-left:60px; padding-right:0px; padding-top:55px; padding-bottom:0px',

        downloadButton("downloadData3", "Download Example File")
      )

    ),


    tabPanel(
      "Animation",
      div(plotlyOutput(outputId = "distPlot2", height = "100%"), align = "center") %>% withSpinner(color =
        "#0dc5c1"),

        fluidPage(
          style = 'padding-left:35px; padding-right:0px; padding-top:0px; padding-bottom:0px',

          column(
            width = 1,
            offset = 0,
            style = 'padding-left:27px; padding-right:0px; padding-top:25px; padding-bottom:0px',

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

              column(
                width = 1,
                offset = 0,
                style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

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

              column(
                width = 1,
                offset = 0,
                style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

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
                  choices = list(
                    "Aurora",
                    "Black",
                    "Black_CA",
                    "Black_OR",
                    "Black_WA",
                    "Blackgill",
                    "Blu_Dec_CA",
                    "Blue_CA",
                    "Bocaccio",
                    "Brown",
                    "CA_scorp",
                    "Canary",
                    "Chilipepper",
                    "China_C" ,
                    "China_N" ,
                    "China_S",
                    "Copper",
                    "Cowcod",
                    "Darkblotched",
                    "Gopher",
                    "Gopher_BLKYL",
                    "Greenstriped",
                    "LST",
                    "POP",
                    "Rougheye",
                    "Scorpionfish",
                    "Sharpchin",
                    "Shortbelly",
                    "Splitnose",
                    "SST",
                    "Widow",
                    "Yelloweye",
                    "Yellowtail"
                  ),
                  selected = NULL
                ),

              ),

              column(
                width = 1,
                offset = 0,
                style = 'padding-left:0px; padding-right:150px; padding-top:0px; padding-bottom:0px',

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
                  choices = list(
                    "Big_skate",
                    "Cabezon_CA",
                    "Cabezon_NCA",
                    "Cabezon_OR",
                    "Cabezon_SCA",
                    "Dogfish",
                    "Kelp_greenling",
                    "Lingcod",
                    "Lingcod_N",
                    "Lingcod_S",
                    "Longnose",
                    "P. Hake",
                    "Sablefish",
                    "SpinyD"
                  ),
                  selected = NULL
                )
              )
            )
          ),

          column(
            width = 1,
            offset = 0,
            style = 'padding-left:0px; padding-right:180px; padding-top:25px; padding-bottom:0px',

            dropdown(
              pickerInput(
                inputId = "Id074",
                #label = "Fishing intensity options",
                choices = c(
                  "F/Fmsy",
                  "TM_ABC",
                  "TM/OFL",
                  "F/FMSY proxy",
                  "(1-SPR)/(1-SPRtarget)"
                ),
              ),

              (
                textInput(
                  inputId = "userInputY12",
                  label = "Min and max values",
                  value = "0, 1.5"
                )
              ),

              style = "default",
              size = "normal",
              icon = NULL,
              label = "Fishing intensity options",
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

          column(
            width = 2,
            offset = 0,
            style = 'padding-left:50px; padding-right:0px; padding-top:25px; padding-bottom:0px',

            dropdown(
              pickerInput(
                inputId = "Id0731",
                #label = "Relative biomass options",
                choices = c("B/BMSY proxy (Btarget)", "B/BMSY", "B/SPR target", "B/B0")
              ),

              (
                textInput(
                  inputId = "userInputX12",
                  label = "Min and max values",
                  value = "0, 4"
                )
              ),

              h4("\nBiomass Limit Reference Point (LRP)"),

              h5(tags$i("\nUp to 5 LRPs can be specified")),

              (
                textInput(
                  inputId = "userInput12",
                  label = "LRP position relative to the target RP",
                  value = "0.5, .62, , , "
                )
              ),

              #column( width = 2, offset = 0, style = 'padding-left:25px; padding-right:0px; padding-top:0px; padding-bottom:0px',
              (
                textInput(
                  inputId = "userInput22",
                  label = "Color",
                  value = " red, orange, blue, blue, blue "
                )
              ),

              #column( width = 3, offset = 0, style = 'padding-left:35px; padding-right:0px; padding-top:0px; padding-bottom:0px',
              (
                textInput(
                  inputId = "userInput33",
                  label = "Label",
                  value = "Flatfish, Scorpaenids and Other, , ,  "
                )

              ),

              style = "default",
              size = "normal",
              icon = NULL,
              label = "Relative biomass options",
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

          column(
            width = 1,
            offset = 0,
            style = 'padding-left:0px; padding-right:190px; padding-top:25px; padding-bottom:0px',

            dropdown(
              awesomeCheckboxGroup(
                inputId = "graphOptions",
                label = "Figure Options",
                choices = list("Species labels", "Legend"),
                selected = list("Species labels")
              ),

              prettyCheckbox(
                inputId = "squarePlot2",
                label = "Square Plot",
                value = FALSE,
                shape = c("curve"),
                animation = "smooth",
                bigger = TRUE,
              ),

              style = "default",
              size = "normal",
              icon = NULL,
              label = "Plot Options",
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
                choices = c("S", "M", "L"),
                selected = "M"
              ),

            ),

            #column( width = 1, offset = 0, style = 'padding-left:0px; padding-right:0px; padding-top:35px; padding-bottom:0px',

            #),
          ),

          column(
            width = 1,
            offset = 0,
            style = 'padding-left:0px; padding-right:0px; padding-top:25px; padding-bottom:0px',

            downloadButton('downloadData2', 'Download Plot')

          ),

        ),

        column(
          width = 2,
          offset = 0,
          style = 'padding-left:60px; padding-right:0px; padding-top:25px; padding-bottom:0px',
          h6("Choose custom species file"),
          fileInput(
            inputId = "fileInput2",
            label =  NULL,
            multiple = FALSE,
            accept = NULL,
            width = NULL,
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          )
        ),

        column(
          width = 2,
          offset = 0,
          style = 'padding-left:60px; padding-right:0px; padding-top:55px; padding-bottom:0px',

          conditionalPanel(condition = "input.customFile2 == false",
          h5("Use Default File"),),

          conditionalPanel(condition = "input.customFile2 == true",
          h5("Use Custom File"),),

          uiOutput('ui.action'),
        ),

        column(
          width = 2,
          offset = 0,
          style = 'padding-left:60px; padding-right:0px; padding-top:55px; padding-bottom:0px',

          downloadButton("downloadData4", "Download Example FIle")
        )

      )
    )
  )
