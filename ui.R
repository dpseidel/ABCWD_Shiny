require(shiny)
require(shinydashboard)
require(shinyjs)
library(leaflet)
library(purrr)


variable_list <- list(
  response = "cwd",
  global = c("sp", "sex", "harv", "time", "e_min"),
  riparian = c("Driver", "Dstream", "Strm3km", "Strm12km"),
  ruggedness = c("Rugg3km", "Rugg12km"),
  landcover = c("Pcover3", "Pagri3", "Pcover12", "Pagri12"),
  soil = c("Clay3km", "Clay12km"),
  humdev = c("Dwell12km", "Hard12km")
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Estimating Alberta's CWD Risk",
    titleWidth = 300
  ),
  dashboardSidebar(
    useShinyjs(),
    width = 300,
    # sidebar setup
    div(
      id = "tab1_sidebar",
      menuItem("Information", tabName = "info", startExpanded = TRUE, icon = icon("dashboard"))
    ),
    div(
      id = "tab2_sidebar",
      menuItem("Load",
        tabName = "load", startExpanded = TRUE, icon = icon("th"),
        fileInput("datafile", "Choose CSV file",
          accept = c("text/csv", "text/comma-separated-values")
        ),
        uiOutput("cwdcol"),
        uiOutput("sexcol"),
        uiOutput("spcol"),
        uiOutput("harvcol"),
        uiOutput("proxcol"),
        uiOutput("datecol"),
        uiOutput("latcol"),
        uiOutput("longcol"),
        br(),
        uiOutput("extbutton"),
        br(),
        uiOutput("button")
      )
    ),
    div(
      id = "tab5_sidebar",
      menuItem("Raster Extraction",
        tabName = "rasters", startExpanded = TRUE, icon = icon("th"),
        checkboxGroupInput("extract",
          "Extract Values to Points",
          choices = reduce(variable_list, c)[-1 * (1:6)]
        ),
        actionButton("extractButton", "Extract Variables!"),
        br(),
        uiOutput("download")
      )
    ),
    div(
      id = "tab3_sidebar",
      menuItem("Choose Variables",
        startExpanded = TRUE, tabName = "vars", icon = icon("th"),
        checkboxGroupInput("Glob_Input", "Global Variables",
          choices = c(
            Species = "",
            Sex = "",
            `Harvest Method` = "",
            `Years Since 1st Positive` = "",
            `+Proximity` = ""
          )
        ),
        selectInput("Ter_Input", "Riparian Variable",
          choices = c(
            `Select One or More` = "",
            `Distance to Major River` = "Driver",
            `Distance to Stream` = "Dstream",
            `Stream Density - 3km2` = "Strm3km",
            `Stream Density - 12km2` = "Strm12km"
          ), multiple = TRUE,
          selected = c("Driver", "Dstream")
        ),
        selectInput("Rugg_Input", "Terrain Variable",
          choices = c(
            `Select One` = "",
            `Ruggedness - 3km2` = "Rugg3km",
            `Ruggedness - 12km2` = "Rugg12km"
          ),
          multiple = FALSE
        ),
        selectInput("Hum_Input", "Human Disturbance Variable",
          choices = c(
            `Select One` = "",
            `Dwell 12km` = "Dwell12km",
            `Hard 12km` = "Hard12km"
          ),
          multiple = FALSE
        ),
        selectInput("LCV_Input", "Landcover Variable",
          choices = c(
            `Select One or More` = "",
            `Agriculture -- 3km2` = "Pagri3",
            `Agriculture -- 12km2` = "Pagri12",
            `Cover -- 3km2` = "Pcover3",
            `Cover -- 12km2` = "Pcover12"
          ), selected = "Pagri12", multiple = TRUE
        ),
        selectInput("Soil_Input", "Soil Texture Variable", # make this reactive based on a scale selector?
          choices = c(
            `Select One` = "",
            `% Clay -- 3km2` = "Clay3km",
            `% Clay -- 12km2` = "Clay12km"
          )
        ),
        tags$div(
          title = "Type additional variable names here, using a `+` between variables as if writing an equation.",
          textInput("other", "Other Variable? Interactions?", "Driver*Pagri12")
        ),
        actionButton("goButton", "Fit Model!", width = "50%")
      )
    ),
    div(
      id = "tab4_sidebar",
      menuItem("Choose Map Constants",
        tabName = "cons", startExpanded = TRUE, icon = icon("th"),
        numericInput("maptime",
          label = "Year",
          value = 2015
        ),
        selectizeInput("mapsp", "Species", c(
          "Mule Deer" = 1,
          "White-Tailed Deer" = 0
        )),
        selectizeInput("mapsex", "Sex",
          choices = c("Male" = 1, "Female" = 0),
          selected = 1
        ),
        # fileInput("newproxfile", "Upload new +Proximity raster? Please ensure EPSG:26912",
        #           accept = c("image/tiff")
        # ),
        actionButton("buildButton", "Build Predictive Layer"),
        br(),
        br(),
        uiOutput("downMap"),
        br(),
        uiOutput("downRep")
      )
    )
  ),
  dashboardBody(
    tabsetPanel(
      # main body set up
      id = "navbar",
      tabPanel(
        title = "Information", id = "tab1", value = "tab1_val",
        valueBoxOutput("tab1_valuebox", width = 12),
        valueBoxOutput("info", width = 12),
        valueBoxOutput("info_box", width = 12)
      ),
      tabPanel(
        title = "Load and Review Points", id = "tab2", value = "tab2_val",
        valueBoxOutput("tab2_valuebox", width = 12),
        DT::dataTableOutput("filetable")
      ),
      tabPanel(
        title = "Extract Predictor Variables",
        id = "tab5", value = "tab5_val",
        valueBoxOutput("tab5_valuebox", width = 12),
        DT::dataTableOutput("extracttable")
      ),
      tabPanel(
        title = "Run Regression", id = "tab3", value = "tab3_val",
        valueBoxOutput("tab3_valuebox", width = 12),
        strong(uiOutput("setup")),
        uiOutput("call"), br(),
        strong(uiOutput("setup2")),
        DT::dataTableOutput("summary"),
        textOutput("aic"),
        br(),
        uiOutput("toMap")
      ),
      tabPanel(
        title = "Risk Map & Report", id = "tab4", value = "tab4_val",
        valueBoxOutput("tab4_valuebox", width = 12),
        mapview::mapviewOutput("map", height = 800)
      )
    )
  )
)
