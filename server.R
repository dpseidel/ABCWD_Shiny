library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(glue)
library(sf)
library(raster)
library(mapview)
library(broom)
library(leaflet)
library(DT)
library(velox)
library(knitr)
library(kableExtra)

# this allows loading files of up to 30MB. Plenty for large csvs but would need
# to be bumped significantly to allow rasters to be loaded.
options(shiny.maxRequestSize = 30 * 1024^2)

server <- shinyServer(function(input, output, session) {
  values <- reactiveValues(selectedTab = 1)
  # syncs sidebar and main tabs
  observeEvent(input$navbar, {
    toggle("tab1_sidebar", condition = input$navbar == "tab1_val")
    toggle("tab2_sidebar", condition = input$navbar == "tab2_val")
    toggle("tab3_sidebar", condition = input$navbar == "tab3_val")
    toggle("tab4_sidebar", condition = input$navbar == "tab4_val")
    toggle("tab5_sidebar", condition = input$navbar == "tab5_val")
    toggle("tab6_sidebar", condition = input$navbar == "tab6_val")
  })

  # starting text on each of the main tabs
  output$tab1_valuebox <- renderValueBox({
    box(
      width = 12, title = "Welcome to the Alberta CWD risk assessment app",
      solidHeader = TRUE, status = "info", align = "center",
      "This tool was built for managers to make it easier to update and fit new
      risk models for CWD. It allows users to upload cleaned surveillance data,
      extract relevant environmental predictors, fit models of CWD risk and map the results.", br(),
      "To begin, first use the `Load and Review Data` tab to load data. Then,
      use the `Extract Predictor Variables` tab to choose variables to extract
      or, if your data already contains the environmental variables of interest,
      continue straight on to build and fit
      your model of interest using the `Run Regression` tab. Finally, build a predictions layer, map your model's
      predictions and download your report using the `Risk Map & Report` tab."
    )
  })

  variable_html <- read_csv("App-variables.csv") %>%
    dplyr::select(-3) %>%
    kable() %>%
    kable_styling(full_width = F) %>%
    column_spec(2, width = "32em")

  output$info <- renderValueBox({
    box(
      width = 12, solidHeader = TRUE, status = "info", align = "left",
      "This app was originally intended as a tool for reproducing and updating
      the CWD risk model published in the 2014 ESRD CWD report. As such, it can preform
      extractions and build predictive rasters using the following variables. Those
      variables listed as `user-specified` must be provided by the user and reflect
      the same format/units as listed, others may be extracted from within the app
      using the `Extract Predictor Variables` tab.",
      tags$div(HTML(paste(variable_html))),
      tags$span(
        style = "text-align:left",
        h5("Below are references for the data layers used and the 2014 ESRD CWD report that inspired this app:"),
        h6(
          "Nobert, B., Pybus MJ., and Merril E. 2014. CWD Surveillance in Alberta 2005-2012: Literature Review and Initial Data Analysis.
                   Fish and Wildlife Policy Branch, Alberta Environment and Sustainable Resource Development, Edmonton, Alberta. 109pp.", br(),
          "AARD [Alberta Agriculture and Rural Development]. (2011) Agricultural Region of Alberta Soil Inventory Database (AGRASID 3.0). Available:",
          tags$a("http://www1.agric.gov.ab.ca/$department/deptdocs.nsf/all/sag3252?opendocument"),
          br(),
          "ABMI [Alberta Biodiversity Monitoring Institute]. (2000) ABMI Wall-to-wall Land Cover Map Version 2.1 (ABMIw2wLCV2000v2.1). Available:",
          a("http://www.abmi.ca"), ".", br(),
          "ABMI [Alberta Biodiversity Monitoring Institute]. (2010) ABMI Human Footprint Map Version 1.1. Available:",
          a("http://www.abmi.ca"), ".", br(),
          "GeoBase. (2011) Canadian Digital Elevation Data (1:250,000 scale). Available:",
          a("https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333"), ".", br(),
          "GOA [Government of Alberta]. (2013) “HydroRvr” polyline shapefile in IMOD database. Original data source unknown."
        )
      )
    )
  })

  output$variables_table <- renderDT(datatable(variables), options = list(lengthChange = FALSE))
  output$info_box <- renderValueBox({
    box(
      width = 12, solidHeader = TRUE, status = "info", align = "center",
      "Created by", a("Dana Paige Seidel", href = "https://www.danaseidel.com"),
      "for use by the Government of Alberta, Department of Fish and Wildlife.",
      br(), "To report issues with the app or to request new features or data, please open an issue at this",
      a("link", href = "https://www.github.com/dpseidel/ABCWD_Shiny/issues/"),
      "."
    )
  })

  output$tab2_valuebox <- renderValueBox({
    box(
      status = "info", "1. Click the `Browse` button on the left to
      import data in the form of a CSV file.", br(),
      "2. Match columns of data to the appropriate variable names using the dropdown menus.", br(),
      "3. Click `Build Regression!` to start regression.",
      br(), br(), "If your data does not already contain your environmental variables of interest
      (with appropriate column names), click `Extract new variables?` button to go to the extraction
      page.",
      br(), br(),
      "Reminder! `cwd`, `sex`, `species`, and `harvest` columns must be in binary (0/1) format when loaded.
      The app expects that 1 refers to CWD+, male, mule deer, and hunted respectively.
      Furthermore, if you are importing data with already extracted values corresponding
      to the known environmental covariates in this app, be sure column names
      match those listed in the table on the `Information` tab exactly.",
      solidHeader = TRUE, align = "left"
    )
  })

  output$tab5_valuebox <- renderValueBox({
    box(
      status = "info", "1. Select the variables to extract from the sidebar", br(),
      "2. Click `Extract Variables!` and wait for data to appear below", br(),
      "3. When satisfied with the extracted variables, click `Download Extracted Data` to download a .csv of the new data file and/or
      click the `Build Regression` tab on the top of this page to fit a risk model using the newly extracted variables.",
      solidHeader = TRUE, align = "left"
    )
  })

  output$tab3_valuebox <- renderValueBox({
    box(
      status = "info", "1. Select which variables to model.", br(),
      "2. Click Fit Model! to generate Regression Function and Statistics.", br(),
      "3. Once regression is complete, click `See Map!` below the results to continue to the mapping tab.",
      br(), br(),
      "Running the model with the default values will fit the best model according to the 2014 ESRD CWD report by Nobert et al.
      You may fit models with custom variables, as long as they are provided in your uploaded data file, by writing custom model
      terms into the `Other Variable? Interactions?` box; however you will be
      unable to build a predictions layer for any variables not included in the app,
      listed on the `Information` tab. Variables
      that are likely to have high levels of covariance are restricted from being included in
      the same models but you may choose to force this inclusion by writing the variable names
      into this `Other Variable? Interactions?` box. Keep in mind this box expects
      typical model notation if you are adding an interaction or more than one
      custom varible; for example, if you wanted to add variables named `wells` and `roads`,
      you should enter \"wells + roads\" in the text box.", br(), br(),
      "If you encounter an error on this page, please click back to the file upload tab and check that
      all your columns are matched correctly and ensure that your environmental variable
      columns have the correct naming conventions. Your progress will be saved even if you switch
      between tabs."
    )
  })

  output$tab4_valuebox <- renderValueBox({
    box(
      status = "info", "1. Choose map constants in sidebar to begin mapping process.", br(),
      "2. Click `Build Predictive Layer` to build a map of risk according to your last fit model and specified constants.", br(),
      "Please keep in mind this step can take about a minute! The map will reload
      and options to download the raster and/or a report
      will appear in the sidebar when raster building is complete.",
      br(),
      "3. Examine your map! It's interactive, you can zoom in and out and hover to view values in space.
      To overlay WMU boundarys or CWD+ cases in your loaded dataset, toggle the checkboxes in the map legend.",
      br(),
      "4. To download the raster or a report containing regression results and a plot,
        click the respectively labeled buttons on the sidebar.",
      br(), br(),
      "Be aware, all prediction maps created in this app are specific to the
       rasters used to build them. Specifially it's important to realize that,
       the proximity layer currently is built using
       CWD+ cases only up until January 2016. This means predictions for years
       much before or much after this point in time will have progressively more error
       or uncertainty. Please contact the maintainer if you feel that the rasters need to be updated
       or have suggestions for others you would like included."
      # "Optionally, you may choose to upload a new +Proximity raster. The raster internal to
      #   this application
      #   includes CWD+ cases only up until January 2016. For the most accurate predictions map, upload a
      #   proximity raster built off all known cases. This is easily done in ArcGIS using the Euclidean Distance tool.
      #   If you choose not to upload a new raster, your predictions will be built using the internal +Proximity raster."
    )
  })

  # This function is repsonsible for loading in the selected file
  # this is the functionality for the second tab
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read_csv(infile$datapath)
  })

  # This previews the CSV data file
  output$filetable <- renderDataTable({
    DT::datatable(filedata(), options = list(scrollX = TRUE))
  })

  # identify year, sex, sp, cwd, and harvest columns
  output$cwdcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    items <- names(df)
    names(items) <- items
    selectInput("cwd", "CWD Column (0/1):", items)
  })

  output$sexcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    items <- names(df)
    names(items) <- items
    selectInput("sex", "Sex Column (0/1):", items)
  })
  output$harvcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    items <- names(df)
    names(items) <- items
    selectInput("harv", "Harvest Method Column (0/1):", items)
  })
  output$spcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    items <- names(df)
    names(items) <- items
    selectInput("sp", "Species Column (0/1):", items)
  })
  output$proxcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    items <- names(df)
    names(items) <- items
    selectInput("prox", "Proximity+ Column", items)
  })
  output$datecol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    items <- names(df)
    names(items) <- items
    selectInput("date", "Time Column", items)
  })
  output$latcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    items <- names(df)
    names(items) <- items
    selectInput("lat", "Easting (UTM 12N NAD83)", items)
  })
  output$longcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    items <- names(df)
    names(items) <- items
    selectInput("long", "Northing (UTM 12N NAD83)", items)
  })

  observe({
    x <- c(input$sp, input$sex, input$harv, input$date, input$prox)

    updateCheckboxGroupInput(session, "Glob_Input",
      label = "Global Variables",
      choices = c(
        Species = x[1],
        Sex = x[2],
        `Harvest Method` = x[3],
        `Years Since 1st Positive` = x[4],
        `+Proximity` = x[5]
      ),
      selected = x
    )
  })

  # Loading sidebar functionality ie Tab 2
  output$button <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    actionButton("modelbutton", "Build Regression!")
  })

  output$extbutton <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)

    actionButton("ext_tabbutton", "Extract new variables?")
  })

  shinyjs::onclick("ext_tabbutton", expr = {
    # move to extract Results
    updateTabsetPanel(session, "navbar", "tab5_val")
  })

  # on click of "Build Regression!"
  shinyjs::onclick("modelbutton", expr = {
    # move to Regression Results
    updateTabsetPanel(session, "navbar", "tab3_val")
  })

  ### Extraction Tab -- Tab 3

  ## Handle my rasters
  raster_files <- list.files("data/rasters/", full.names = T)
  names(raster_files) <- c(
    "Pagri12", "Pagri3", "Clay12km", "Clay3km", "Pcover12", "Pcover3",
    "Dwell12km", "Hard12km", "Dstream", "Driver", "+Prox",
    # this is just a place holder -- +Prox needs to be replaced reactively with input$prox
    "Rugg12km", "Rugg3km", "Strm12km", "Strm3km"
  )

  # NOTE: input labels for lat and long are backwards and actually refer to UTM east and north
  extraction <- eventReactive(input$extractButton, {
    vars <- unlist(input$extract)

    sf <- filedata() %>%
      st_as_sf(coords = c(input$lat, input$long), crs = 2152, remove = FALSE, na.fail = F)

    vxs <- map(raster_files[vars], velox)
    extractions <- map_dfc(vxs, ~ as.numeric(.x$extract_points(sf)))

    # dplyr::bind_cols(dplyr::select(filedata(), key), extractions)
    extractions
  })

  output$extracttable <- renderDataTable({
    if (length(input$extractButton) == 0) return(NULL)

    DT::datatable(
      bind_cols(filedata(), extraction()),
      options = list(scrollX = TRUE)
    )
  })

  output$download <- renderUI({
    # if (input$new == "no") return(NULL)
    # if (input$extractButton == 0 ) return(NULL)
    downloadButton("extracteddata", "Download extracted data")
  })


  output$extracteddata <- downloadHandler(
    filename = "cwdshiny_extracteddata.csv",
    content = function(file) {
      write_csv(bind_cols(filedata(), extraction()), file)
    }
  )


  # Regression Functionality ie Tab 4
  regressioncall <- eventReactive(input$goButton, {
    vars <- c(
      input$Glob_Input, input$Hum_Input, input$Ter_Input, input$Rugg_Input,
      input$LCV_Input, input$other, input$Soil_Input
    )

    as.formula(paste(input$cwd, "~", glue_collapse(vars[str_length(vars) != 0], " + ")))
  })

  logit <- reactive({
    if (input$extractButton > 0) {
      data <- bind_cols(filedata(), extraction())
    } else {
      data <- filedata()
    }
    glm(regressioncall(), data = data, family = binomial(link = "logit"))
  })

  output$setup <- renderText({
    "Model Call: "
  })
  output$setup2 <- renderText({
    "Regression Summary:"
  })

  output$call <- renderPrint({
    regressioncall()
  })
  output$summary <- renderDataTable({
    DT::datatable(broom::tidy(logit()),
      options = list(
        searching = FALSE,
        pageLength = 25
      )
    )
  })
  output$aic <- renderText({
    paste("The AIC of this model is", AIC(logit()), ". Keep in mind that AIC values
    can only be compared across models fit to the same dataset and should not be taken
    as the only measure of model fit or suitability.")
  })

  output$toMap <- renderUI({
    if (is.null(logit())) return(NULL)
    actionButton("toMap", "See Map!")
  })

  shinyjs::onclick("toMap", expr = {
    # move to Map Results
    updateTabsetPanel(session, "navbar", "tab4_val")
  })


  # MAP tab ie tab 4
  # these constant names need to reflect the variable/columns given.
  constants <- reactive(data.frame(
    time = (input$maptime - 2000),
    sex = as.numeric(input$mapsex),
    hunt = 1,
    sp = as.numeric(input$mapsp)
  ))

  pars <- reactive(
    c(
      input$prox, input$Soil_Input, input$Hum_Input,
      input$LCV_Input, input$Rugg_Input, input$Ter_Input
    )
  )

  stk <- reactive({
    infile <- input$newproxfile
    # fixing the raster_file name
    names(raster_files)[11] <- input$prox
    stack <- stack(na.omit(raster_files[pars()[str_length(pars()) != 0]]))
    return(stack)
  })

  # # if user loads a new prox file use it, else use the internal one.
  # # this is almost completely functional but I don't recommend using it
  # # requires allowing LARGE uploads, and ability to resample rasters on the fly
  # # which will require a lot of memory. Easier to update rasters on the server
  # # side as a part of regular maintanance.
  # stk <- reactive({
  #   infile <- input$newproxfile
  #
  #   # fixing the raster_file name
  #   names(raster_files)[11] <- input$prox
  #   stack <- stack(na.omit(raster_files[pars()[str_length(pars()) != 0]]))
  #
  #   if (is.null(infile)) {
  #     # User has not uploaded a file yet
  #     return(stack)
  #   }
  #
  #   newprox <- raster(infile$datapath)
  #   # we need resampling to match...
  #   # fast resample with gdal warp
  #   # this may cause issues when deployed... not sure.
  #   tmp_fil <- tempfile(fileext = ".tif")
  #   t1 <- c(272584.6, 5427085, 572784.6, 6093785) # xmin, ymin, xmax, ymax coordinates
  #   t2 <- c(100, 100) # resolution
  #   gdalwarp(
  #     srcfile = newprox, dstfile = tmp_fil,  tr = t2, te = t1,
  #     output_Raster = T, overwrite = T, verbose = T
  #   )
  #
  #   rsmpl <- raster(tmp_fil)
  #   names(rsmpl) <- input$prox
  #
  #   # drop the original e_min raster
  #   temp_stack <- subset(stack, names(stack)[-which(names(stack) == input$prox)])
  #
  #   # clean_up and return a new stack
  #   file.remove(tmp_file)
  #   return(stack(rsmpl, temp_stack))
  # })


  predictions <- eventReactive(input$buildButton, {
    cnt <- isolate(constants())
    names(cnt) <- c(input$date, input$sex, input$harv, input$sp)

    exp(
      raster::predict(
        object = stk(),
        model = logit(),
        fun = predict,
        const = cnt
      )
    )
  })

  WMUs <- st_read("data/AB_WMUs.shp") %>% st_transform(4326)

  main <- reactive(
    paste(
      "CWDRisk", (input$maptime),
      ifelse(input$mapsex == "1", "M", "F"),
      ifelse(input$mapsp == "1", "MD", "WTD"),
      sep = "_"
    )
  )

  positives <- reactive({
    filedata() %>%
      filter(cwd == 1) %>%
      st_as_sf(coords = c(input$lat, input$long), crs = 2152, remove = FALSE, na.fail = F) %>%
      st_transform(4326)
  })

  output$map <- renderLeaflet({
    if (length(input$buildButton) == 0 || input$buildButton == 0) return(mapview()@map %>% setView(-113, 55, zoom = 5))

    pred <- predictions()
    vx <- velox(pred)
    vx$aggregate(c(12, 12), "max")

    pal <- colorNumeric(
      palette = colorRamp(c("#49AD3F", "#f1F904", "#D73027"), bias = 2),
      domain = NULL, na.color = "#00000000"
    )

    mv <- mapview(vx$as.RasterLayer(1), layer.name = "Risk", col.regions = pal(1:256)) +
      mapview(positives(), layer.name = "CWD+", cex = 3) +
      mapview(WMUs, alpha.regions = 0, alpha = .75)

    mv@map %>%
      addLayersControl(
        overlayGroups = c("WMUs", "Risk", "CWD+"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("CWD+"))
  })

  output$downloadMap <- downloadHandler(
    filename = function() {
      paste0(main(), ".tif") # uses title to make file name.
    },
    content = function(file) {
      raster::writeRaster(predictions(), file)
    }
  )

  output$downMap <- renderUI({
    df <- predictions()
    if (is.null(df)) return(NULL)

    downloadButton("downloadMap", "Download Prediction Raster")
  })

  output$downRep <- renderUI({
    df <- predictions()
    if (is.null(df)) return(NULL)

    downloadButton("report", "Generate report")
  })

  #### Report Building

  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list( # data = filedata(),  #  update this after extractions?
        call = regressioncall(),
        reg = logit(),
        title = main(),
        pred = predictions()
      ) # needs to match params list in yaml

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
})
