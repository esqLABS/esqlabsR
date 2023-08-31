#---------- Initialization of app ----------#
# Load required packages
library(esqlabsR)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rhandsontable)

#---------- User interface ----------#
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = TRUE, collapsed = TRUE),
  dashboardBody(
    shinyjs::useShinyjs(),
    box(
      title = "Project Configuration",
      width = 12,
      collapsible = TRUE,
      fluidRow(
        column(4, actionButton("changeProjConfig", "Change project configuration file")),
        column(6, span(textOutput("currentProjConfig"), style = "color:red")),
      ),
      br(),
      fluidRow(
        column(4, actionButton("save", "Save plotsFile")),
        column(6, span(textOutput("currentPlotsFile"), style = "color:red"))
      ),
      br(),
      fluidRow(
        column(4, actionButton("saveNew", "Save as new file"))
      ),
      br(),
      fluidRow(
        column(5, selectInput("sheetsToLoad", label = "Observed data sheets to load:", choices = NULL, multiple = TRUE)),
        column(3, div(style = "margin-top: 25px;", actionButton("loadDataSets", "Load observed data")))
      )
    ),
    tabBox(
      id = "tabset1",
      width = 12,
      tabPanel(
        "DataCombined",
        br(),
        fluidRow(
          column(2, actionButton("addData", "Add Data")),
          column(4, span(textOutput("infoAddData"), style = "color:red")),
          column(2, actionButton("addColumnDataCombined", "Add input field..."))
        ),
        br(),
        fluidRow(
          column(3, textInput("DataCombinedName", label = "DataCombinedName")),
          column(3, selectInput("dataType", label = "dataType", choices = c("observed", "simulated"))),
          column(3, textInput("label", label = "label"))
        ),
        fluidRow(
          column(3, selectizeInput("scenario", label = "scenario", choices = "", options = list(create = TRUE))),
          column(6, selectizeInput("path", label = "path", choices = "", options = list(create = TRUE)))
        ),
        fluidRow(
          column(9, selectizeInput("dataSet", label = "dataSet", choices = "", options = list(create = TRUE)))
        ),
        fluidRow(
          column(2, textInput("group", label = "group")),
          column(2, textInput("xOffsets", label = "xOffsets", placeholder = "e.g. (1, 2, 3)")),
          column(2, textInput("yOffsets", label = "yOffsets", placeholder = "e.g. (1, 2, 3)")),
          column(2, textInput("xScaleFactors", label = "xScaleFactors", placeholder = "e.g. (1, 2, 3)")),
          column(2, textInput("yScaleFactors", label = "yScaleFactors", placeholder = "e.g. (1, 2, 3)")),
        ),
        fluidRow(
          uiOutput("newInputsTab1")
        ),
        br(),
        tableOutput("dataCombinedSheet")
      ),
      tabPanel(
        "plotConfiguration",
        br(),
        fluidRow(
          column(2, actionButton("addPlot", "Add Plot")),
          column(3, span(textOutput("infoAddPlot"), style = "color:red")),
          column(2, actionButton("addColumnPlotConfiguration", "Add input field..."))
        ),
        br(),
        fluidRow(
          column(2, textInput("plotID", label = "plotID")),
          column(2, selectizeInput("PlotDataCombined", label = "DataCombinedName", choices = "")),
          column(2, selectInput("plotType",
                                label = "plotType",
                                choices = c("individual", "population", "observedVsSimulated", "residualsVsSimulated", "residualsVsTime")
          )),
          column(2, textInput("plotTitle", label = "title")),
          column(2, selectizeInput("xUnit", label = "xUnit", choices = c("", unname(purrr::list_flatten(ospUnits))))),
          column(2, selectizeInput("yUnit", label = "yUnit", choices = c("", unname(purrr::list_flatten(ospUnits)))))
        ),
        fluidRow(
          column(2, selectizeInput("xAxisScale", label = "xAxisScale", choices = c("", tlf::Scaling))),
          column(2, selectizeInput("yAxisScale", label = "yAxisScale", choices = c("", tlf::Scaling))),
          column(2, textInput("xAxisLimits", label = "xAxisLimits", placeholder = "e.g. (0, 10)")),
          column(2, textInput("yAxisLimits", label = "yAxisLimits", placeholder = "e.g. (0, 10)")),
          column(2, textInput("quantiles", label = "quantiles", placeholder = "e.g. (-2, 0, 2)")),
          column(2, textInput("foldDistance", label = "foldDistance")),
        ),
        fluidRow(
          uiOutput("newInputsTab2")
        ),
        br(),
        tableOutput("plotConfigurationSheet")
      ),
      tabPanel(
        "plotGrids",
        br(),
        fluidRow(
          column(2, actionButton("addPlotGrid", "Add Plot Grid")),
          column(3, span(textOutput("infoAddPlotGrid"), style = "color:red")),
          column(2, actionButton("addColumnPlotGrid", "Add input field..."))
        ),
        br(),
        fluidRow(
          column(3, textInput("plotGridName", label = "name")),
          column(3, selectizeInput("plotIDs", label = "plotIDs", choices = "", multiple = TRUE)),
          column(3, textInput("plotGridTitle", label = "title"))
        ),
        fluidRow(
          uiOutput("newInputsTab3")
        ),
        br(),
        tableOutput("plotGridSheet")
      )
    ),
    box(
      width = 12,
      # div(
      # style = "overflow-x:scroll;",
      rHandsontableOutput("currentSheet")
      # )
    )
  )
)

#---------- Backend logic ----------#
server <- function(input, output, session) {
  r <- reactiveValues()
  r$projConfigFileMessage <- "No project configuration selected"
  r$currentTab <- "DataCombined"

  ######## FILE IMPORT ###########

  # Choose a file on computer
  observeEvent(input$changeProjConfig, {
    tryCatch(
      {
        r$newConfigFile <- file.choose()
      },
      error = function(cond) {
        showNotification(ui = "No project configuration selected")
        return(NULL)
      }
    )
  })

  # Load excel files based on selected project configuration
  observeEvent(r$newConfigFile, {
    tryCatch(
      {
        loadProjectConfiguration(r)
      },
      error = function(cond) {
        showNotification(ui = "Loading project configuration caused an error, no new configuration loaded.", duration = NULL)
        showNotification(ui = cond$message, duration = NULL)
      },
      warning = function(cond) {
        showNotification(ui = "Loading project configuration caused a warning, no new configuration loaded.", duration = NULL)
        showNotification(ui = cond$message, duration = NULL)
      }
    )
  })


  #' loadProjectConfiguration
  #' @description Read project configuration files and import plot, scenario and data files.
  #'
  #' @param r reactiveValues object
  loadProjectConfiguration <- function(r) {
    r$newProjectConfiguration <- createDefaultProjectConfiguration(r$newConfigFile)

    r$projConfigFileMessage <- paste(" Current project configuration file:", r$newConfigFile)

    r$plotFile <- r$newProjectConfiguration$plotsFile
    r$scenarioFile <- r$newProjectConfiguration$scenarioDefinitionFile

    # Load plotFile
    if (file.exists(r$plotFile)) {
      r$plotsFileMessage <- paste(" Current plotsFile:", r$plotFile)
      r$dfDataCombined <- readExcel(r$plotFile, sheet = "DataCombined", col_types = "text")
      r$dfPlots <- readExcel(r$plotFile, sheet = "plotConfiguration", col_types = "text")
      r$dfPlotGrids <- readExcel(r$plotFile, sheet = "plotGrids", col_types = "text")
    } else {
      r$plotsFileMessage <- paste(" No plotsFile defined in current project configuration")
      r$plotFile <- NULL
      r$dfDataCombined <- NULL
      r$dfPlots <- NULL
      r$dfPlotGrids <- NULL
    }

    # Load ScenarioFile
    if (file.exists(r$scenarioFile)) {
      r$dfScenarios <- readExcel(r$scenarioFile, sheet = "Scenarios")
      r$dfOutputPaths <- readExcel(r$scenarioFile, sheet = "OutputPaths")
    }

    # Load dataFile
    dataFile <- r$newProjectConfiguration$dataFile
    dataImporterConfigurationFile <- r$newProjectConfiguration$dataImporterConfigurationFile
    if (file.exists(dataFile) & file.exists(dataImporterConfigurationFile)) {
      # get names of sheets and update input for choosing which to load
      sheets <- readxl::excel_sheets(dataFile)
      updateSelectInput(session, inputId = "sheetsToLoad", choices = sheets)
    }
  }


  ######## Add new rows in tables ###########

  observeEvent(input$addData, {
    req(input$DataCombinedName)
    req(input$label)

    if (input$dataType == "simulated") {
      req(input$scenario)
      req(input$path)

      newRow <- data.frame(
        DataCombinedName = input$DataCombinedName,
        dataType = input$dataType, label = input$label,
        scenario = input$scenario,
        path = input$path, group = input$group,
        xOffsets = input$xOffsets, yOffsets = input$yOffsets,
        xScaleFactors = input$xScaleFactors, yScaleFactors = input$yScaleFactors
      )
    } else {
      req(input$dataSet)
      newRow <- data.frame(
        DataCombinedName = input$DataCombinedName,
        dataType = input$dataType, label = input$label,
        dataSet = input$dataSet, group = input$group,
        xOffsets = input$xOffsets, yOffsets = input$yOffsets,
        xScaleFactors = input$xScaleFactors, yScaleFactors = input$yScaleFactors
      )
    }

    if (!is.null(r$newInputsTab1)) {
      addColumns <- data.frame(t(sapply(r$newInputsTab1, function(x) {
        input[[x]]
      })))
      newRow <- bind_cols(newRow, addColumns)
    }

    r$dfDataCombined <- bind_rows(r$dfDataCombined, newRow)
  })

  observeEvent(input$addPlot, {
    if (is.na(input$plotID) || input$plotID == "" || input$plotID == " " || input$plotID %in% r$dfPlots$plotID) {
      showNotification(ui = "plotID is missing or already exists.")
      return()
    }
    req(input$plotID)

    newRow <- data.frame(
      plotID = input$plotID,
      DataCombinedName = input$PlotDataCombined,
      plotType = input$plotType,
      title = input$plotTitle,
      xUnit = input$xUnit,
      yUnit = input$yUnit,
      xAxisScale = input$xAxisScale,
      yAxisScale = input$yAxisScale,
      xAxisLimits = input$xAxisLimits,
      yAxisLimits = input$yAxisLimits,
      quantiles = input$quantiles,
      foldDistance = input$foldDistance
    )

    if (!is.null(r$newInputsTab2)) {
      addColumns <- data.frame(t(sapply(r$newInputsTab2, function(x) {
        input[[x]]
      })))
      newRow <- bind_cols(newRow, addColumns)
    }

    r$dfPlots <- bind_rows(r$dfPlots, newRow)
  })

  observeEvent(input$addPlotGrid, {
    if (is.na(input$plotGridName) || input$plotGridName == "" || input$plotGridName == " ") {
      showNotification(ui = "plotGridName is missing.")
    }
    req(input$plotGridName)

    if (gsub(" ", "", input$plotGridName) == "" || is.null(input$plotIDs)) {
      output$infoAddPlotGrid <- renderText("Please fill in fields 'name' and 'plotIDs'")
      return()
    }
    if (gsub(" ", "", input$plotGridName) %in% r$dfPlotGrids$name) {
      output$infoAddPlotGrid <- renderText("Plot grid already exists, please choose another name")
      return()
    }

    newRow <- data.frame(
      name = input$plotGridName,
      plotIDs = paste(input$plotIDs, collapse = ", "),
      title = input$plotGridTitle
    )
    if (!is.null(r$newInputsTab3)) {
      addColumns <- data.frame(t(sapply(r$newInputsTab3, function(x) {
        input[[x]]
      })))
      newRow <- bind_cols(newRow, addColumns)
    }
    r$dfPlotGrids <- bind_rows(r$dfPlotGrids, newRow)
  })

  ####### Add more input fields ########

  # ... in DataCombined tab
  observeEvent(input$addColumnDataCombined, {
    showModal(modalDialog(
      tags$h2("Enter name of new input field"),
      textInput("newInputDataCombined", "Name"),
      footer = tagList(
        actionButton("submit1", "Submit"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$submit1, {
    removeModal()
    # add new input field name to reactive variable newInputsTab1 if it does not exist yet
    if (is.null(input[[input$newInputDataCombined]])) {
      r$newInputsTab1 <- c(r$newInputsTab1, input$newInputDataCombined)
    } else {
      showNotification(ui = paste("Input field", input$newInputDataCombined, "already exists."))
    }
  })

  output$newInputsTab1 <- renderUI({
    variable_list <- lapply(r$newInputsTab1, function(x) {
      column(2, textInput(x, x))
    })
  })


  # ... in plotConfiguration tab
  observeEvent(input$addColumnPlotConfiguration, {
    showModal(modalDialog(
      tags$h2("Enter name of new input field"),
      selectInput("newInputPlotConfiguration", "Name", choices = names(DefaultPlotConfiguration$public_fields)),
      footer = tagList(
        actionButton("submit2", "Submit"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$submit2, {
    removeModal()
    # add new input field name to reactive variable newInputsTab2 if it does not exist yet
    if (is.null(input[[input$newInputPlotConfiguration]])) {
      r$newInputsTab2 <- c(r$newInputsTab2, input$newInputPlotConfiguration)
    } else {
      showNotification(ui = paste("Input field", input$newInputPlotConfiguration, "already exists."))
    }
  })

  output$newInputsTab2 <- renderUI({
    variable_list <- lapply(r$newInputsTab2, function(x) {
      column(2, textInput(x, x))
    })
  })

  # ... in plotGrid tab
  observeEvent(input$addColumnPlotGrid, {
    showModal(modalDialog(
      tags$h2("Enter name of new input field"),
      selectInput("newInputPlotGrid", "Name", choices = names(PlotGridConfiguration$public_fields)),
      footer = tagList(
        actionButton("submit3", "Submit"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$submit3, {
    removeModal()
    # add new input field name to reactive variable newInputsTab3 if it does not exist yet
    if (is.null(input[[input$newInputPlotGrid]])) {
      r$newInputsTab3 <- c(r$newInputsTab3, input$newInputPlotGrid)
    } else {
      showNotification(ui = paste("Input field", input$input$newInputPlotGrid, "already exists."))
    }
  })

  output$newInputsTab3 <- renderUI({
    variable_list <- lapply(r$newInputsTab3, function(x) {
      column(2, textInput(x, x))
    })
  })

  ####### SAVE OUTPUT ########

  # save changes to projectConfiguration$plotsFile
  observeEvent(input$save, {
    tryCatch(
      {
        # save changes in current sheet/rhandsontable first
        switch(r$currentTab,
               DataCombined = {
                 r$dfDataCombined <- hot_to_r(input$currentSheet)
               },
               plotConfiguration = {
                 r$dfPlots <- hot_to_r(input$currentSheet)
               },
               plotGrids = {
                 r$dfPlotGrids <- hot_to_r(input$currentSheet)
               }
        )
        writeExcel(list(
          "DataCombined" = r$dfDataCombined,
          "plotConfiguration" = r$dfPlots,
          "plotGrids" = r$dfPlotGrids
        ), path = r$plotFile)
        showNotification(ui = "File saved")
      },
      error = function(cond) {
        warning(cond$message)
        showNotification(ui = cond$message)
        showNotification(ui = "plotsFile could not be saved")
      }
    )
  })

  # save data frames of plotsFile in new file
  observeEvent(input$saveNew, {
    tryCatch(
      {
        # save changes in current sheet/rhandsontable first
        switch(r$currentTab,
               DataCombined = {
                 r$dfDataCombined <- hot_to_r(input$currentSheet)
               },
               plotConfiguration = {
                 r$dfPlots <- hot_to_r(input$currentSheet)
               },
               plotGrids = {
                 r$dfPlotGrids <- hot_to_r(input$currentSheet)
               }
        )
        writeExcel(list(
          "DataCombined" = r$dfDataCombined %||% data.frame(),
          "plotConfiguration" = r$dfPlots %||% data.frame(),
          "plotGrids" = r$dfPlotGrids %||% data.frame()
        ), path = file.choose(new = TRUE))
        showNotification(ui = "File saved")
      },
      error = function(cond) {
        warning(cond$message)
        showNotification(ui = cond$message)
        showNotification(ui = "New file could not be created")
      }
    )
  })

  ####### GENERAL OBSERVERS ##########

  # Update Inputs based on data contents
  observeEvent(r$dfDataCombined, {
    choices <- if (is.null(r$dfDataCombined)) {
      ""
    } else {
      r$dfDataCombined$DataCombinedName
    }
    updateSelectizeInput(session, "PlotDataCombined", choices = choices)
  })

  observeEvent(r$dfPlots, {
    choices <- if (is.null(r$dfPlots)) {
      ""
    } else {
      r$dfPlots$plotID
    }
    updateSelectizeInput(session, "plotIDs", choices = choices)
  })


  observeEvent(r$dfScenarios, {
    choices <- if (is.null(r$dfScenarios)) {
      ""
    } else {
      r$dfScenarios$Scenario_name
    }
    updateSelectizeInput(session, "scenario", choices = choices, options = list(create = TRUE))
  })

  observeEvent(r$dfOutputPaths, {
    choices <- if (is.null(r$dfOutputPaths)) {
      ""
    } else {
      r$dfOutputPaths$OutputPath
    }
    updateSelectizeInput(session, "path", choices = choices, options = list(create = TRUE))
  })


  observeEvent(r$dataSets, {
    choices <- if (is.null(r$dataSets)) {
      ""
    } else {
      names(r$dataSets)
    }
    updateSelectizeInput(session, "dataSet", choices = choices, options = list(create = TRUE))
  })

  # load observed data, only chosen sheets
  observeEvent(input$loadDataSets, {
    req(r$newProjectConfiguration)

    dataImporterConfiguration <- ospsuite::loadDataImporterConfiguration(r$newProjectConfiguration$dataImporterConfigurationFile)
    dataImporterConfiguration$sheets <- input$sheetsToLoad
    tryCatch(
      {
        r$dataSets <- ospsuite::loadDataSetsFromExcel(
          xlsFilePath = r$newProjectConfiguration$dataFile,
          importerConfigurationOrPath = dataImporterConfiguration
        )
      },
      error = function(cond) {
        warning(cond$message)
        showNotification(ui = "No data loaded.", duration = NULL)
        showNotification(ui = HTML(paste(capture.output(cat(cond$message)), collapse = "<br/>")), duration = NULL)
      },
      warning = function(cond) {
        warning(cond$message)
        showNotification(ui = "No data loaded.", duration = NULL)
        showNotification(ui = HTML(paste(capture.output(cat(cond$message)), collapse = "<br/>")), duration = NULL)
      }
    )
  })

  # Enable/Disable some fields depending on dataType input
  observeEvent(input$dataType, {
    output$infoAddData <- NULL
    # enable fields 'scenario' and 'path', disable 'dataSet' for simulated data
    if (input$dataType == "simulated") {
      shinyjs::enable("scenario")
      shinyjs::enable("path")
      updateTextInput(session, "dataSet", value = "")
      shinyjs::disable("dataSet")
      # disable fields 'scenario' and 'path', enable 'dataSet' for observed data
    } else {
      shinyjs::disable("scenario")
      shinyjs::disable("path")
      shinyjs::enable("dataSet")
    }
  })

  # disable 'Save plotsFile' button, when no file directory is given by project configuration
  observe(
    if (is.null(r$plotFile)) {
      shinyjs::disable("save")
    } else {
      shinyjs::enable("save")
    }
  )

  # Display notification if one input is missing for adding data
  observeEvent(input$addData, {
    necessary_inputs <- c("DataCombinedName", "label")

    if (input$dataType == "simulated") {
      necessary_inputs <- c(necessary_inputs, "scenario", "path")
    } else {
      necessary_inputs <- c(necessary_inputs, "dataSet")
    }

    for (ni in necessary_inputs) {
      if (is.na(input[[ni]]) || input[[ni]] == "" || input[[ni]] == " ") {
        showNotification(ui = paste(ni, "is missing."))
      }
    }
  })

  observeEvent(input$addPlot, {
    necessary_inputs <- c("DataCombinedName", "label")

    if (input$dataType == "simulated") {
      necessary_inputs <- c(necessary_inputs, "scenario", "path")
    } else {
      necessary_inputs <- c(necessary_inputs, "dataSet")
    }

    for (ni in necessary_inputs) {
      if (is.na(input[[ni]]) || input[[ni]] == "" || input[[ni]] == " ") {
        showNotification(ui = paste(ni, "is missing."))
      }
    }
  })

  # when changing tabs: save changes made in the previous sheet/dataframe to
  # corresponding reactive value
  observeEvent(input$tabset1, {
    r$previousTab <- r$currentTab
    switch(r$previousTab,
           DataCombined = {
             r$dfDataCombined <- hot_to_r(input$currentSheet)
           },
           plotConfiguration = {
             r$dfPlots <- hot_to_r(input$currentSheet)
           },
           plotGrids = {
             r$dfPlotGrids <- hot_to_r(input$currentSheet)
           }
    )
    r$currentTab <- input$tabset1
  })

  # OUTPUTS
  # render the sheet which is currently edited - DataCombined, plots or plotGrids
  output$currentSheet <- renderRHandsontable({
    req(r$currentTab)
    switch(r$currentTab,
           DataCombined = {
             if (!is.null(r$dfDataCombined)) {
               rhandsontable(r$dfDataCombined, height = 300)
             } else {
               return()
             }
           },
           plotConfiguration = {
             if (!is.null(r$dfPlots)) {
               rhandsontable(r$dfPlots, height = 300)
             } else {
               return()
             }
           },
           plotGrids = {
             if (!is.null(r$dfPlotGrids)) {
               rhandsontable(r$dfPlotGrids, height = 300)
             } else {
               return()
             }
           }
    ) %>%
      # enable column sorting, reordering and resizing
      hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE)
  })

  # Display a message describing imported file status
  output$currentProjConfig <- renderText({
    req(r$projConfigFileMessage)
    r$projConfigFileMessage
  })

  output$currentPlotsFile <- renderText({
    req(r$plotsFileMessage)
    r$plotsFileMessage
  })
}

shinyApp(ui = ui, server = server)
