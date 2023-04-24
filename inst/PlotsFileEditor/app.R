#---------- Initialization of app ----------#
# Load required packages
library(shiny)

#---------- User interface ----------#
ui <- fluidPage(
  tags$style(type = "text/css", ".modal-dialog {overflow: auto; width: auto; background:white;}"),
  shinyjs::useShinyjs(),
  headerPanel("PlotsFile Editor"),
  br(),
  actionButton("changeProjConfig", "Change project configuration file"),
  span(textOutput("currentProjConfig"), style = "color:red"),
  br(),
  actionButton("save", "Save plotsFile"),
  actionButton("saveNew", "Save as new file"),
  hr(),
  tabsetPanel(
    tabPanel(
      "DataCombined",
      br(),
      fluidRow(
        column(3, actionButton("addDataCombined", "Add DataCombined")),
        column(3, span(textOutput("infoAddDataCombined"), style = "color:red")),
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
      br(),
      tableOutput("dataCombinedSheet")
    ),
    tabPanel(
      "plotConfiguration",
      br(),
      fluidRow(
        column(2, actionButton("addPlot", "Add Plot")),
        column(3, span(textOutput("infoAddPlot"), style = "color:red")),
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
      br(),
      tableOutput("plotConfigurationSheet")
    ),
    tabPanel(
      "plotGrids",
      br(),
      fluidRow(
        column(2, actionButton("addPlotGrid", "Add Plot Grid")),
        column(3, span(textOutput("infoAddPlotGrid"), style = "color:red")),
      ),
      fluidRow(
        column(3, textInput("plotGridName", label = "name")),
        column(3, selectizeInput("plotIDs", label = "plotIDs", choices = "", multiple = TRUE)),
        column(3, textInput("plotGridTitle", label = "title"))
      ),
      br(),
      tableOutput("plotGridSheet")
    )
  )
)

#---------- Backend logic ----------#
server <- function(input, output, session) {
  dfDataCombined <- reactiveVal(value = NULL)
  dfPlots <- reactiveVal(value = NULL)
  dfPlotGrids <- reactiveVal(value = NULL)
  projectConfigFile <- reactiveVal(value = NULL)
  plotFile <- NULL

  output$currentProjConfig <- renderText({
    if (is.null(projectConfigFile())) {
      " No project configuration selected"
    } else {
      paste(" Current project configuration file:", projectConfigFile())
    }
  })

  changeProjectConfiguration <- function() {
    newConfigFile <-
      tryCatch(
        {
          file.choose()
        },
        error = function(cond) {
          return(NULL)
        }
      )
    newProjectConfiguration <- NULL
    if (!is.null(newConfigFile)) {
      newProjectConfiguration <-
        tryCatch(
          {
            createDefaultProjectConfiguration(newConfigFile)
          },
          error = function(cond) {
            return(NULL)
          }
        )
    }
    if (!is.null(newProjectConfiguration)) {
      projectConfigFile(newConfigFile)
      codeFolder <- dirname(newConfigFile)
      plotFile <<- file.path(codeFolder, newProjectConfiguration$paramsFolder, newProjectConfiguration$plotsFile)

      if (file.exists(plotFile)) {
        dfDataCombined(readExcel(plotFile, sheet = "DataCombined"))
        dfPlots(readExcel(plotFile, sheet = "plotConfiguration"))
        dfPlotGrids(readExcel(plotFile, sheet = "plotGrids"))
        updateSelectizeInput(session, "PlotDataCombined", choices = dfDataCombined()$DataCombinedName)
        updateSelectizeInput(session, "plotIDs", choices = dfPlots()$plotID)
      } else {
        dfDataCombined(NULL)
        dfPlots(NULL)
        dfPlotGrids(NULL)
        updateSelectizeInput(session, "PlotDataCombined", choices = "")
        updateSelectizeInput(session, "plotIDs", choices = "")
      }

      scenarioFile <- file.path(codeFolder, newProjectConfiguration$paramsFolder, newProjectConfiguration$scenarioDefinitionFile)
      if (file.exists(scenarioFile)) {
        dfScenarios <- readExcel(scenarioFile, sheet = "Scenarios")
        dfOutputPaths <- readExcel(scenarioFile, sheet = "OutputPaths")
        updateSelectizeInput(session, "scenario", choices = dfScenarios$Scenario_name, options = list(create = TRUE))
        updateSelectizeInput(session, "path", choices = dfOutputPaths$OutputPath, options = list(create = TRUE))
      }

      # try to load observed data from 'Data' folder as defined in project configuration
      dataFolder <- file.path(codeFolder, newProjectConfiguration$dataFolder)
      dataFile <- file.path(dataFolder, newProjectConfiguration$dataFile)
      dataImporterConfigurationFile <- file.path(dataFolder, newProjectConfiguration$dataImporterConfigurationFile)
      if (file.exists(dataFile) & file.exists(dataImporterConfigurationFile)) {
        tryCatch(
          {
            dataSets <- ospsuite::loadDataSetsFromExcel(
              xlsFilePath = dataFile,
              importerConfigurationOrPath = dataImporterConfigurationFile,
              # currently all sheets will be loaded
              importAllSheets = TRUE
            )
            updateSelectizeInput(session, "dataSet", choices = names(dataSets) %||% "", options = list(create = TRUE))
          },
          error = function(cond) {
            return()
          }
        )
      }
    }
  }

  observeEvent(input$changeProjConfig, {
    changeProjectConfiguration()
  })

  observeEvent(input$dataType, {
    if (input$dataType == "simulated") {
      shinyjs::enable("scenario")
      shinyjs::enable("path")
      updateTextInput(session, "dataSet", value = "")
      shinyjs::disable("dataSet")
    } else {
      shinyjs::disable("scenario")
      shinyjs::disable("path")
      shinyjs::enable("dataSet")
    }
  })

  output$dataCombinedSheet <- renderTable(dfDataCombined())

  output$plotConfigurationSheet <- renderTable(dfPlots())

  output$plotGridSheet <- renderTable(dfPlotGrids())

  observeEvent(input$addDataCombined, {
    # obligatory inputs 'DataCombinedName' and 'label'
    if (gsub(" ", "", input$DataCombinedName) == "" || gsub(" ", "", input$label) == "") {
      output$infoAddDataCombined <- renderText("Please fill in fields 'DataCombinedName' and 'label'")
      return()
    }
    if (input$dataType == "simulated") {
      newRow <- data.frame(
        DataCombinedName = input$DataCombinedName,
        dataType = input$dataType,
        label = input$label,
        scenario = input$scenario,
        path = input$path, group = input$group
      )
    } else {
      if (gsub(" ", "", input$dataSet) == "") {
        output$infoAddDataCombined <- renderText("Please fill in field 'dataSet'")
        return()
      }
      newRow <- data.frame(
        DataCombinedName = input$DataCombinedName,
        dataType = input$dataType,
        label = input$label,
        dataSet = input$dataSet,
        group = input$group
      )
    }
    dfDataCombined(bind_rows(dfDataCombined(), newRow))
    updateSelectizeInput(session, "PlotDataCombined", choices = dfDataCombined()$DataCombinedName)
  })

  observeEvent(input$addPlot, {
    if (gsub(" ", "", input$plotID) == "") {
      output$infoAddPlotGrid <- renderText("Please fill in field 'plotID'")
      return()
    }
    if (gsub(" ", "", input$plotID) %in% dfPlots()$plotID) {
      output$infoAddPlotGrid <- renderText("Plot ID already exists, please choose another ID")
      return()
    }
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
    dfPlots(bind_rows(dfPlots(), newRow))
    updateSelectizeInput(session, "plotIDs", choices = dfPlots()$plotID)
  })

  observeEvent(input$addPlotGrid, {
    if (gsub(" ", "", input$plotGridName) == "" || is.null(input$plotIDs)) {
      output$infoAddPlotGrid <- renderText("Please fill in fields 'name' and 'plotIDs'")
      return()
    }
    if (gsub(" ", "", input$plotGridName) %in% dfPlotGrids()$name) {
      output$infoAddPlot <- renderText("Plot grid already exists, please choose another name")
      return()
    }
    newRow <- data.frame(
      name = input$plotGridName,
      plotIDs = paste(input$plotIDs, collapse = ", "),
      title = input$plotGridTitle
    )
    dfPlotGrids(bind_rows(dfPlotGrids(), newRow))
  })

  # save changes to projectConfiguration$plotsFile
  observeEvent(input$save, {
    writeExcel(list(
      "DataCombined" = dfDataCombined(),
      "plotConfiguration" = dfPlots(),
      "plotGrids" = dfPlotGrids()
    ), path = plotFile)
  })

  # save data frames of plotsFile in new file
  observeEvent(input$saveNew, {
    try(writeExcel(list(
      "DataCombined" = dfDataCombined() %||% data.frame(),
      "plotConfiguration" = dfPlots() %||% data.frame(),
      "plotGrids" = dfPlotGrids() %||% data.frame()
    ), path = file.choose(new = TRUE)))
  })
}

shinyApp(ui = ui, server = server)
