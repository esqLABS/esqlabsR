#' @title Configuration
#' @description A class representing all configurations files living in a project.
#' This includes:
#' - Scenario Configurations
#' - Models
#' - Model Parameters
#' - Individuals Parameters
#' - Applications Parameters
#' - Population Parameters (#TODO)
Configuration <- R6::R6Class(
  "Configuration",
  public = list(
    #' @description Creates a new instance of Configuration
    #' @param project A Project in which the configurations are defined.
    initialize = function(project) {
      #' @description Creates a new instance of Configuration
      #' @param project A Project in which the configurations are defined.
      private$.project <- project
    },
    #' @description Prints the configurations
    #' @param lod Level of detail to print.
    #' - 1: Print only the number of configurations per category.
    #' - 2 (default): For each category, list all available configurations.
    print = function(lod = 2) {
      cli_h1("Configurations")

      if (lod == 1) {
        cli_ul()
        cli_li("Scenarios: {length(self$scenarios)}")
        cli_li("Models: {length(self$models)}")
        cli_li("Model Parameters: {length(self$modelParameters)}")
        cli_li("Individuals: {length(self$individuals)}")
        cli_li("Plots: {length(self$plots)}")
        cli_li("Applications: {length(self$applications)}")
        cli_li("OutputPaths: {length(self$outputPaths)}")
        cli_end()
      }

      if (lod == 2) {
        cli_ul()
        cli_li("Scenarios:")
        scenarios <- cli_ul()
        purrr::map(names(self$scenarios), ~ cli_li(.x))
        cli_end(scenarios)
        cli_li("OutputPaths:")
        outpuPaths <- cli_ul()
        purrr::imap(self$outputPaths, ~ cli_li(.y))
        cli_end(outpuPaths)
        cli_li("Models:")
        models <- cli_ul()
        purrr::map(self$models, ~ cli_li(.x))
        cli_end(models)
        cli_li("Model Parameters:")
        modelParameters <- cli_ul()
        purrr::imap(self$modelParameters, ~ cli_li(.y))
        cli_end(modelParameters)
        cli_li("Individuals:")
        individuals <- cli_ul()
        purrr::imap(self$individuals, ~ cli_li(.y))
        cli_end(individuals)
        cli_li("Plots:")
        plots <- cli_ul()
        purrr::imap(self$plots, ~ cli_li(.y))
        cli_end(plots)
        cli_li("Applications:")
        applications <- cli_ul()
        purrr::imap(self$applications, ~ cli_li(.y))
        cli_end(applications)
      }

      invisible(self)
    }
  ),
  active = list(
    #' @field scenarios all the scenario configurations defined in the project
    scenarios = function(value) {
      if (is.null(private$.scenarios)) {
        private$.scenarios <- private$.createScenariosConfigurations()
      }
      if (!missing(value)) {
        private$.scenarios <- value
      }
      return(private$.scenarios)
    },
    #' @field models all the model files (.pkml files) available in the project
    models = function(value) {
      if (is.null(private$.models)) {
        private$.models <- list.files(
          private$.project$projectConfiguration$modelFolder,
          pattern = ".pkml$"
        )
      }
      if (!missing(value)) {
        stop("Cannot set available models. Theses are automatically listed from", private$.project$projectConfiguration$modelFolder)
      }
      return(private$.models)
    },
    #' @field modelParameters all the model parameters configurations defined in the project
    modelParameters = function(value) {
      if (is.null(private$.modelParameters)) {
        private$.modelParameters <- private$.createModelParametersConfigurations()
      }
      if (!missing(value)) {
        private$.modelParameters <- modifyList(private$.modelParameters, value)
      }
      return(private$.modelParameters)
    },
    #' @field individuals all the individuals configurations defined in the project
    individuals = function(value) {
      if (is.null(private$.individuals)) {
        private$.individuals <- private$.createIndividualsConfigurations()
      }
      if (!missing(value)) {
        private$.individuals <- modifyList(private$.individuals, value)
      }
      return(private$.individuals)
    },
    #' @field plots all the plots configurations defined in the project
    plots = function(value) {
      if (is.null(private$.plots)) {
        private$.plots <- private$.createPlotsConfigurations()
      }
      if (!missing(value)) {
        private$.plots <- modifyList(private$.plots, value)
      }
      return(private$.plots)
    },
    #' @field populations all the populations configurations defined in the project
    populations = function(value) {
      if (is.null(private$.populations)) {
        private$.populations <- private$.createPopulationsConfigurations()
      }
      if (!missing(value)) {
        private$.populations <- modifyList(private$.populations, value)
      }
      return(private$.populations)
    },
    #' @field applications all the applications configurations defined in the project
    applications = function(value) {
      if (is.null(private$.applications)) {
        private$.applications <- private$.createApplicationsConfigurations()
      }
      if (!missing(value)) {
        private$.applications <- modifyList(private$.applications, value)
      }
      return(private$.applications)
    },
    #' @field outputPaths all the output paths configurations defined in the project
    outputPaths = function(value) {
      if (is.null(private$.outputPaths)) {
        private$.outputPaths <- private$.createOutputPaths()
      }
      if (!missing(value)) {
        private$.outputPaths <- modifyList(private$.outputPaths, value)
      }
      return(private$.outputPaths)
    }
  ),
  private = list(
    .project = NULL,
    .scenarios = NULL,
    .models = NULL,
    .modelParameters = NULL,
    .individuals = NULL,
    .populations = NULL,
    .plots = NULL,
    .applications = NULL,
    .outputPaths = NULL,
    .createScenariosConfigurations = function() {
      scenariosConfigurationData <- readExcel(
        path = private$.project$projectConfiguration$scenariosFile,
        sheet = "Scenarios"
      )

      validateScenariosFileStructure(
        filePath = private$.project$projectConfiguration$scenariosFile,
        data = scenariosConfigurationData
      )

      scenarios <- list()

      for (i in 1:nrow(scenariosConfigurationData)) {
        scenarioConfigurationData <- scenariosConfigurationData[i, ]
        scenarios[[scenarioConfigurationData$Scenario_name]] <- ScenarioConfiguration$new(private$.project, scenarioConfigurationData)
      }

      return(scenarios)
    },
    .createIndividualsConfigurations = function() {
      individualFilePath <- private$.project$projectConfiguration$individualsFile
      individualsSheets <- readxl::excel_sheets(individualFilePath)
      individualsCharacteristicsData <- readExcel(individualFilePath, sheet = 1)

      validateIndividualsFileStructure(
        filePath = individualFilePath,
        data = individualsCharacteristicsData
      )

      individuals <- list()

      for (i in 1:nrow(individualsCharacteristicsData)) {
        individualCharacteristicsData <- individualsCharacteristicsData[i, ]
        individualId <- individualCharacteristicsData$IndividualId

        if (individualId %in% individualsSheets) {
          individualParameters <- createParametersFromSheet(private$.project, individualFilePath, individualId)
        }

        individuals[[individualId]] <-
          Individual$new(
            project = private$.project,
            id = individualId,
            individualCharacteristicsData = individualCharacteristicsData,
            individualParameters = individualParameters %||% NULL
          )
      }
      return(individuals)
    },
    .createPopulationsConfigurations = function() {
      populationsFilePath <- private$.project$projectConfiguration$populationsFile
      populationsCharacteristicsData <- readExcel(populationsFilePath, sheet = 1)
      userDefinedVariabilityData <- readExcel(populationsFilePath, sheet = 2)

      validatePopulationsFileStructure(
        filePath = populationsFilePath,
        data = populationsCharacteristicsData
      )

      populations <- list(
        fromConfiguration = list(),
        fromCSV = list()
      )

      for (i in 1:nrow(populationsCharacteristicsData)) {
        populationCharacteristicsData <- populationsCharacteristicsData[i, ]
        populationId <- populationCharacteristicsData$PopulationName

        populations$fromConfiguration[[populationId]] <- Population$new(
          project = private$.project,
          id = populationId,
          populationCharacteristicsData = populationCharacteristicsData,
          userDefinedVariabilityData = userDefinedVariabilityData,
          CSVFile = NULL
        )
      }


      for (file in list.files(private$.project$projectConfiguration$populationsFolder,
        full.names = TRUE,
        pattern = ".csv"
      )) {
        populationId <- tools::file_path_sans_ext(basename(file))

        populations$fromCSV[[populationId]] <- Population$new(
          project = private$.project,
          id = populationId,
          populationCharacteristicsData = NULL,
          userDefinedVariabilityData = userDefinedVariabilityData,
          CSVFile = file
        )
      }
      return(populations)
    },
    .createPlotsConfigurations = function() {
      plotFilePath <- private$.project$projectConfiguration$plotsFile
      plotsSheets <- readxl::excel_sheets(plotFilePath)
      plotsCharacteristicsData <- readExcel(plotFilePath, sheet = 2)
      validatePlotsFileStructure(
        filePath = plotFilePath,
        data = plotsCharacteristicsData
      )
      plots <- list()
      for (i in 1:nrow(plotsCharacteristicsData)) {
        plotCharacteristicsData <- plotsCharacteristicsData[i, ]
        plotId <- plotCharacteristicsData$plotID
        if (plotId %in% plotsSheets) {
          plotParameters <- createParametersFromSheet(private$.project, plotFilePath, plotId)
        }
        plots[[plotId]] <- list(
          DataCombinedName = plotCharacteristicsData$DataCombinedName,
          plotType = plotCharacteristicsData$plotType,
          title = plotCharacteristicsData$title,
          xUnit = plotCharacteristicsData$xUnit,
          yUnit = plotCharacteristicsData$yUnit,
          xAxisScale = plotCharacteristicsData$xAxisScale,
          yAxisScale = plotCharacteristicsData$yAxisScale,
          xValuesLimits = plotCharacteristicsData$xValuesLimits,
          yValuesLimits = plotCharacteristicsData$yValuesLimits,
          aggregation = plotCharacteristicsData$aggregation,
          quantiles = plotCharacteristicsData$quantiles,
          nsd = plotCharacteristicsData$nsd,
          foldDistance = plotCharacteristicsData$foldDistance
        )
      }
      return(plots)
    },
    .createModelParametersConfigurations = function() {
      return(
        createParametersFromFile(
          private$.project,
          private$.project$projectConfiguration$modelParamsFile
        )
      )
    },
    .createApplicationsConfigurations = function() {
      return(
        createParametersFromFile(
          private$.project,
          private$.project$projectConfiguration$applicationsFile
        )
      )
    },
    .createOutputPaths = function() {
      outputPathsData <- readExcel(
        path = private$.project$projectConfiguration$scenariosFile,
        sheet = "OutputPaths"
      )

      outputPaths <- list()

      for (i in 1:nrow(outputPathsData)) {
        outputPathData <- outputPathsData[i, ]
        outputPaths[[outputPathData$OutputPathId]] <- outputPathData$OutputPath
      }

      return(outputPaths)
    }
  )
)


validateScenariosFileStructure <- function(filePath, data) {
  columnNames <- c(
    "Scenario_name",
    "IndividualId",
    "PopulationId",
    "ReadPopulationFromCSV",
    "ModelParameterSheets",
    "ApplicationProtocol",
    "SimulationTime",
    "SimulationTimeUnit",
    "SteadyState",
    "SteadyStateTime",
    "SteadyStateTimeUnit",
    "ModelFile",
    "OutputPathsIds"
  )

  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath, expectedColNames = columnNames))
  }
}

validateIndividualsFileStructure <- function(filePath, data) {
  columnNames <- c(
    "IndividualId",
    "Species",
    "Population",
    "Gender",
    "Weight [kg]",
    "Height [cm]",
    "Age [year(s)]",
    "Protein",
    "Ontogeny"
  )

  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath, expectedColNames = columnNames))
  }
}
validatePopulationsFileStructure <- function(filePath, data) {
  columnNames <- c(
    "PopulationName", "species", "population", "numberOfIndividuals", "proportionOfFemales", "weightMin", "weightMax",
    "weightUnit", "heightMin", "heightMax", "heightUnit", "ageMin", "ageMax", "BMIMin", "BMIMax", "BMIUnit", "Protein", "Ontogeny"
  )

  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath, expectedColNames = columnNames))
  }
}
validatePlotsFileStructure <- function(filePath, data) {
  columnNames <- c(
    "plotID",
    "DataCombinedName",
    "plotType",
    "xUnit",
    "yUnit",
    "xAxisScale",
    "yAxisScale",
    "xValuesLimits",
    "yValuesLimits",
    "aggregation",
    "quantiles",
    "nsd",
    "foldDistance"
  )
  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath, expectedColNames = columnNames))
  }
}



checkExists <- function(type, names) {
  notFound <- c()
  for (name in names) {
    if (!(name %in% names(private$.project$configurations[[type]]) || name %in% private$.project$configurations[[type]])) {
      notFound <- c(notFound, name)
    }
  }
  if (length(notFound) > 0) {
    stop(paste0("The following ", type, " were not found: ", paste0(notFound, collapse = ", ")))
  }
}
