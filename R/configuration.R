#' @title Configuration Object
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
        cli_end()
      }

      if (lod == 2) {
        cli_ul()
        cli_li("Scenarios:")
        scenarios <- cli_ul()
        purrr::map(names(self$scenarios), ~ cli_li(.x))
        cli_end(scenarios)
        cli_li("Models:")
        models <- cli_ul()
        purrr::map(self$models, ~ cli_li(.x))
        cli_end(models)
        cli_li("Model Parameters:")
        modelParameters <- cli_ul()
        purrr::map(names(self$modelParameters), ~ cli_li(.x))
        cli_end(modelParameters)
        cli_li("Individuals:")
        individuals <- cli_ul()
        purrr::map(names(self$individuals), ~ cli_li(.x))
        cli_end(individuals)
        cli_li("Applications:")
        applications <- cli_ul()
        purrr::map(names(self$applications), ~ cli_li(.x))
        cli_end(applications)
      }
    }
  ),
  private = list(
    .project = NULL,
    .scenarios = NULL,
    .models = NULL,
    .modelParameters = NULL,
    .individuals = NULL,
    .applications = NULL
  ),
  active = list(
    #' @field scenarios all the scenario configurations defined in the project
    scenarios = function(value) {
      if (is.null(private$.scenarios)) {
        private$.scenarios <- createScenariosConfigurations(private$.project)
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
        private$.modelParameters <- createModelParametersConfigurations(private$.project)
      }
      if (!missing(value)) {
        private$.modelParameters <- modifyList(private$.modelParameters, value)
      }
      return(private$.modelParameters)
    },
    #' @field individuals all the individuals configurations defined in the project
    individuals = function(value) {
      if (is.null(private$.individuals)) {
        private$.individuals <- createIndividualsConfigurations(private$.project)
      }
      if (!missing(value)) {
        private$.individuals <- modifyList(private$.individuals, value)
      }
      return(private$.individuals)
    },
    #' @field applications all the applications configurations defined in the project
    applications = function(value) {
      if (is.null(private$.applications)) {
        private$.applications <- createApplicationsConfigurations(private$.project)
      }
      if (!missing(value)) {
        private$.applications <- modifyList(private$.applications, value)
      }
      return(private$.applications)
    }
  )
)


createScenariosConfigurations <- function(project) {
  scenariosConfigurationData <- readExcel(
    path = project$projectConfiguration$scenariosFile,
    sheet = "Scenarios"
  )

  checkScenariosFileStructure(
    filePath = project$projectConfiguration$scenariosFile,
    data = scenariosConfigurationData
  )

  scenarios <- list()

  for (i in 1:nrow(scenariosConfigurationData)) {
    scenarioConfigurationData <- scenariosConfigurationData[i, ]
    scenarios[[scenarioConfigurationData$Scenario_name]] <- ScenarioConfiguration$new(project, scenarioConfigurationData)
  }

  return(scenarios)
}

createIndividualsConfigurations <- function(project) {
  individualFilePath <- project$projectConfiguration$individualsFile
  individualsSheets <- readxl::excel_sheets(individualFilePath)
  individualsCharacteristicsData <- readExcel(individualFilePath, sheet = 1)

  checkIndividualsFileStructure(
    filePath = individualFilePath,
    data = individualsCharacteristicsData
  )

  individuals <- list()

  for (i in 1:nrow(individualsCharacteristicsData)) {
    individualCharacteristicsData <- individualsCharacteristicsData[i, ]
    individualId <- individualCharacteristicsData$IndividualId

    if (individualId %in% individualsSheets) {
      individualParameters <- createParametersFromSheet(project, individualFilePath, individualId)
    }

    individuals[[individualId]] <-
      Individual$new(
        project = project,
        individualCharacteristicsData = individualCharacteristicsData,
        individualParameters = individualParameters %||% NULL
      )
  }
  return(individuals)
}


createModelParametersConfigurations <- function(project) {
  return(
    createParametersFromFile(
      project,
      project$projectConfiguration$modelParamsFile
    )
  )
}

createApplicationsConfigurations <- function(project) {
  return(
    createParametersFromFile(
      project,
      project$projectConfiguration$applicationsFile
    )
  )
}


checkScenariosFileStructure <- function(filePath, data) {
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
