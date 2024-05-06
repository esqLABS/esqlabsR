ModelParameter <- R6::R6Class(
  classname = "ModelParameter",
  public = list(
    initialize = function(project, modelParameterData) {
      private$.project <- project
      self$containerPath <- modelParameterData$`Container Path`
      self$parameterName <- modelParameterData$`Parameter Name`
      self$value <- modelParameterData$Value
      self$units <- modelParameterData$Units
    },
    print = function() {
      print(private$.parameter)
    },
    toDataFrame = function() {
      return(
        tibble::tibble(
          `Container Path` = self$containerPath,
          `Parameter Name` = self$parameterName,
          Value = self$value,
          Units = self$units
        )
      )
    }
  ),
  active = list(
    containerPath = function(value) {
      if (!missing(value)) {
        private$.parameter$containerPath <- value
      }
      return(private$.parameter$containerPath)
    },
    parameterName = function(value) {
      if (!missing(value)) {
        private$.parameter$parameterName <- value
      }
      return(private$.parameter$parameterName)
    },
    value = function(value) {
      if (!missing(value)) {
        private$.parameter$value <- value
      }
      return(private$.parameter$value)
    },
    units = function(value) {
      if (!missing(value)) {
        if (is.na(value)){
          value <- ""
        }
        private$.parameter$units <- value
      }
      return(private$.parameter$units)
    },
    parameterObject = function() {
      return(
        list(
          paths = paste(self$containerPath, self$parameterName, sep = "|"),
          values = self$value,
          units = self$units
        )
      )
    }
  ),
  private = list(
    .project = NULL,
    .parameter = list()
  )
)


checkParametersFileStructure <- function(filePath, data) {
  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")

  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath = filePath, expectedColNames = columnNames))
  }
}

createParametersFromFile <- function(project, filePath) {
  fileModelParameters <- list()
  sheets <- readxl::excel_sheets(filePath)
  for (sheet in sheets) {
    fileModelParameters[[sheet]] <- createParametersFromSheet(project = project, filePath = filePath, sheet = sheet)
  }
  return(fileModelParameters)
}

createParametersFromSheet <- function(project, filePath, sheet) {
  sheetModelParametersData <- readExcel(filePath, sheet = sheet)
  checkParametersFileStructure(
    filePath = filePath,
    data = sheetModelParametersData
  )
  sheetModelParameters <- list()
  for (i in 1:nrow(sheetModelParametersData)) {
    parameterData <- sheetModelParametersData[i, ]
    parameterID <- parameterData$`Parameter Name`

    i <- 0
    while (parameterID %in% names(sheetModelParameters)) {
      parameterID <- parameterData$`Parameter Name`
      parameterID <- paste(
        stringr::str_replace_all(
          stringr::str_extract(
            parameterData$`Container Path`,
            paste0(paste(rep("[^\\|]*", i), collapse = "\\|"), "[^\\|]*$")
          ), "\\|", "_"
        ),
        parameterID,
        sep = "_"
      )
      i <- i + 1
    }
    sheetModelParameters[[parameterID]] <-
      ModelParameter$new(
        project = project,
        modelParameterData = parameterData
      )
  }
  return(sheetModelParameters)
}


flattenParameterObjects <- function(parameterObjects) {
  list(
    paths = purrr::map(parameterObjects, ~ purrr::pluck(.x, "paths")) %>% purrr::flatten_chr(),
    values = purrr::map(parameterObjects, ~ purrr::pluck(.x, "values")) %>% purrr::flatten_dbl(),
    units = purrr::map(parameterObjects, ~ purrr::pluck(.x, "units")) %>% purrr::flatten_chr()
  )
}
