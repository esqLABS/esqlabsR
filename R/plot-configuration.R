#' PlotConfiguration Class
#'
#' @description
#' This R6 class is used to manage and process plot configurations from an Excel file. It handles the
#' creation of different configurations like `DataCombined`, `PlotConfiguration`, `PlotGrids`, and
#' `ExportConfiguration` by reading them from specific sheets in the provided Excel file. The class also
#' validates the structure of each sheet and creates empty configurations if necessary.
#'
#' @field .filePath A string containing the path to the Excel file where configuration data is stored.
#' @field .plotConfig A list to hold various configurations like DataCombined, PlotConfiguration, PlotGrids, and ExportConfiguration.
#'
#' @export
#' PlotConfiguration Class
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  private = list(
    # Private fields
    .filePath = NULL,         #' @field .filePath - Path to the Excel file containing configuration sheets
    .plotConfig = NULL,       #' @field .plotConfig - List containing all configurations after processing the Excel sheets

    #' Private method to create DataCombined configuration
    #' @description Reads and processes the "DataCombined" sheet from the Excel file. It validates the structure and processes data
    #' by organizing it based on data type.
    #'
    #' @return A list containing the DataCombined configuration.
    .createDataCombinedFromSheet = function() {
      dataCombinedData <- readExcel(path = private$.filePath, sheet = "DataCombined")
      private$.validatePlotsSheetFileStructure(dataCombinedData, dataCombinedDefaultColumns)

      # Validate and create an empty configuration if data is missing
      dataCombined <- private$.validateAndCreateEmptyConfiguration(dataCombinedData, dataCombinedDefaultColumns)

      if (nrow(dataCombinedData) > 0) {
        # Process non-empty data and organize it by label
        for (label in unique(dataCombinedData$label)) {
          dataCombinedDataFilteredByLabel <- dataCombinedData[dataCombinedData$label == label, ]
          for (i in 1:nrow(dataCombinedDataFilteredByLabel)) {
            dataCombinedRecord <- dataCombinedDataFilteredByLabel[i, ]
            for (column in dataCombinedDefaultColumns[-3]) {
              dataCombined[[dataCombinedRecord$DataCombinedName]][[label]][[column]] <- dataCombinedRecord[[column]]
            }
          }
        }
      }

      return(dataCombined)
    },

    #' @description Reads and processes the "plotConfiguration" sheet from the Excel file. It validates the structure and processes data
    #' by organizing it based on plot ID.
    #'
    #' @return A list containing the PlotConfiguration data.
    .createPlotConfigurationFromSheet = function() {
      plotConfigurationsData <- readExcel(path = private$.filePath, sheet = "plotConfiguration")
      private$.validatePlotsSheetFileStructure(plotConfigurationsData, plotConfigurationDefaultColumns)

      # Validate and create an empty configuration if data is missing
      plotConfigurations <- private$.validateAndCreateEmptyConfiguration(plotConfigurationsData, plotConfigurationDefaultColumns)

      if (nrow(plotConfigurationsData) > 0) {
        # Process non-empty data and organize it by plot ID
        for (i in 1:nrow(plotConfigurationsData)) {
          plotConfiguration <- plotConfigurationsData[i, ]
          for (column in plotConfigurationDefaultColumns) {
            if (column %in% c("xValuesLimits", "yValuesLimits", "foldDistance")) {
              # Handle columns with comma-separated values
              if (is.na(plotConfiguration[[column]])) {
                plotConfigurations[[plotConfiguration$plotID]][[column]] <- plotConfiguration[[column]]
              } else {
                plotConfigurations[[plotConfiguration$plotID]][[column]] <- as.numeric(unlist(strsplit(plotConfiguration[[column]], ",")))
              }
            } else {
              plotConfigurations[[plotConfiguration$plotID]][[column]] <- plotConfiguration[[column]]
            }
          }
        }
      }

      return(plotConfigurations)
    },

    #' @description Reads and processes the "plotGrids" sheet from the Excel file. It validates the structure and processes data
    #' by organizing it based on plot names and their respective grid layout.
    #'
    #' @return A list containing the PlotGrids data.
    .createPlotGridsFromSheet = function() {
      plotGridsData <- readExcel(path = private$.filePath, sheet = "plotGrids")
      private$.validatePlotsSheetFileStructure(plotGridsData, plotGridsDefaultColumns)

      # Validate and create an empty configuration if data is missing
      plotGrids <- private$.validateAndCreateEmptyConfiguration(plotGridsData, plotGridsDefaultColumns)

      if (nrow(plotGridsData) > 0) {
        # Process non-empty data and handle comma-separated plotIDs
        for (i in 1:nrow(plotGridsData)) {
          plotGrid <- plotGridsData[i, ]
          for (column in plotGridsDefaultColumns) {
            if (column == "plotIDs") {
              if (is.na(plotGrid[[column]])) {
                plotGrids[[plotGrid$name]][[column]] <- plotGrid[[column]]
              } else {
                plotGrids[[plotGrid$name]][[column]] <- trimws(unlist(strsplit(plotGrid[[column]], ",")))
              }
            } else {
              plotGrids[[plotGrid$name]][[column]] <- plotGrid[[column]]
            }
          }
        }
      }

      return(plotGrids)
    },

    #' @description Reads and processes the "exportConfiguration" sheet from the Excel file. It validates the structure and processes
    #' data by organizing export configurations based on plot grid names.
    #'
    #' @return A list containing the ExportConfiguration data.
    .createExportConfigurationFromSheet = function() {
      exportConfigurationsData <- readExcel(path = private$.filePath, sheet = "exportConfiguration")
      private$.validatePlotsSheetFileStructure(exportConfigurationsData, exportConfigurationDefaultColumns)

      # Validate and create an empty configuration if data is missing
      exportConfigurations <- private$.validateAndCreateEmptyConfiguration(exportConfigurationsData, exportConfigurationDefaultColumns)

      if (nrow(exportConfigurationsData) > 0) {
        # Process non-empty data
        for (i in 1:nrow(exportConfigurationsData)) {
          exportConfiguration <- exportConfigurationsData[i, ]
          for (column in exportConfigurationDefaultColumns) {
            exportConfigurations[[exportConfiguration$plotGridName]][[column]] <- exportConfiguration[[column]]
          }
        }
      }

      return(exportConfigurations)
    },

    #' @description Ensures that the Excel sheet being read contains the expected columns.
    #'
    #' @param data A data frame containing the sheet data.
    #' @param defaultColumns A character vector containing the expected column names.
    #' @return None. Stops the process with an error if the sheet structure is invalid.
    .validatePlotsSheetFileStructure = function(data, defaultColumns) {
      if (!all(defaultColumns %in% names(data))) {
        stop(messages$errorWrongXLSStructure(filePath = private$.filePath, expectedColNames = defaultColumns))
      }
    },

    #' @description Creates an empty configuration list if the provided data is missing or has no rows.
    #'
    #' @param configurationsData A data frame containing configuration data.
    #' @param configurationDefaultColumns A character vector of default column names.
    #' @return A list representing the empty configuration.
    .validateAndCreateEmptyConfiguration = function(configurationsData, configurationDefaultColumns) {
      if (nrow(configurationsData) == 0) {
        return(private$.createEmptyConfiguration(configurationDefaultColumns))
      }
      return(list()) # Return an empty list if the data is valid
    },

    #' @description Generates an empty list with keys based on the provided default column names.
    #'
    #' @param configurationDefaultColumns A character vector of default column names.
    #' @return A named list with NA values.
    .createEmptyConfiguration = function(configurationDefaultColumns) {
      emptyConfiguration <- setNames(as.list(rep(NA, length(configurationDefaultColumns))), configurationDefaultColumns)
      return(emptyConfiguration)
    },

    #' @description Initializes and combines all configuration sheets into a single list.
    #'
    #' @return A list containing all the configurations.
    .initializePlotConfiguration = function() {
      private$.plotConfig <- list(
        datacombined = private$.createDataCombinedFromSheet(),
        plotconfiguration = private$.createPlotConfigurationFromSheet(),
        plotgrids = private$.createPlotGridsFromSheet(),
        exportconfiguration = private$.createExportConfigurationFromSheet()
      )
    }
  ),

  public = list(
    #' Public method to initialize the class
    #' @description Initializes the class and processes the Excel sheets for configuration.
    #'
    #' @param filePath A string representing the file path to the Excel file.
    initialize = function(filePath) {
      private$.filePath <- filePath
      config <- private$.initializePlotConfiguration()
    },
    #' @description Retrieves the full plot configuration list.
    #'
    #' @return A list containing all configurations (DataCombined, PlotConfiguration, PlotGrids, ExportConfiguration).
    getPlotConfig = function() {
      return(private$.plotConfig)
    }
  )
)


# ====================== Project Configuration: Column Names ======================
dataCombinedDefaultColumns <- c(
  "DataCombinedName",
  "dataType",
  "label",
  "scenario",
  "path",
  "dataSet",
  "group",
  "xOffsets",
  "xOffsetsUnits",
  "yOffsets",
  "yOffsetsUnits",
  "xScaleFactors",
  "yScaleFactors"
)

plotConfigurationDefaultColumns <- c(
  "plotID",
  "DataCombinedName",
  "plotType",
  "title",
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

plotGridsDefaultColumns <- c("name", "plotIDs", "title")

exportConfigurationDefaultColumns <- c("plotGridName", "outputName", "width")

# ====== Helper Functions: Validate Project Configuration Sheet Names ========
validateSheetsFileNames <- function(filePath) {
  sheets <- readxl::excel_sheets(filePath)
  sheetNames <- c("DataCombined", "plotConfiguration", "plotGrids", "exportConfiguration")

  if (!all(sheetNames %in% sheets)) {
    stop(messages$errorWrongXLSFileSchema(filePath = filePath, expectedSheetNames = sheetNames))
  }
}

