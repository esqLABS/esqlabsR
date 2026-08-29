#' Create `ScenarioConfiguration` objects for each simulation from a given snapshot.
#'
#' Scenario name will be set to simulation name, model file name is considered to be
#' <SimulationName>.pkml.
#'
#' @param projectConfiguration An object of type `ProjectConfiguration`. Required
#' as each `ScenarioConfiguration` object needs a project configuration for creation.
#' @param snapshot PK-Sim snapshot
#'
#' @returns A list of `ScenarioConfiguration` objects, one for each simulation
#'
#' @export
#'
#' @examples
#' # Read snapshot from file
#' snapshotFile <- file.path(
#'   "..",
#'   "Models",
#'   "Snapshots",
#'   "v11.2",
#'   "dextromethorphan_aggregated_simulations_v11.1.json"
#' )
#' pkSimSnapshot <- jsonlite::read_json(path = snapshotFile, simplifyVector = FALSE)
#' # Create scenarios from the snapshot
#' scenarioConfigurations <- createScenariosFromSnapshots(snapshot = pkSimSnapshot)
#' # Optionally - write the new scenarios to an excel file
#' addScenariosToExcel(scenarioConfigurations, projectConfiguration)
createScenarioConfigurationsFromSnapshot <- function(snapshot, projectConfiguration) {
  snapshotSimulations <- snapshot$Simulations
  # Create a scenario configuration for each simulation
  scenarioConfigurations <- vector("list", length(snapshotSimulations))
  scenarioNames <- vector("character", length(snapshotSimulations))
  # For each simulation, create a scenario
  for (idx in seq_along(snapshotSimulations)) {
    simulation <- snapshotSimulations[[idx]]
    # Create a new scenario configuration
    scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)
    # Scenario name
    scenarioConfiguration$scenarioName <- simulation$Name
    # Simulation time
    outputSchema <- .createOutputSchemaStringFromJson(simulation$OutputSchema)
    scenarioConfiguration$simulationTime <- outputSchema$Intervals
    scenarioConfiguration$simulationTimeUnit <- outputSchema$Unit
    # Output paths
    scenarioConfiguration$outputPaths <- unlist(simulation$OutputSelections, use.names = FALSE)
    # Model file
    scenarioConfiguration$modelFile <- paste0(simulation$Name, ".pkml")

    # Add the new ScenarioConfiguration to the output list
    scenarioConfigurations[[idx]] <- scenarioConfiguration
    scenarioNames[[idx]] <- simulation$Name
  }
  names(scenarioConfigurations) <- scenarioNames

  return(scenarioConfigurations)
}

#' Create output simulation time string form output schema in json format
#'
#' @param outputSchemaJson The 'OutputSchema' section from a Simulation of a json
#' PK-Sim snapshot
#'
#' @returns
#' A named list, with 'Intervals' a string where each output interval is defined as
#' <start, end, resolution>, and intervals are separated by a ';',
#' and 'Unit' the unit of the start time of the first interval.
#' All values are transformed to 'Unit'.
#' @noRd
.createOutputSchemaStringFromJson <- function(outputSchemaJson) {
  outputIntervals <- list()
  # All values will have the unit of the very first "Start time" parameter
  schemaUnit <- NULL
  # Iterate through all output intervals defined
  for (outputInterval in outputSchemaJson) {
    # Each output interval is defined by the parameters "Start time", "End time",
    # and "Resolution". Store the values and the units of the parameters separately.
    paramValues <- list()
    paramUnits <- list()

    for (param in outputInterval$Parameters) {
      # The unit of the very first parameter "Start time" will be the unit of
      # all values
      if (param$Name == "Start time") {
        schemaUnit <- schemaUnit %||% param$Unit
      }
      paramValues[[param$Name]] <- param$Value
      paramUnits[[param$Name]] <- param$Unit
    }
    # Combine parameter values to a string. All values are converted to the
    # unit of the very first parameter "Start time".
    intervalString <- paste(
      ospsuite::toUnit(ospsuite::ospDimensions$Time,
        paramValues[["Start time"]],
        targetUnit = schemaUnit,
        sourceUnit = paramUnits[["Start time"]]
      ),
      ospsuite::toUnit(ospsuite::ospDimensions$Time,
        paramValues[["End time"]],
        targetUnit = schemaUnit,
        sourceUnit = paramUnits[["End time"]]
      ),
      ospsuite::toUnit(ospsuite::ospDimensions$Resolution,
        paramValues[["Resolution"]],
        targetUnit = paste0("pts/", schemaUnit),
        sourceUnit = paramUnits[["Resolution"]]
      ),
      sep = ", "
    )
    outputIntervals <- c(outputIntervals, intervalString)
  }
  return(list(
    "Intervals" = paste(outputIntervals, collapse = "; "),
    "Unit" = schemaUnit
  ))
}

#' Adds entries for plots defined for all simulations of a PK-Sim snapshot to the Plots.xlsx file.
#'
#' @description
#' Extracts simulation plot definition from an PK-Sim project snapshot and converts
#' it to the esqlabsR plot excel file structure. The plots are added to the end of the
#' excel sheets, any of the duplicates (e.g., DataCombined with the same name) are
#' NOT overwritten.
#'
#' Currently, only time-values profiles are extracted.
#'
#'
#' @param snapshot PK-Sim snapshot as read by `rjson::fromJSON`
#' @param outputPath Path of the excel file to be created.
#' @export
#'
#' @examples
#' \dontrun{
#' snapshotFile <- file.path("..", "dextromethorphan_aggregated_simulations_v11.1.json")
#' pkSimSnapshot <- rjson::fromJSON(file = snapshotFile)
#' createPlotExcelFromSnapshot(
#'   snapshot = pkSimSnapshot,
#'   outputPath = "../Plots.xlsx"
#' )
#' }
addPlotsToExcelFromSnapshot <- function(snapshot, outputPath) {
  snapshotSimulations <- snapshot$Simulations
  dataCombinedDf <- data.frame(list(
    "DataCombinedName" = character(),
    "dataType" = character(),
    "label" = character(),
    "scenario" = character(),
    "path" = character(),
    "dataSet" = character(),
    "group" = character(),
    "xOffsets" = numeric(),
    "xOffsetsUnits" = character(),
    "yOffsets" = numeric(),
    "yOffsetsUnits" = character(),
    "xScaleFactors" = numeric(),
    "yScaleFactors" = numeric()
  ))
  plotConfigurationDf <- data.frame(list(
    "plotID" = character(),
    "DataCombinedName" = character(),
    "plotType" = character(),
    "title" = character(),
    "xUnit" = character(),
    "yUnit" = character(),
    "xAxisScale" = character(),
    "yAxisScale" = character(),
    "xAxisLimits" = character(),
    "yAxisLimits" = character(),
    "quantiles" = character(),
    "foldDistance" = character()
  ))
  plotGridsDf <- data.frame(list(
    "name" = character(),
    "plotIDs" = character(),
    "title" = character()
  ))

  for (simulation in snapshotSimulations) {
    # For each simulation, create a plotGrid
    simulationName <- simulation$Name
    plotIDs <- c()

    # Create map of groupings.
    # For one observed data, only one mapping of simulation output can exist
    outputMappings <- list()
    for (outputMapping in simulation$OutputMappings) {
      obsData <- outputMapping$ObservedData
      simPath <- .getModelOutputPathFromSnapshot(outputMapping$Path)

      # The group for both observed data and simulated result should be the name
      # of observed data
      outputMappings[[obsData]] <- obsData
      outputMappings[[simPath]] <- obsData
    }

    for (individualAnalysis in simulation$IndividualAnalyses) {
      # For each individual analysis, create a plot, consisting of a DataCombined
      # and a plotGrid
      plotID <- paste0(simulationName, " - ", individualAnalysis$Name)
      plotTitle <- individualAnalysis$Title
      # Replace NULL by NA
      plotTitle <- plotTitle %||% NA

      # Axis specs
      xUnit <- individualAnalysis$Axes[[1]]$Unit
      yUnit <- individualAnalysis$Axes[[2]]$Unit
      if (individualAnalysis$Axes[[1]]$Scaling == "Log") {
        xAxisScale <- "log"
      } else {
        xAxisScale <- "lin"
      }
      if (individualAnalysis$Axes[[2]]$Scaling == "Log") {
        yAxisScale <- "log"
      } else {
        yAxisScale <- "lin"
      }

      # Update grouping names, as for each individual analysis, the same data set
      # can have different names
      outputMappingsForAnalysis <- .updateOutputMappingGroupNamesFromSnapshot(individualAnalysis = individualAnalysis, outputMappings = outputMappings)
      for (curve in individualAnalysis$Curves) {
        # Skip the curve if it is not visible
        if (!is.null(curve$CurveOptions$Visible) && !curve$CurveOptions$Visible) {
          next
        }

        dataCombinedRow <- .getDCRowFromSnapshotCurve(plotID, simulationName, curve, outputMappingsForAnalysis)
        dataCombinedDf <- rbind(dataCombinedDf, dataCombinedRow)
      }
      plotConfigurationRow <-
        list(
          "plotID" = plotID,
          "DataCombinedName" = plotID,
          "plotType" = "individual",
          "title" = plotTitle,
          "xUnit" = xUnit,
          "yUnit" = yUnit,
          "xAxisScale" = xAxisScale,
          "yAxisScale" = yAxisScale,
          "xAxisLimits" = NA,
          "yAxisLimits" = NA,
          "quantiles" = NA,
          "foldDistance" = NA
        )
      plotConfigurationDf <- rbind(plotConfigurationDf, plotConfigurationRow)

      # Enclosing plotID in parenthesis, so it gets properly parsed
      plotIDs <- c(plotIDs, paste0('"', plotID, '"'))
    }
    plotGridRow <- list(
      "name" = simulationName,
      "plotIDs" = paste0(plotIDs, collapse = ","),
      "title" = paste0(simulationName, " - time profile")
    )
    plotGridsDf <- rbind(plotGridsDf, plotGridRow)
  }

  .writeExcel(
    data = list(
      "DataCombined" = dataCombinedDf,
      "plotConfiguration" = plotConfigurationDf,
      "plotGrids" = plotGridsDf
    ),
    path = outputPath
  )
}

#' Guess the type of the curve - simulated or observed
#'
#' @param curve
#'
#' @returns "simulated" or "observed"
#' @noRd
.getSnapshotCurveDataType <- function(curve) {
  # Assumption - the X column from simulations are always called 'Time'. For
  # observed data, the name of the column includes the name of the data
  if (curve$X == "Time") {
    return("simulated")
  }
  return("observed")
}

# Create a DataCombined row for the curve
#'
#' @param plotID Plot ID that will be used as DataCombined name
#' @param simulationName Name of the simulation used in the plot
#' @param curve The `curve` object from the snapshot. Used for getting the label
#' @param outputMappings A named list of output mappings
#'
#' @returns a data frame row
#' @noRd
.getDCRowFromSnapshotCurve <- function(plotID, simulationName, curve, outputMappings) {
  dataType <- .getSnapshotCurveDataType(curve)
  group <- NA

  if (dataType == "simulated") {
    # We assume that simulated scenario name is equal to the name of the simulation
    scenario <- simulationName
    # Replace '/' by '_' as this is what happens to pkml files during export.
    scenario <- gsub(pattern = "/", "_", scenario, fixed = TRUE)
    # Path is stored in the name of the Y-variable
    path <- curve$Y
    # However, it contains simulation name as the first path element, which must
    # be removed
    path <- .getModelOutputPathFromSnapshot(path)
    dataSet <- NA
    if (!is.null(outputMappings[[path]])) {
      group <- outputMappings[[path]]
    }
  }
  # Observed data
  else {
    scenario <- NA
    path <- NA

    # name of data set is stored in the name of the X-variable
    dataSet <- curve$X
    # However, only the first part is the name of the data set
    dataSet <- .getObsDataNameFromSnapshot(dataSet)
    if (!is.null(outputMappings[[dataSet]])) {
      group <- outputMappings[[dataSet]]
    }
  }
  dataCombinedRow <- list(
    "DataCombinedName" = plotID,
    "dataType" = dataType,
    "label" = curve$Name,
    "scenario" = scenario,
    "path" = path,
    "dataSet" = dataSet,
    "group" = group,
    "xOffsets" = NA,
    "xOffsetsUnits" = NA,
    "yOffsets" = NA,
    "yOffsetsUnits" = NA,
    "xScaleFactors" = NA,
    "yScaleFactors" = NA
  )

  return(dataCombinedRow)
}

# Get the name of observed data from the full name
#'
#' @param fullName Full name of the observed data.
#'
#' @returns `fullName` split by `|` and the first part returned.
#' @noRd
.getObsDataNameFromSnapshot <- function(fullName) {
  strsplit(fullName, "|", fixed = TRUE)[[1]][[1]]
}

# Get model output path from the full name
#'
#' @param fullName Full name of the model output.
#'
#' @returns `fullName` split by `|` without the first part (being the simulation name)
#' @noRd
.getModelOutputPathFromSnapshot <- function(fullName) {
  fullName <- strsplit(fullName, "|", fixed = TRUE)[[1]]
  path <- paste0(fullName[2:length(fullName)], collapse = "|")
  return(path)
}

# Update names of the group for a specific figure
#'
#' @param individualAnalysis The `IndividualAnalysis` structure from the snapshot
#' @param outputMappings A named list of output mappings
#'
#' @returns outputMappings with updated group names
#' @noRd
.updateOutputMappingGroupNamesFromSnapshot <- function(individualAnalysis, outputMappings) {
  for (curve in individualAnalysis$Curves) {
    # Skip the curve if it is not visible
    if (!is.null(curve$CurveOptions$Visible) && !curve$CurveOptions$Visible) {
      next
    }
    # Update the name of the group
    if (.getSnapshotCurveDataType(curve) == "observed") {
      dataName <- .getObsDataNameFromSnapshot(curve$X)
      # Indeces that belong to this group
      idx <- which(outputMappings == dataName, useNames = FALSE)
      outputMappings[idx] <- curve$Name
    }
  }
  return(outputMappings)
}
