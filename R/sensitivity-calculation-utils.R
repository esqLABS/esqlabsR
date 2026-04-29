# validation helpers ----

#' Validate a character vector
#'
#' @param object An object or a vector of type `character`.
#' @param nullAllowed Boolean flag if `NULL` is accepted for the object. If
#'   `TRUE`, `NULL` always returns TRUE, otherwise `NULL` returns FALSE. Default
#'   is `FALSE`.
#'
#' @return `NULL` if the entered object is a valid character vector, otherwise
#' produces an error. Also accepts `NULL` as input if `nullAllowed` is set to
#' `TRUE`.
#'
#' @keywords internal
#' @noRd
.validateCharVector <- function(object, nullAllowed = FALSE) {
  objectName <- deparse(substitute(object))
  objectType <- "character"

  # Handle NULL
  if (is.null(object)) {
    if (nullAllowed) {
      return(invisible(NULL))
    }
    stop(messages$errorWrongType(objectName, class(object)[1], objectType))
  }

  # Check if the argument is of type character
  if (!(is.atomic(object) && typeof(object) == objectType)) {
    stop(messages$errorWrongType(objectName, class(object)[1], objectType))
  }

  # Ensure all values are distinct
  if (!hasOnlyDistinctValues(object)) {
    stop(messages$errorDuplicatedValues())
  }

  # Check for empty string values
  if (any(!nzchar(object))) {
    stop(messages$errorEmptyString(objectName))
  }
}

# plotting helpers ----

#' Filter out data not needed for plotting
#'
#' @param data Internal data frame used while plotting.
#' @inheritParams sensitivitySpiderPlot
#'
#' @keywords internal
#' @noRd
.filterPlottingData <- function(
  data,
  outputPaths = NULL,
  parameterPaths = NULL,
  pkParameters = NULL
) {
  if (!is.null(outputPaths)) {
    data <- dplyr::filter(data, OutputPath %in% outputPaths)
  }

  if (!is.null(parameterPaths)) {
    data <- dplyr::filter(data, ParameterPath %in% parameterPaths)
  }

  if (!is.null(pkParameters)) {
    data <- dplyr::filter(data, PKParameter %in% pkParameters)
  }

  return(data)
}


# save / load SensitivityCalculation ----

#' Save Sensitivity Calculation Results
#'
#' Saves the results of a sensitivity analysis to a specified directory,
#' including metadata and simulation output required for restoring or sharing
#' the analysis.
#'
#' @param sensitivityCalculation A named list of class `SensitivityCalculation`
#'   as returned by [sensitivityCalculation()], containing `simulationResults`,
#'   `outputPaths`, `parameterPaths`, and `pkData`.
#' @param outputDir A character string specifying the path to the directory
#'   where the results should be saved.
#' @param overwrite Logical. If `TRUE`, an existing directory at `outputDir`
#'   will be deleted and replaced. Default is `FALSE`.
#'
#' @return Invisibly returns `NULL`. Results are saved to disk in the specified
#' folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sensitivityCalculation <- sensitivityCalculation(
#'   simulation = mySim,
#'   outputPaths = "Organism|PeripheralVenousBlood|Drug|Plasma",
#'   parameterPaths = c("Drug|Lipophilicity", "Application|Dose")
#' )
#'
#' saveSensitivityCalculation(
#'   sensitivityCalculation,
#'   outputDir = "output/my-sensitivity",
#'   overwrite = TRUE
#' )
#' }
saveSensitivityCalculation <- function(
  sensitivityCalculation,
  outputDir,
  overwrite = FALSE
) {
  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")
  validateIsString(outputDir)
  validateIsLogical(overwrite)

  if (dir.exists(outputDir)) {
    if (!overwrite) {
      stop(messages$errorOutputDirExists(outputDir))
    }

    contents <- list.files(outputDir, all.files = TRUE, no.. = TRUE)
    if (interactive() && length(contents) > 0) {
      if (!usethis::ui_yeah(messages$promptDeleteOutputDir(outputDir))) {
        stop(messages$abortedByUser())
      }
    }

    unlink(outputDir, recursive = TRUE)
  }

  dir.create(outputDir, recursive = TRUE)

  simulationResults <- sensitivityCalculation$simulationResults

  # Store the simulation source path so it can be reloaded later
  simFilePath <- simulationResults[[1]][[1]]$simulation$sourceFile
  sensitivityCalculation$simFilePath <- simFilePath

  # Export each SimulationResults object to CSV
  for (i in seq_along(simulationResults)) {
    for (j in seq_along(simulationResults[[i]])) {
      fileName <- sprintf("simulationResult_%03d_%03d.csv", i, j)
      filePath <- file.path(outputDir, fileName)
      ospsuite::exportResultsToCSV(
        simulationResults[[i]][[j]],
        filePath
      )
      # Clear `SimulationResults` while preserving named list structure
      simulationResults[[i]][[j]] <- list(NULL)
    }
  }

  # save sensitivityCalculation w/o `SimulationResults`
  sensitivityCalculation$simulationResults <- simulationResults
  saveRDS(
    sensitivityCalculation,
    file.path(outputDir, "sensitivityCalculation.meta")
  )

  invisible(NULL)
}

#' Load Sensitivity Calculation Results
#'
#' Restores a previously saved sensitivity calculation from a directory created
#' with [saveSensitivityCalculation()]. If no simulation object is provided, the
#' function attempts to load it from the saved simulation file path.
#'
#' @param outputDir Path to the directory containing the saved sensitivity
#'   calculation files.
#' @param simulation Optional. A `Simulation` object. If not provided, the
#'   function will attempt to load the simulation from the path stored in the
#'   metadata.
#'
#' @return A named list of class `SensitivityCalculation`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load sensitivity analysis result from disk
#' sensitivityCalculation <- loadSensitivityCalculation("output/my-sensitivity")
#' }
loadSensitivityCalculation <- function(outputDir, simulation = NULL) {
  validateIsString(outputDir)
  validateIsOfType(simulation, "Simulation", nullAllowed = TRUE)

  metaPath <- file.path(outputDir, "sensitivityCalculation.meta")
  if (!file.exists(metaPath)) {
    stop(messages$errorSensitivityCalculationNotFound(metaPath))
  }

  # Load sensitivityCalculation structure
  sensitivityCalculation <- readRDS(metaPath)

  # Attempt to load simulation if not provided
  if (is.null(simulation)) {
    simFilePath <- sensitivityCalculation$simFilePath
    simulation <- tryCatch(
      {
        ospsuite::loadSimulation(simFilePath)
      },
      error = function(e) {
        stop(messages$errorFailedToLoadSimulation(simFilePath, e$message))
      }
    )
  }

  # Locate simulation result files
  simResultFiles <- list.files(
    path = outputDir,
    pattern = "^simulationResult_\\d+_\\d+\\.csv$",
    full.names = TRUE
  )

  variationRange <- unique(sensitivityCalculation$pkData$ParameterFactor)
  parameterPaths <- sensitivityCalculation$parameterPaths

  expectedCount <- length(parameterPaths) * length(variationRange)
  if (length(simResultFiles) != expectedCount) {
    stop(messages$errorCorruptSensitivityCalculation(outputDir))
  }

  simulationResults <- sensitivityCalculation$simulationResults

  # Refill the structure by index using naming convention from export
  for (file in simResultFiles) {
    matches <- stringr::str_match(
      basename(file),
      "simulationResult_(\\d+)_(\\d+)\\.csv$"
    )
    i <- as.integer(matches[2])
    j <- as.integer(matches[3])

    simulationResult <- ospsuite::importResultsFromCSV(simulation, file)
    simulationResults[[i]][[j]] <- simulationResult

    if (
      !all(
        sensitivityCalculation$outputPaths %in%
          simulationResult$allQuantityPaths
      )
    ) {
      stop(messages$errorCorruptSensitivityCalculation(outputDir))
    }
  }

  sensitivityCalculation$simFilePath <- NULL
  sensitivityCalculation$simulationResults <- simulationResults

  return(sensitivityCalculation)
}

#' Split parameter path for plot labels
#'
#' Used by sensitivity tornado and time-profile plots to wrap long parameter
#' paths onto two lines for readability.
#'
#' @keywords internal
#' @noRd
.splitParameterName <- function(x, equalLines = FALSE) {
  xBreak <- x
  if (!is.null(x)) {
    n <- stringr::str_count(x, stringr::fixed("|"))
    if (isTRUE(n >= 3)) {
      xBreak <- sub("((?:[^|]*\\|){2}[^|]*)\\|", "\\1|\n", x)
    } else if (equalLines) {
      xBreak <- paste0(xBreak, "\n")
    }
  }

  return(xBreak)
}
