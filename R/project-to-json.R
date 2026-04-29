# Project → JSON serialization ----

#' Convert a Project object to a JSON-serializable list
#' @param project A `Project` object.
#' @returns A list matching the Project.json schema.
#' @keywords internal
#' @noRd
.projectToJson <- function(project) {
  list(
    schemaVersion = project$schemaVersion %||% "2.0",
    # Preserve the version stamp from the loaded file; only fall back to the
    # current package version when the project was constructed in-memory and
    # has no recorded version. This keeps round-trip save/load stable.
    esqlabsRVersion = project$esqlabsRVersion %||%
      as.character(utils::packageVersion("esqlabsR")),
    filePaths = .filePathsToJson(project),
    observedData = project$observedData %||% list(),
    outputPaths = as.list(project$outputPaths) %||% list(),
    scenarios = .scenariosToJson(project$scenarios, project$outputPaths),
    modelParameters = .parameterGroupsToJson(project$modelParameters),
    individuals = .individualsToJson(project$individuals),
    populations = .populationsToJson(project$populations),
    applications = .applicationsToJson(project$applications),
    plots = .plotsToJson(project$plots)
  )
}

#' @keywords internal
#' @noRd
.filePathsToJson <- function(project) {
  list(
    modelFolder = .relativePathOrNull(project, "modelFolder"),
    configurationsFolder = .relativePathOrNull(project, "configurationsFolder"),
    modelParamsFile = .relativeFilename(project, "modelParamsFile"),
    individualsFile = .relativeFilename(project, "individualsFile"),
    populationsFile = .relativeFilename(project, "populationsFile"),
    populationsFolder = .relativeFilename(project, "populationsFolder"),
    scenariosFile = .relativeFilename(project, "scenariosFile"),
    applicationsFile = .relativeFilename(project, "applicationsFile"),
    plotsFile = .relativeFilename(project, "plotsFile"),
    dataFolder = .relativePathOrNull(project, "dataFolder"),
    outputFolder = .relativePathOrNull(project, "outputFolder")
  )
}

#' @keywords internal
#' @noRd
.relativePathOrNull <- function(project, fieldName) {
  absPath <- suppressWarnings(project[[fieldName]])
  if (is.null(absPath)) {
    return(NULL)
  }
  if (is.null(project$projectDirPath)) {
    return(absPath)
  }
  fs::path_rel(absPath, project$projectDirPath)
}

#' @keywords internal
#' @noRd
.relativeFilename <- function(project, fieldName) {
  absPath <- suppressWarnings(project[[fieldName]])
  if (is.null(absPath)) {
    return(NULL)
  }
  basename(absPath)
}

#' @keywords internal
#' @noRd
.scenariosToJson <- function(scenarios, outputPaths) {
  if (is.null(scenarios) || length(scenarios) == 0) {
    return(list())
  }

  lapply(scenarios, function(sc) {
    outputPathIds <- NULL
    if (
      !is.null(sc$outputPaths) &&
        length(sc$outputPaths) > 0 &&
        !is.null(outputPaths)
    ) {
      outputPathIds <- names(outputPaths)[match(sc$outputPaths, outputPaths)]
      outputPathIds <- outputPathIds[!is.na(outputPathIds)]
      if (length(outputPathIds) == 0) outputPathIds <- NULL
    }

    simTimeStr <- NULL
    if (!is.null(sc$simulationTime)) {
      intervals <- vapply(
        sc$simulationTime,
        function(int) {
          paste(int, collapse = ", ")
        },
        character(1)
      )
      simTimeStr <- paste(intervals, collapse = "; ")
    }

    list(
      name = sc$scenarioName,
      individualId = sc$individualId,
      populationId = if (sc$simulationType == "Population") {
        sc$populationId
      } else {
        NULL
      },
      readPopulationFromCSV = sc$readPopulationFromCSV,
      modelParameters = as.list(sc$modelParameters),
      applicationProtocol = if (
        is.null(sc$applicationProtocol) || is.na(sc$applicationProtocol)
      ) {
        NULL
      } else {
        sc$applicationProtocol
      },
      simulationTime = simTimeStr,
      simulationTimeUnit = sc$simulationTimeUnit,
      steadyState = sc$simulateSteadyState,
      steadyStateTime = if (
        sc$simulateSteadyState && !is.null(sc$steadyStateTime)
      ) {
        ospsuite::toUnit(
          quantityOrDimension = ospsuite::ospDimensions$Time,
          values = sc$steadyStateTime,
          targetUnit = sc$steadyStateTimeUnit %||% "min"
        )
      } else {
        NULL
      },
      steadyStateTimeUnit = if (sc$simulateSteadyState) {
        sc$steadyStateTimeUnit
      } else {
        NULL
      },
      overwriteFormulasInSS = sc$overwriteFormulasInSS,
      modelFile = sc$modelFile,
      outputPathIds = if (!is.null(outputPathIds)) {
        as.list(outputPathIds)
      } else {
        NULL
      }
    )
  })
}

#' @keywords internal
#' @noRd
.parameterGroupsToJson <- function(groups) {
  if (is.null(groups) || length(groups) == 0) {
    return(list())
  }

  result <- list()
  for (name in names(groups)) {
    group <- groups[[name]]
    entries <- list()
    for (i in seq_along(group$paths)) {
      split <- .splitParameterPathIntoContainerAndName(group$paths[i])
      entries[[i]] <- list(
        containerPath = split$containerPath,
        parameterName = split$parameterName,
        value = group$values[i],
        units = if (group$units[i] == "") NULL else group$units[i]
      )
    }
    result[[name]] <- entries
  }
  result
}

#' @keywords internal
#' @noRd
.applicationsToJson <- function(applications) {
  if (is.null(applications) || length(applications) == 0) {
    return(structure(list(), names = character(0)))
  }
  result <- list()
  for (id in names(applications)) {
    pset <- applications[[id]]$parameters
    params <- list()
    if (!is.null(pset) && length(pset$paths) > 0) {
      params <- vector("list", length(pset$paths))
      for (j in seq_along(pset$paths)) {
        split <- .splitParameterPathIntoContainerAndName(pset$paths[[j]])
        params[[j]] <- list(
          containerPath = split$containerPath,
          parameterName = split$parameterName,
          value = pset$values[[j]],
          units = pset$units[[j]]
        )
      }
    }
    result[[id]] <- list(parameters = params)
  }
  result
}

#' @keywords internal
#' @noRd
.individualsToJson <- function(individuals, parameterSetMapping = NULL) {
  if (is.null(individuals) || length(individuals) == 0) {
    return(list())
  }
  result <- vector("list", length(individuals))
  for (i in seq_along(individuals)) {
    id <- names(individuals)[[i]]
    indiv <- individuals[[i]]
    entry <- list(individualId = id)
    for (field in c("species", "population", "gender", "proteinOntogenies")) {
      if (!is.null(indiv[[field]])) entry[[field]] <- indiv[[field]]
    }
    for (field in c("weight", "height", "age")) {
      if (!is.null(indiv[[field]]) && !is.na(indiv[[field]])) {
        entry[[field]] <- as.double(indiv[[field]])
      }
    }
    pset <- indiv$parameters
    if (!is.null(pset) && length(pset$paths) > 0) {
      params <- vector("list", length(pset$paths))
      for (j in seq_along(pset$paths)) {
        split <- .splitParameterPathIntoContainerAndName(pset$paths[[j]])
        params[[j]] <- list(
          containerPath = split$containerPath,
          parameterName = split$parameterName,
          value = pset$values[[j]],
          units = pset$units[[j]]
        )
      }
      entry$parameters <- params
    }
    result[[i]] <- entry
  }
  result
}

#' @keywords internal
#' @noRd
.populationsToJson <- function(populations) {
  if (is.null(populations) || length(populations) == 0) {
    return(list())
  }

  lapply(names(populations), function(id) {
    pop <- populations[[id]]
    c(list(populationId = id), pop)
  })
}

#' @keywords internal
#' @noRd
.plotsToJson <- function(plots) {
  if (is.null(plots)) {
    return(list(
      dataCombined = list(),
      plotConfiguration = list(),
      plotGrids = list()
    ))
  }

  list(
    dataCombined = .dataCombinedToNestedJson(plots$dataCombined),
    plotConfiguration = .dataFrameToListOfLists(plots$plotConfiguration),
    plotGrids = .dataFrameToListOfLists(plots$plotGrids)
  )
}

#' @keywords internal
#' @noRd
.dataFrameToListOfLists <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    row <- lapply(row, function(x) if (is.na(x)) NULL else x)
    row
  })
}


#' Convert flat dataCombined data.frame to nested JSON structure
#' @param df data.frame with DataCombinedName, dataType, and entry fields
#' @returns List of dataCombined objects with simulated/observed arrays
#' @keywords internal
#' @noRd
.dataCombinedToNestedJson <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  transformCols <- c(
    "xOffsets",
    "xOffsetsUnits",
    "yOffsets",
    "yOffsetsUnits",
    "xScaleFactors",
    "yScaleFactors"
  )

  dataCombinedNames <- unique(df$DataCombinedName)

  lapply(dataCombinedNames, function(dataCombinedName) {
    dataCombinedRows <- df[
      df$DataCombinedName == dataCombinedName,
      ,
      drop = FALSE
    ]

    simRows <- dataCombinedRows[
      dataCombinedRows$dataType == "simulated",
      ,
      drop = FALSE
    ]
    obsRows <- dataCombinedRows[
      dataCombinedRows$dataType == "observed",
      ,
      drop = FALSE
    ]

    simulated <- lapply(seq_len(nrow(simRows)), function(i) {
      row <- simRows[i, ]
      entry <- list(
        label = row$label,
        scenario = row$scenario,
        path = row$path,
        group = if (is.na(row$group)) NULL else row$group
      )
      for (col in transformCols) {
        entry[[col]] <- if (is.na(row[[col]])) NULL else row[[col]]
      }
      entry
    })

    observed <- lapply(seq_len(nrow(obsRows)), function(i) {
      row <- obsRows[i, ]
      entry <- list(
        label = row$label,
        dataSet = row$dataSet,
        group = if (is.na(row$group)) NULL else row$group
      )
      for (col in transformCols) {
        entry[[col]] <- if (is.na(row[[col]])) NULL else row[[col]]
      }
      entry
    })

    list(
      name = dataCombinedName,
      simulated = simulated,
      observed = observed
    )
  })
}
