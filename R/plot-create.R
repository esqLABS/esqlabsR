# Plot generation ----

#' Generate plots from a Project
#'
#' @param project Object of class `Project` that
#'   contains information about the output paths and plots configuration.
#' @param simulatedScenarios A list of simulated scenarios as returned by
#'   `runScenarios()`. Can be `NULL` if no simulated data is required for the
#'   plots.
#' @param stopIfNotFound If TRUE (default), the function stops if any of the
#'   simulated results or observed data are not found. If FALSE a warning is
#'   printed.
#'
#' @param plotGridNames Names of the plot grids for which the figures will be
#'   created. If `NULL` (default), all plot grids will be created. If a plot
#'   grid with a given name does not exist, an error is thrown.
#'
#' @param outputFolder Optional - path to the folder where the results will be
#'   stored. If `NULL` (default), `project$outputFolder` is used.
#'   Only relevant for plots specified for export in the export configuration.
#' @param dataCombinedList A (named) list of `DataCombined` objects as input to
#'   create plots defined in the `plotGridNames` argument. Missing
#'   `DataCombined` will be created from the project configuration (default
#'   behavior). Defaults to `NULL`, in which case all `DataCombined` are
#'   created from the project configuration.
#'
#' @returns A list of `ggplot` objects
#'
#' @import tidyr
#'
#' @export
createPlots <- function(
  project,
  plotGridNames = NULL,
  simulatedScenarios = NULL,
  dataCombinedList = NULL,
  outputFolder = NULL,
  stopIfNotFound = TRUE
) {
  validateIsOfType(project, "Project")
  validateIsString(plotGridNames, nullAllowed = TRUE)
  validateIsOfType(dataCombinedList, "DataCombined", nullAllowed = TRUE)
  if (!typeof(dataCombinedList) %in% c("list", "NULL")) {
    stop(messages$errorDataCombinedListMustBeList(typeof(dataCombinedList)))
  }
  plotConfigurations <- .getPlotConfigurations(
    project = project,
    plotGridNames = plotGridNames
  )
  dfPlotConfigurations <- plotConfigurations$plotConfigurations
  dfPlotGrids <- plotConfigurations$plotGrids
  dfExportConfigurations <- plotConfigurations$exportConfigurations

  # Exit early if no plotGrids are defined
  if (is.null(dfPlotGrids)) {
    return(NULL)
  }

  # Get the names of data combined that are required for creation of the plots
  dataCombinedNames <- unique(dfPlotConfigurations$DataCombinedName)
  # Do not create DataCombined that are already passed
  if (!is.null(dataCombinedList)) {
    dataCombinedNames <- setdiff(dataCombinedNames, names(dataCombinedList))
  }
  # Filter and validate only used data combined
  dataCombinedListFromConfig <- createDataCombined(
    project = project,
    dataCombinedNames = dataCombinedNames,
    simulatedScenarios = simulatedScenarios,
    stopIfNotFound = stopIfNotFound
  )
  # Add entries from the provided list of DataCombined.
  dataCombinedListFromConfig[names(dataCombinedList)] <- dataCombinedList
  dataCombinedList <- dataCombinedListFromConfig

  dfPlotConfigurations <- .validatePlotConfiguration(
    dfPlotConfigurations,
    names(dataCombinedList)
  )

  # create a list of plotConfiguration objects as defined in sheet "plotConfiguration"
  defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfigurationList <- apply(dfPlotConfigurations, 1, \(row) {
    plotConfiguration <- .createConfigurationFromRow(
      defaultConfiguration = defaultPlotConfiguration,
      # Have to exclude all columns that should not be vectorized
      # Excluding title and subtitle because they should not be processed,
      # e.g., split by ","
      row[
        !(names(row) %in%
          c(
            "plotID",
            "DataCombinedName",
            "plotType",
            "title",
            "subtitle",
            "xLabel",
            "yLabel",
            "aggregation",
            "quantiles",
            "nsd",
            "foldDistance"
          ))
      ]
    )
    # Apply title and subtitle properties
    if (!is.na(row[["title"]])) {
      plotConfiguration$title <- row[["title"]]
    }

    if ("subtitle" %in% names(row) && !is.na(row[["subtitle"]])) {
      plotConfiguration$subtitle <- row[["subtitle"]]
    }

    # Check for log scale with zero in axis limits
    .validateLogScaleAxisLimits(plotConfiguration, row[["plotID"]])

    return(plotConfiguration)
  })
  names(plotConfigurationList) <- dfPlotConfigurations$plotID

  # create a list of plots from dataCombinedList and plotConfigurationList
  plotList <- lapply(dfPlotConfigurations$plotID, \(plotId) {
    dataCombined <- dataCombinedList[[
      dfPlotConfigurations[
        dfPlotConfigurations$plotID == plotId,
      ]$DataCombinedName
    ]]
    switch(
      dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$plotType,
      # Individual time profile
      individual = plotIndividualTimeProfile(
        dataCombined,
        plotConfigurationList[[plotId]]
      ),
      # Population time profile
      population = {
        aggregation <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$aggregation
        quantiles <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$quantiles
        nsd <- dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$nsd
        args <- list()
        args$dataCombined <- dataCombined
        args$defaultPlotConfiguration <- plotConfigurationList[[plotId]]
        # Is aggregation defined?
        if (!is.null(aggregation) && !is.na(aggregation)) {
          args$aggregation <- aggregation
        }
        # quantiles defined?
        if (!is.null(quantiles) && !is.na(quantiles)) {
          args$quantiles <- as.numeric(unlist(strsplit(quantiles, split = ",")))
        }
        # if nsd is defined, add it to the args
        if (!is.null(nsd) && !is.na(nsd)) {
          args$nsd <- as.numeric(nsd)
        }
        do.call(plotPopulationTimeProfile, args)
      },
      observedVsSimulated = {
        foldDist <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$foldDistance
        if (is.na(foldDist)) {
          plotObservedVsSimulated(dataCombined, plotConfigurationList[[plotId]])
        } else {
          plotObservedVsSimulated(
            dataCombined,
            plotConfigurationList[[plotId]],
            foldDistance = as.numeric(unlist(strsplit(foldDist, split = ",")))
          )
        }
      },
      residualsVsSimulated = plotResidualsVsSimulated(
        dataCombined,
        plotConfigurationList[[plotId]]
      ),
      residualsVsTime = plotResidualsVsTime(
        dataCombined,
        plotConfigurationList[[plotId]]
      )
    )
  })
  names(plotList) <- dfPlotConfigurations$plotID

  # create plotGridConfiguration objects and add plots from plotList
  defaultPlotGridConfig <- createEsqlabsPlotGridConfiguration()
  plotGrids <- apply(dfPlotGrids, 1, \(row) {
    plotGridConfiguration <- .createConfigurationFromRow(
      defaultConfiguration = defaultPlotGridConfig,
      row[!(names(row) %in% c("name", "plotIDs", "title"))]
    )

    # Ignore if title is not defined or no 'title' column is present
    if (!is.na(row$title) && !is.null(row$title)) {
      plotGridConfiguration$title <- row$title
    }

    plotsToAdd <- plotList[intersect(
      unlist(row$plotIDs),
      dfPlotConfigurations$plotID
    )]
    # Have to remove NULL instances. NULL can be produced e.g. when trying to create
    # a simulated vs observed plot without any groups
    plotsToAdd <- plotsToAdd[lengths(plotsToAdd) != 0]
    # Cannot create a plot grid if no plots are added. Skip
    if (length(plotsToAdd) == 0) {
      return(NULL)
    }
    # When only one plot is in the grid, do not show panel labels
    if (length(plotsToAdd) == 1) {
      plotGridConfiguration$tagLevels <- NULL
    }
    plotGridConfiguration$addPlots(plots = plotsToAdd)
    if (
      length(
        invalidPlotIDs <- setdiff(
          unlist(row$plotIDs),
          dfPlotConfigurations$plotID
        )
      ) !=
        0
    ) {
      warning(messages$warningInvalidPlotID(invalidPlotIDs, row$title))
    }
    plotGrid(plotGridConfiguration)
  })
  names(plotGrids) <- dfPlotGrids$name

  ## Remove rows that are entirely empty
  dfExportConfigurations <- dplyr::filter(
    dfExportConfigurations,
    !dplyr::if_all(dplyr::everything(), is.na)
  )
  dfExportConfigurations <- .validateExportConfigurations(
    dfExportConfigurations,
    plotGrids
  )
  if (nrow(dfExportConfigurations) > 0) {
    # create a list of ExportConfiguration objects from dfExportConfigurations
    outputFolder <- outputFolder %||%
      file.path(
        project$outputFolder,
        "Figures",
        format(Sys.time(), "%F %H-%M")
      )

    defaultExportConfiguration <- createEsqlabsExportConfiguration(outputFolder)
    exportConfigurations <- apply(dfExportConfigurations, 1, \(row) {
      exportConfiguration <- .createConfigurationFromRow(
        defaultConfiguration = defaultExportConfiguration,
        row[!(names(row) %in% c("plotGridName", "name"))]
      )
      # Replace "\" and "/" by "_" so the file name does not result in folders
      name <- row[["name"]]
      name <- gsub(pattern = "\\", "_", name, fixed = TRUE)
      name <- gsub(pattern = "/", "_", name, fixed = TRUE)
      exportConfiguration$name <- name
      return(exportConfiguration)
    })
    # export plotGrid if defined in exportConfigurations
    lapply(seq_along(exportConfigurations), function(i) {
      exportConfigurations[[
        i
      ]]$savePlot(plotGrids[[dfExportConfigurations$plotGridName[i]]])
    })
  }

  return(plotGrids)
}


#' @rdname createPlots
#' @export
createPlotsFromExcel <- function(...) {
  lifecycle::deprecate_soft(
    what = "createPlotsFromExcel()",
    with = "createPlots()",
    when = "6.0.0"
  )
  createPlots(...)
}
