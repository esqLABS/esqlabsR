# Color palette ----

#' esqLABS color palette
#'
#' Returns the list of colors extrapolated between the esqLABS colors blue, red,
#' and green.
#'
#' For `nrOfColors` == 1, the esqLABS-blue is returned For `nrOfColors` == 2,
#' the esqLABS-blue and green are returned For `nrOfColors` == 3, the
#' esqLABS-blue, red, and green are returned For `nrOfColors` > 3, the three
#' esqLABS colors are fixed, and the remaining colors are extrapolated from blue
#' to red to green. If `nrOfColors` is uneven, the blue-to-red section becomes
#' one color more than the red-to-green section. In this implementation,
#' blue-to-green is not considered.
#'
#' @param nrOfColors Positive integer defining the number of colors to be
#'   generated.
#'
#' @import grDevices
#' @returns A list of colors as HEX values.
#' @import grDevices
#' @export
esqlabsColors <- function(nrOfColors) {
  # esqLABS colors in HSV model
  esqRedHSV <- grDevices::rgb2hsv(234, 94, 94, maxColorValue = 255)
  esqBlueHSV <- grDevices::rgb2hsv(74, 189, 203, maxColorValue = 255)
  esqGreenHSV <- grDevices::rgb2hsv(118, 187, 96, maxColorValue = 255)
  # default color palette.
  esq_palette <- c(
    hsv(esqBlueHSV[1], esqBlueHSV[2], esqBlueHSV[3]),
    hsv(esqRedHSV[1], esqRedHSV[2], esqRedHSV[3]),
    hsv(esqGreenHSV[1], esqGreenHSV[2], esqGreenHSV[3])
  )

  # pre-calculate distances between blue and red and red and green.
  deltaH_b_r <- (esqRedHSV[1] - esqBlueHSV[1])
  deltaS_b_r <- max(esqRedHSV[2], esqBlueHSV[2]) -
    min(esqRedHSV[2], esqBlueHSV[2])
  deltaV_b_r <- max(esqRedHSV[3], esqBlueHSV[3]) -
    min(esqRedHSV[3], esqBlueHSV[3])

  deltaH_r_g <- abs(esqRedHSV[1] - (esqGreenHSV[1] + 1))
  deltaS_r_g <- max(esqRedHSV[2], esqGreenHSV[2]) -
    min(esqRedHSV[2], esqGreenHSV[2])
  deltaV_r_g <- max(esqRedHSV[3], esqGreenHSV[3]) -
    min(esqRedHSV[3], esqGreenHSV[3])

  if (nrOfColors < 0) {
    stop(messages$nrOfColorsShouldBePositive(nrOfColors))
  }
  if (nrOfColors == 0) {
    return(c())
  }
  if (nrOfColors == 2) {
    palette <- c(esq_palette[1], esq_palette[3])
    return(palette)
  }
  if (nrOfColors <= 3) {
    palette <- esq_palette[1:nrOfColors]
    return(palette)
  }

  nrOfColorsToGenerate <- nrOfColors - 3

  palette <- esq_palette[1]
  nrOfColors_first <- nrOfColorsToGenerate %/% 2 + nrOfColorsToGenerate %% 2
  nrOfColors_second <- nrOfColorsToGenerate %/% 2
  # calculate the first half - blue to red.
  # Index starts with 1 as it defines the number of colors.
  for (i in 1:nrOfColors_first) {
    deltaH <- deltaH_b_r / (nrOfColors_first + 1)
    deltaS <- deltaS_b_r / (nrOfColors_first + 1)
    deltaV <- deltaV_b_r / (nrOfColors_first + 1)

    h <- esqBlueHSV[1] + deltaH * i
    if (h > 1) {
      h <- h - 1
    }
    s <- min(esqBlueHSV[2], esqRedHSV[2]) + deltaS * i
    v <- min(esqBlueHSV[3], esqRedHSV[3]) + deltaV * i

    palette <- c(palette, hsv(h, s, v))
  }

  palette <- c(palette, esq_palette[2])
  # calculate the second half - red to green.
  # Index starts with 1 as it defines the number of colors.
  if (nrOfColors_second > 0) {
    for (i in 1:nrOfColors_second) {
      deltaH <- deltaH_r_g / (nrOfColors_second + 1)
      deltaS <- deltaS_r_g / (nrOfColors_second + 1)
      deltaV <- deltaV_r_g / (nrOfColors_second + 1)

      h <- esqRedHSV[1] + deltaH * i
      if (h > 1) {
        h <- h - 1
      }
      s <- min(esqGreenHSV[2], esqRedHSV[2]) + deltaS * i
      v <- min(esqGreenHSV[3], esqRedHSV[3]) + deltaV * i

      palette <- c(palette, hsv(h, s, v))
    }
  }
  palette <- c(palette, esq_palette[3])

  return(palette) # nolint: return_linter.
}

#' Returns the HSV values for a given R color name
#'
#' @param color vector of any of the three kinds of R color specifications,
#'   i.e., either a color name (as listed by colors()), a hexadecimal string of
#'   the form "#rrggbb" or "#rrggbbaa" (see rgb), or a positive integer `i`
#'   meaning `palette()[i]`.
#'
#' @returns A matrix with a column for each color. The three rows of the matrix
#'   indicate hue, saturation and value and are named "h", "s", and "v"
#'   accordingly.
#' @export
#' @import ospsuite ospsuite.utils grDevices
#'
#' @examples
#' col2hsv("yellow")
#' @export
col2hsv <- function(color) {
  validateIsString(color)
  rgb <- col2rgb(color)
  return(grDevices::rgb2hsv(rgb))
}
# Plot configuration constructors ----

#' @title Create an instance of `DefaultPlotConfiguration` R6 class
#' @rdname createEsqlabsPlotConfiguration
#'
#' @description
#'
#' An instance of `DefaultPlotConfiguration` R6 class from `{tlf}` package is
#' needed for creating visualizations with the `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @returns An instance of `DefaultPlotConfiguration` R6 class.
#'
#' @examples
#' createEsqlabsPlotConfiguration()
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsPlotConfiguration <- function() {
  defaultPlotConfiguration <- ospsuite::DefaultPlotConfiguration$new()

  # Size
  defaultPlotConfiguration$titleSize <- 10
  defaultPlotConfiguration$xLabelSize <- 9
  defaultPlotConfiguration$yLabelSize <- 9
  defaultPlotConfiguration$xAxisLabelTicksSize <- 8
  defaultPlotConfiguration$yAxisLabelTicksSize <- 8
  defaultPlotConfiguration$legendKeysSize <- 6

  defaultPlotConfiguration$xLabelMargin <- c(10, 0, 0, 0)
  defaultPlotConfiguration$yLabelMargin <- c(0, 0, 10, 0)

  # Lines size
  defaultPlotConfiguration$linesSize <- 0.5

  # Points size
  defaultPlotConfiguration$pointsSize <- 1.75

  # Error bars size
  defaultPlotConfiguration$errorbarsSize <- 0.65
  defaultPlotConfiguration$errorbarsCapSize <- 2.75

  # Legend appearance
  # defaultPlotConfiguration$legendBorderColor <- "grey10"
  # defaultPlotConfiguration$legendBorderType <- 1
  defaultPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideTopLeft

  # Axis appearance
  defaultPlotConfiguration$yAxisLabelTicksAngle <- 0

  # Colors
  defaultPlotConfiguration$pointsColor <- esqlabsEnv$colorPalette
  defaultPlotConfiguration$ribbonsFill <- esqlabsEnv$colorPalette
  defaultPlotConfiguration$linesColor <- esqlabsEnv$colorPalette

  return(defaultPlotConfiguration)
}

#' @title Create an instance of `PlotGridConfiguration` R6 class
#' @rdname createEsqlabsPlotGridConfiguration
#'
#' @description
#'
#' An instance of `PlotGridConfiguration` R6 class from `{tlf}` package is
#' needed for creating a grid of multiple visualizations created using the
#' `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @returns An instance of `PlotGridConfiguration` R6 class.
#'
#' @examples
#' createEsqlabsPlotGridConfiguration()
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsPlotGridConfiguration <- function() {
  # nolint: object_length_linter.
  plotGridConfiguration <- tlf::PlotGridConfiguration$new()

  plotGridConfiguration$tagLevels <- "a"
  plotGridConfiguration$tagSize <- 11
  plotGridConfiguration$titleSize <- 12

  plotGridConfiguration$titleHorizontalJustification <- 0.5

  return(plotGridConfiguration)
}

#' @param outputFolder Path to the folder where the results will be stored.
#'
#' @title Create an instance of `ExportConfiguration` R6 class
#' @rdname createEsqlabsExportConfiguration
#'
#' @description
#'
#' An instance of `ExportConfiguration` R6 class from `{tlf}` package is needed
#' for saving the plots and plot grids created using the `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @returns An instance of `ExportConfiguration` R6 class.
#'
#' @examples
#' myProjConfig <- Project$new()
#' createEsqlabsExportConfiguration(myProjConfig$outputFolder)
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsExportConfiguration <- function(outputFolder) {
  # nolint: object_length_linter.
  # Specifying the namespace because we want to use the ExportConfiguration
  # from esqlabsR and not from TLF
  exportConfiguration <- esqlabsR::ExportConfiguration$new()

  exportConfiguration$path <- outputFolder
  exportConfiguration$dpi <- 300
  # NULL is not supported by ExportConfiguration, so we should assign here
  # something useful. NULL in the Project currently means "do not
  # export".
  exportConfiguration$format <- "png"
  exportConfiguration$width <- 18
  exportConfiguration$heightPerRow <- 12
  exportConfiguration$units <- "cm"
  return(exportConfiguration)
}

# Plot configuration override and overlay helpers ----

#' Update Plot Configuration with Overrides
#'
#' Updates a plot configuration object `plotConfiguration` with explicitly
#' defined overrides from `plotOverrideConfig` list. It retains any custom
#' settings in `plotConfiguration` that deviate from the defaults
#'
#' @param plotConfiguration A plot configuration object.
#' @param plotOverrideConfig A list with new configuration settings to apply.
#'
#' @keywords internal
#' @noRd
.updatePlotConfiguration <- function(plotConfiguration, plotOverrideConfig) {
  defaultValues <- createEsqlabsPlotConfiguration()

  for (name in names(plotOverrideConfig)) {
    if (!name %in% names(plotConfiguration)) {
      warning(messages$UnknownPlotConfiguration(name))
      next
    }

    if (is.null(defaultValues[[name]]) && is.null(plotConfiguration[[name]])) {
      plotConfiguration[[name]] <- plotOverrideConfig[[name]]
    } else if (
      !is.null(defaultValues[[name]]) && !is.null(plotConfiguration[[name]])
    ) {
      if (all(plotConfiguration[[name]] == defaultValues[[name]])) {
        plotConfiguration[[name]] <- plotOverrideConfig[[name]]
      }
    }
  }

  return(plotConfiguration)
}

#' Apply Specific Configuration Overrides to Default Plot Configuration
#'
#' This function applies specific configuration overrides to the default plot
#' configuration. It first applies any additional parameters provided via `...`,
#' then updates the default configuration with overrides from the
#' `plotOverrideConfig` list, but only if the corresponding values have not
#' already been set by the additional parameters. Finally, it validates the
#' final configuration to ensure all options are valid.
#'
#' @param defaultPlotConfiguration An object of class `DefaultPlotConfiguration`
#'   or a list of such objects.
#' @param plotOverrideConfig A list with new configuration settings to apply.
#' @param ... Additional parameters to override specific configuration settings
#'   dynamically.
#'
#' @keywords internal
#' @noRd
.applyPlotConfiguration <- function(
  defaultPlotConfiguration = NULL,
  plotOverrideConfig = NULL,
  ...
) {
  # validate input defaultPlotConfiguration
  if (is.null(defaultPlotConfiguration)) {
    defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  } else {
    validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration")
  }

  # Clone the `DefaultPlotConfiguration` object
  # If a list of configurations is passed, clone only the first configuration
  # in the list. List processing not supported yet.
  if (inherits(defaultPlotConfiguration, "list")) {
    customPlotConfiguration <- defaultPlotConfiguration[[1]]$clone()
  } else {
    customPlotConfiguration <- defaultPlotConfiguration$clone()
  }

  # Capture additional parameters passed through ... and override
  additionalParams <- list(...)
  for (param in names(additionalParams)) {
    if (!is.null(additionalParams[[param]])) {
      customPlotConfiguration[[param]] <- additionalParams[[param]]
    }
  }

  # override only default configuration values with settings in plotOverrideConfig
  customPlotConfiguration <- .updatePlotConfiguration(
    customPlotConfiguration,
    plotOverrideConfig
  )

  # convert to list and validate final plot configuration
  plotConfigurationList <- purrr::map(
    purrr::set_names(names(customPlotConfiguration)),
    ~ customPlotConfiguration[[.]]
  )
  optionNames <- unique(c(names(plotOverrideConfig), names(additionalParams)))
  ospsuite.utils::validateIsOption(
    plotConfigurationList,
    .getPlotConfigurationOptions(optionNames)
  )

  return(customPlotConfiguration)
}


#' Calculate axis limits
#'
#' This function calculates axis limits based on minimum and maximum values.
#'
#' @param x Numeric vector for which limits are calculated.
#'
#' @keywords internal
#' @noRd
.calculateLimits <- function(x, scaling = NULL) {
  if (!is.null(scaling) && scaling == "log") {
    limits <- c(
      min(x[x > 0], na.rm = TRUE) * 0.9,
      max(x[x > 0], na.rm = TRUE) * 1.1
    )
  } else {
    limits <- c(
      (if (min(x, na.rm = TRUE) <= 0) 1.01 else 0.99) * min(x, na.rm = TRUE),
      (if (max(x, na.rm = TRUE) > 0) 1.01 else 0.99) * max(x, na.rm = TRUE)
    )
  }

  return(limits)
}

#' Get valid plot configuration options
#'
#' Generates a list of valid configuration options for plotting. Each
#' configuration option specifies constraints, including data type, allowable
#' values, and value ranges, formatted to facilitate validation with
#' `ospsuite::validateIsOption` function.
#'
#' @returns A list of lists, each containing type specifications and constraints
#'   for a plot configuration parameter.
#' @keywords internal
#' @noRd
.getPlotConfigurationOptions <- function(names) {
  plotConfigurationOptions <- list(
    legendPosition = list(
      type = "character",
      allowedValues = c("left", "right", "bottom", "top", "none")
    ),
    legendTitle = list(
      type = "character",
      nullAllowed = TRUE
    ),
    linesAlpha = list(
      type = "numeric",
      valueRange = c(0, 1)
    ),
    linesSize = list(
      type = "numeric",
      valueRange = c(0.1, 10)
    ),
    parameterFactor = list(
      type = "numeric",
      valueRange = c(1e-16, 1e16)
    ),
    pointsShape = list(
      type = "integer",
      valueRange = c(0L, 25L)
    ),
    pointsSize = list(
      type = "numeric",
      valueRange = c(0.1, 10)
    ),
    subtitle = list(
      type = "character",
      nullAllowed = TRUE
    ),
    title = list(
      type = "character",
      nullAllowed = TRUE
    ),
    titleSize = list(
      type = "numeric"
    ),
    xAxisScale = list(
      type = "character",
      allowedValues = c("log", "lin")
    ),
    xLabel = list(
      type = "character",
      nullAllowed = TRUE
    ),
    yAxisFacetScales = list(
      type = "character",
      allowedValues = c("fixed", "free")
    ),
    yAxisScale = list(
      type = "character",
      allowedValues = c("log", "lin")
    ),
    yAxisTicks = list(
      type = "integer",
      valueRange = c(1L, 20L)
    ),
    xAxisType = list(
      type = "character",
      allowedValues = c("percent", "absolute")
    ),
    yAxisType = list(
      type = "character",
      allowedValues = c("percent", "absolute")
    ),
    yLabel = list(
      type = "character",
      nullAllowed = TRUE
    )
  )

  return(plotConfigurationOptions[names])
}

#' Read plot configurations from a Project object
#'
#' Reads plot grids, plot configurations, and export configurations from
#' `project$plots` instead of Excel files.
#'
#' @param project Object of class `Project`
#' @param plotGridNames Names of the plot grids to filter for. If `NULL`,
#'   all plot grids are returned.
#'
#' @returns A list with elements `plotGrids`, `exportConfigurations`, and
#'   `plotConfigurations`, or `NULL` if no plot grids are defined.
#' @keywords internal
.getPlotConfigurations <- function(project, plotGridNames) {
  dfPlotGrids <- project$plots$plotGrids
  dfExportConfigurations <- project$plots$exportConfiguration

  # Handle empty export configurations (no columns)
  if (ncol(dfExportConfigurations) == 0) {
    dfExportConfigurations <- data.frame(
      plotGridName = character(0),
      name = character(0)
    )
  }

  # Rename outputName to name if present (legacy column name from Excel)
  if ("outputName" %in% names(dfExportConfigurations)) {
    dfExportConfigurations <- dplyr::rename(
      dfExportConfigurations,
      name = outputName
    )
  }

  # Filter for only specified plot grids
  if (!is.null(plotGridNames)) {
    missingPlotGrids <- setdiff(plotGridNames, unique(dfPlotGrids$name))
    if (length(missingPlotGrids) != 0) {
      stop(messages$invalidPlotGridNames(missingPlotGrids))
    }

    dfPlotGrids <- dplyr::filter(dfPlotGrids, name %in% plotGridNames)
    dfExportConfigurations <- dplyr::filter(
      dfExportConfigurations,
      plotGridName %in% plotGridNames
    )
  }

  # Exit early if no PlotGrid is defined
  if (nrow(dfPlotGrids) == 0) {
    return()
  }

  dfPlotConfigurations <- project$plots$plotConfiguration

  # Filter and validate plotGrids
  dfPlotGrids <- dplyr::filter(
    dfPlotGrids,
    !dplyr::if_all(dplyr::everything(), is.na)
  )

  # Exit early if all rows were NA

  if (nrow(dfPlotGrids) == 0) {
    return()
  }

  dfPlotGrids <- .validatePlotGrids(
    dfPlotGrids,
    unique(dfPlotConfigurations$plotID)
  )

  # Filter and validate only used plot configurations
  dfPlotConfigurations <- dplyr::filter(
    dfPlotConfigurations,
    plotID %in% unlist(unique(dfPlotGrids$plotIDs))
  )

  return(list(
    plotGrids = dfPlotGrids,
    exportConfigurations = dfExportConfigurations,
    plotConfigurations = dfPlotConfigurations
  ))
}
