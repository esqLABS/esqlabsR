# Plot configuration utilities ----
#
# Palette, plot / grid configuration constructors, and the override helpers the
# sensitivity plots use to merge their own settings into the user's plot
# configuration.

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
    cli::cli_abort(messages$nrOfColorsShouldBePositive(nrOfColors))
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
col2hsv <- function(color) {
  validateIsString(color)
  rgb <- col2rgb(color)
  return(grDevices::rgb2hsv(rgb))
}

#' @title Create the ESQlabs plot configuration
#' @rdname createEsqlabsPlotConfiguration
#'
#' @description
#'
#' A named list of plot settings (text sizes, line and point sizes, colors,
#' legend position) chosen to reflect the corporate standards adopted by
#' ESQlabs GmbH.
#'
#' Pass it, changed or not, as `defaultPlotConfiguration` to
#' [sensitivitySpiderPlot()], [sensitivityTornadoPlot()], and
#' [sensitivityTimeProfiles()]. [createPlots()] applies the same values to the
#' plots it builds.
#'
#' @returns A named list.
#'
#' @examples
#' myPlotConfiguration <- createEsqlabsPlotConfiguration()
#' myPlotConfiguration$linesSize <- 1
#'
#' @family createPlottingConfigurations
#'
#' @export
createEsqlabsPlotConfiguration <- function() {
  list(
    # Text sizes
    titleSize = 10,
    xLabelSize = 9,
    yLabelSize = 9,
    xAxisLabelTicksSize = 8,
    yAxisLabelTicksSize = 8,
    legendKeysSize = 6,
    xLabelMargin = c(10, 0, 0, 0),
    yLabelMargin = c(0, 0, 10, 0),
    # Lines, points, and error bars
    linesSize = 0.5,
    linesAlpha = 0.75,
    pointsSize = 1.75,
    pointsShape = ospsuite.plots::ospShapeNames,
    errorbarsSize = 0.65,
    errorbarsCapSize = 2.75,
    # Legend and axes
    legendPosition = "top",
    legendJustification = "left",
    yAxisLabelTicksAngle = 0,
    # Colors
    pointsColor = esqlabsEnv$colorPalette,
    ribbonsFill = esqlabsEnv$colorPalette,
    linesColor = esqlabsEnv$colorPalette
  )
}

#' House style for the plots built by `createPlots()`
#'
#' A `ggplot2` theme whose sizes, margins, and legend placement come from
#' [createEsqlabsPlotConfiguration()], plus discrete colour and fill scales
#' with the [esqlabsColors()] palette. Margins are `c(top, right, bottom,
#' left)` in points.
#'
#' The palette is set through scales rather than the theme: an `{ospsuite.plots}`
#' plot that has no colour or fill scale gets the OSP scales when it is built,
#' which ignore the theme palette.
#'
#' @returns A list of `ggplot2` components, to add to a plot with `+`.
#' @keywords internal
#' @noRd
.esqlabsPlotStyle <- function() {
  config <- createEsqlabsPlotConfiguration()
  marginFrom <- function(values) {
    ggplot2::margin(
      t = values[[1]],
      r = values[[2]],
      b = values[[3]],
      l = values[[4]],
      unit = "pt"
    )
  }
  theme <- ggplot2::theme(
    # A text box wraps a title that is wider than the plot. The subtitle is
    # aligned with the title.
    plot.title = ggtext::element_textbox_simple(
      size = config$titleSize,
      hjust = 0
    ),
    plot.subtitle = ggtext::element_textbox_simple(hjust = 0),
    axis.title.x = ggplot2::element_text(
      size = config$xLabelSize,
      margin = marginFrom(config$xLabelMargin)
    ),
    axis.title.y = ggplot2::element_text(
      size = config$yLabelSize,
      margin = marginFrom(config$yLabelMargin)
    ),
    axis.text.x = ggplot2::element_text(size = config$xAxisLabelTicksSize),
    axis.text.y = ggplot2::element_text(
      size = config$yAxisLabelTicksSize,
      angle = config$yAxisLabelTicksAngle
    ),
    legend.key.size = ggplot2::unit(config$legendKeysSize, "pt"),
    legend.position = config$legendPosition,
    legend.justification = config$legendJustification
  )
  list(
    theme,
    ggplot2::discrete_scale("colour", palette = esqlabsColors),
    ggplot2::discrete_scale("fill", palette = esqlabsColors)
  )
}

#' @title Create the ESQlabs plot grid configuration
#' @rdname createEsqlabsPlotGridConfiguration
#'
#' @description
#'
#' A [PlotGridConfiguration] object for arranging several plots in one figure
#' with [plotGrid()].
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by ESQlabs GmbH.
#'
#' @returns A [PlotGridConfiguration] object.
#'
#' @examples
#' createEsqlabsPlotGridConfiguration()
#'
#' @family createPlottingConfigurations
#'
#' @export
createEsqlabsPlotGridConfiguration <- function() {
  # nolint: object_length_linter.
  plotGridConfiguration <- PlotGridConfiguration$new()

  plotGridConfiguration$tagLevels <- "a"
  plotGridConfiguration$tagSize <- 11
  plotGridConfiguration$titleSize <- 12

  plotGridConfiguration$titleHorizontalJustification <- 0.5

  return(plotGridConfiguration)
}

# Plot configuration override and overlay helpers ----

#' Apply function-specific settings to a plot configuration
#'
#' A setting from `plotOverrideConfig` replaces the value in
#' `plotConfiguration` when that value is unset (absent from the list) or still
#' equals the value from [createEsqlabsPlotConfiguration()]. A value the user
#' changed is kept.
#'
#' @param plotConfiguration A named list of plot settings.
#' @param plotOverrideConfig A named list with new configuration settings to
#'   apply.
#' @returns The updated named list.
#' @keywords internal
#' @noRd
.updatePlotConfiguration <- function(plotConfiguration, plotOverrideConfig) {
  defaultValues <- createEsqlabsPlotConfiguration()

  for (name in names(plotOverrideConfig)) {
    currentValue <- plotConfiguration[[name]]
    # Use an NA-safe comparison: a plain `all(x == default)` returns NA when
    # either side holds an NA, and `isTRUE(NA)` is FALSE, which would silently
    # skip a legit override.
    isDefault <- !is.null(defaultValues[[name]]) &&
      isTRUE(all.equal(
        currentValue,
        defaultValues[[name]],
        check.attributes = FALSE
      ))
    if (is.null(currentValue) || isDefault) {
      # `[name]` rather than `[[name]]` so that a `NULL` override is stored as
      # an unset entry instead of being dropped from the list.
      plotConfiguration[name] <- list(plotOverrideConfig[[name]])
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
#' @param defaultPlotConfiguration A named list from
#'   [createEsqlabsPlotConfiguration()], or `NULL` to use that list unchanged.
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
  if (is.null(defaultPlotConfiguration)) {
    defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  } else if (
    !is.list(defaultPlotConfiguration) ||
      !rlang::is_named(defaultPlotConfiguration)
  ) {
    cli::cli_abort(
      messages$plotConfigurationNotNamedList(class(defaultPlotConfiguration)),
      call = rlang::caller_env()
    )
  }
  customPlotConfiguration <- defaultPlotConfiguration

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

  # validate final plot configuration
  optionNames <- unique(c(names(plotOverrideConfig), names(additionalParams)))
  ospsuite.utils::validateIsOption(
    customPlotConfiguration,
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
    positive <- x[x > 0]
    limits <- c(
      min(positive, na.rm = TRUE) * 0.9,
      max(positive, na.rm = TRUE) * 1.1
    )
    # A single distinct value collapses the range; widen multiplicatively so
    # both bounds stay strictly positive (a log axis cannot include zero).
    if (limits[[1]] == limits[[2]]) {
      v <- limits[[1]]
      limits <- c(v * 0.95, v * 1.05)
    }
  } else {
    limits <- c(
      (if (min(x, na.rm = TRUE) <= 0) 1.01 else 0.99) * min(x, na.rm = TRUE),
      (if (max(x, na.rm = TRUE) > 0) 1.01 else 0.99) * max(x, na.rm = TRUE)
    )
    # A single distinct value (e.g. all zeros -> c(0, 0)) collapses the range,
    # which renders as a blank panel downstream. Widen to a small symmetric pad
    # around the value.
    if (limits[[1]] == limits[[2]]) {
      v <- limits[[1]]
      pad <- max(abs(v), 1) * 0.05
      limits <- c(v - pad, v + pad)
    }
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
      allowedValues = c(
        ospsuite.plots::AxisScales$linear,
        ospsuite.plots::AxisScales$log
      )
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
      allowedValues = c(
        ospsuite.plots::AxisScales$linear,
        ospsuite.plots::AxisScales$log
      )
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
