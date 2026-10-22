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
    legendPosition = "outsideTopLeft",
    yAxisLabelTicksAngle = 0,
    # Colors
    pointsColor = esqlabsEnv$colorPalette,
    ribbonsFill = esqlabsEnv$colorPalette,
    linesColor = esqlabsEnv$colorPalette
  )
}

# Plot style ----
#
# The parts of a Plot definition's settings that are added to the plot the
# `ospsuite` plot function returns: one theme, the titles, the color, fill,
# shape, and line type scales, legend guides, and the watermark. The field
# table `.plotFields` (R/plots.R) says which plot types apply each field.

#' Style components for one plot
#'
#' Every color, fill, and shape scale is set explicitly: a `{ospsuite.plots}`
#' plot that has no such scale gets the OSP scales when it is built, which
#' ignore any theme palette.
#'
#' @param config Plot settings from `.plotConfigurationForEntry()`.
#' @param plotType The plot type.
#' @param plot The plot the `ospsuite` plot function returned, read for the
#'   theme values and scales that a setting changes.
#' @returns A list of `ggplot2` components, to add to `plot` with `+`.
#' @keywords internal
#' @noRd
.plotStyleComponents <- function(config, plotType, plot) {
  c(
    list(
      .plotTheme(config, plot),
      do.call(
        ggplot2::labs,
        .dropNull(list(
          title = config$title,
          subtitle = config$subtitle,
          caption = config$caption,
          x = config$xLabel,
          y = config$yLabel,
          colour = config$legendTitle,
          fill = config$legendTitle
        ))
      )
    ),
    .plotScales(config, plotType, plot),
    .plotWatermark(config)
  )
}

# `tlf` alignments as `hjust` values.
.plotHjust <- function(align) {
  if (is.null(align)) {
    return(NULL)
  }
  c(left = 0, center = 0.5, right = 1)[[align]]
}

.plotMargin <- function(values) {
  if (is.null(values)) {
    return(NULL)
  }
  ggplot2::margin(
    t = values[[1]],
    r = values[[2]],
    b = values[[3]],
    l = values[[4]],
    unit = "pt"
  )
}

#' The theme element of one text element
#'
#' @param config Plot settings.
#' @param element One of `.plotTextElements`.
#' @param textbox Whether the element is a plot title, subtitle, or caption,
#'   drawn as a text box that wraps long text (unless it is rotated).
#' @param defaultAlign Alignment when the element sets none.
#' @returns A theme element, or `NULL` when no style field of the element is
#'   set and it is not a text box.
#' @keywords internal
#' @noRd
.plotTextElement <- function(
  config,
  element,
  textbox = FALSE,
  defaultAlign = NULL
) {
  style <- function(suffix) config[[paste0(element, suffix)]]
  isSet <- vapply(
    .plotTextStyleSuffixes,
    function(suffix) !is.null(style(suffix)),
    logical(1)
  )
  if (!any(isSet) && !textbox) {
    return(NULL)
  }
  hjust <- .plotHjust(style("Align") %||% defaultAlign)
  properties <- .dropNull(list(
    colour = style("Color"),
    size = style("Size"),
    face = style("FontFace"),
    family = style("FontFamily"),
    hjust = hjust,
    margin = .plotMargin(style("Margin"))
  ))
  angle <- style("Angle")
  if (textbox && (is.null(angle) || angle == 0)) {
    return(do.call(
      ggtext::element_textbox_simple,
      c(properties, .dropNull(list(halign = hjust)))
    ))
  }
  do.call(ggplot2::element_text, c(properties, .dropNull(list(angle = angle))))
}

#' Legend placement for one `legendPosition` value
#'
#' `none` hides the legend. An `inside...` value places it inside the panel at
#' that corner or side. An `outside...` value places it on the side named first
#' and justifies it toward the side named second (`outsideTopLeft`: top, left).
#' A `ggplot2` name (`top`, `bottom`, `left`, `right`) is used as is, centered.
#'
#' @param position One of `.plotLegendPositions`.
#' @returns A list of theme arguments.
#' @keywords internal
#' @noRd
.plotLegendPlacement <- function(position) {
  if (position %in% c("none", "top", "bottom", "left", "right")) {
    return(list(
      legend.position = position,
      legend.justification = if (position != "none") "center"
    ))
  }
  sides <- tolower(regmatches(
    position,
    gregexpr("Top|Bottom|Left|Right", position)
  )[[1]])
  if (startsWith(position, "inside")) {
    point <- c(
      x = if ("left" %in% sides) {
        0
      } else if ("right" %in% sides) {
        1
      } else {
        0.5
      },
      y = if ("bottom" %in% sides) {
        0
      } else if ("top" %in% sides) {
        1
      } else {
        0.5
      }
    )
    return(list(
      legend.position = "inside",
      legend.position.inside = unname(point),
      legend.justification = unname(point)
    ))
  }
  list(
    legend.position = sides[[1]],
    legend.justification = if (length(sides) > 1L) sides[[2]] else "center"
  )
}

#' The theme of one plot
#'
#' @inheritParams .plotStyleComponents
#' @returns A `ggplot2` theme.
#' @keywords internal
#' @noRd
.plotTheme <- function(config, plot) {
  lineElement <- function(prefix) {
    properties <- .dropNull(list(
      colour = config[[paste0(prefix, "Color")]],
      linewidth = config[[paste0(prefix, "Size")]],
      linetype = config[[paste0(prefix, "Linetype")]]
    ))
    if (length(properties) == 0L) {
      return(NULL)
    }
    do.call(ggplot2::element_line, properties)
  }
  rectElement <- function(fill, colour, linewidth, linetype) {
    properties <- .dropNull(list(
      fill = fill,
      colour = colour,
      linewidth = linewidth,
      linetype = linetype
    ))
    if (length(properties) == 0L) {
      return(NULL)
    }
    do.call(ggplot2::element_rect, properties)
  }

  elements <- list(
    plot.title = .plotTextElement(config, "title", TRUE, "left"),
    plot.subtitle = .plotTextElement(config, "subtitle", TRUE, "left"),
    plot.caption = .plotTextElement(config, "caption", TRUE, "right"),
    axis.title.x = .plotTextElement(config, "xLabel"),
    axis.title.y = .plotTextElement(config, "yLabel"),
    legend.text = .plotTextElement(config, "legendKeys"),
    axis.text.x = .plotTextElement(config, "xAxisLabelTicks"),
    axis.text.y = .plotTextElement(config, "yAxisLabelTicks"),
    axis.line.x = lineElement("xAxis"),
    axis.line.y = lineElement("yAxis"),
    panel.grid.major.x = lineElement("xGrid"),
    panel.grid.major.y = lineElement("yGrid"),
    plot.background = rectElement(
      config$plotBackgroundFill,
      config$plotBackgroundColor,
      config$plotBackgroundSize,
      config$plotBackgroundLinetype
    ),
    panel.background = rectElement(
      config$plotPanelBackgroundFill,
      NULL,
      NULL,
      NULL
    ),
    # `ospsuite.plots` draws the panel outline with `panel.border`.
    panel.border = if (
      !is.null(config$plotPanelBackgroundColor) ||
        !is.null(config$plotPanelBackgroundSize) ||
        !is.null(config$plotPanelBackgroundLinetype)
    ) {
      rectElement(
        NA,
        config$plotPanelBackgroundColor,
        config$plotPanelBackgroundSize,
        config$plotPanelBackgroundLinetype
      )
    }
  )
  # `ospsuite` blanks the legend title; a `legendTitle` shows it again.
  if (!is.null(config$legendTitle)) {
    elements$legend.title <- .plotTextElement(config, "legendTitle") %||%
      ggplot2::element_text()
  }

  backgroundFill <- config$legendBackgroundColor
  if (!is.null(config$legendBackgroundAlpha)) {
    backgroundFill <- backgroundFill %||%
      ggplot2::calc_element(
        "legend.background",
        ggplot2::complete_theme(plot$theme)
      )$fill %||%
      "white"
    backgroundFill <- scales::alpha(
      backgroundFill,
      config$legendBackgroundAlpha
    )
  }
  elements$legend.background <- rectElement(
    backgroundFill,
    config$legendBorderColor,
    config$legendBorderSize,
    config$legendBorderType
  )

  if (!is.null(config$legendPosition)) {
    elements <- c(elements, .plotLegendPlacement(config$legendPosition))
  }

  do.call(ggplot2::theme, .dropNull(elements))
}

#' Color, fill, shape, and line type scales and legend guides of one plot
#'
#' On a time profile a simulated curve and its observed points share one group
#' color, from `linesColor`; the fill is `linesColor` on `individual` plots and
#' `ribbonsFill` on `population` plots. On the other plot types the points take
#' `pointsColor`. Colors, shapes, and line types are used in order and
#' recycled.
#'
#' @inheritParams .plotStyleComponents
#' @returns A list of `ggplot2` components.
#' @keywords internal
#' @noRd
.plotScales <- function(config, plotType, plot) {
  recycled <- function(aesthetic, values) {
    ggplot2::discrete_scale(
      aesthetic,
      palette = function(n) rep_len(values, n)
    )
  }
  isTimeProfile <- plotType %in% .timeProfilePlotTypes
  colors <- if (isTimeProfile) config$linesColor else config$pointsColor
  fills <- if (identical(plotType, "population")) config$ribbonsFill else colors
  components <- .dropNull(list(
    if (!is.null(colors)) recycled("colour", colors),
    if (!is.null(fills)) recycled("fill", fills),
    if (!is.null(config$pointsShape)) recycled("shape", config$pointsShape)
  ))

  if (isTimeProfile && !is.null(config$linesLinetype)) {
    components <- c(
      components,
      list(recycled("linetype", config$linesLinetype))
    )
  }

  if (identical(plotType, "observedVsSimulated")) {
    foldLines <- plot$scales$get_scales("linetype")
    if (
      !is.null(foldLines) &&
        (!is.null(config$linesLinetype) ||
          identical(config$foldLinesLegendDiagonal, FALSE))
    ) {
      labels <- foldLines$breaks
      linetypes <- foldLines$palette(length(labels))
      if (!is.null(config$linesLinetype)) {
        linetypes <- rep_len(config$linesLinetype, length(labels))
      }
      names(linetypes) <- labels
      # The first fold line is the line of identity.
      breaks <- if (identical(config$foldLinesLegendDiagonal, FALSE)) {
        labels[-1]
      } else {
        labels
      }
      components <- c(
        components,
        list(ggplot2::scale_linetype_manual(
          values = linetypes,
          breaks = breaks,
          guide = ggplot2::guide_legend(order = 10, title = NULL)
        ))
      )
    }
    if (identical(config$foldLinesLegend, FALSE)) {
      components <- c(components, list(ggplot2::guides(linetype = "none")))
    }
  }

  components
}

#' Watermark of one plot
#'
#' One text layer inside the plot's panel, so it stays with the plot inside a
#' plot grid. It is independent of the session watermark of `ospsuite.plots`,
#' whose `watermarkFormat` option gives the style a field leaves unset.
#'
#' @param config Plot settings.
#' @returns A list with one `ggplot2` layer, or an empty list.
#' @keywords internal
#' @noRd
.plotWatermark <- function(config) {
  if (is.null(config$watermark)) {
    return(list())
  }
  format <- ospsuite.plots::getOspsuite.plots.option(
    ospsuite.plots::OptionKeys$watermarkFormat
  )
  margin <- config$watermarkMargin %||% c(0, 0, 0, 0)
  align <- config$watermarkAlign %||% "center"
  x <- switch(
    align,
    left = grid::unit(0, "npc") + grid::unit(margin[[4]], "pt"),
    center = grid::unit(0.5, "npc"),
    right = grid::unit(1, "npc") - grid::unit(margin[[2]], "pt")
  )
  y <- grid::unit(0.5, "npc") +
    grid::unit((margin[[3]] - margin[[1]]) / 2, "pt")
  label <- grid::textGrob(
    config$watermark,
    x = x,
    y = y,
    hjust = .plotHjust(align),
    rot = config$watermarkAngle %||% format$angle,
    gp = grid::gpar(
      col = config$watermarkColor %||% format$color,
      alpha = if (is.null(config$watermarkColor)) format$alpha else 1,
      fontsize = config$watermarkSize %||% format$fontsize,
      fontfamily = config$watermarkFontFamily %||% "",
      fontface = config$watermarkFontFace %||% "plain"
    )
  )
  list(ggplot2::annotation_custom(label))
}

#' Tick labels ported from `tlf`
#'
#' @param ticks The tick values.
#' @returns The tick labels.
#' @keywords internal
#' @noRd
.logTickLabels <- function(ticks) {
  exponents <- floor(log10(ticks))
  prefixes <- paste0(ticks * 10^(-exponents), "%*%")
  prefixes[prefixes == "1%*%"] <- ""
  .parseTickLabels(paste0(prefixes, "10^", exponents))
}

.lnTickLabels <- function(ticks) {
  exponents <- floor(log(ticks))
  prefixes <- paste0(ticks * exp(-exponents), "%*%")
  prefixes[prefixes == "1%*%"] <- ""
  .parseTickLabels(paste0(prefixes, "e^", exponents))
}

.sqrtTickLabels <- function(ticks) {
  .parseTickLabels(paste0("sqrt(", ticks^2, ")"))
}

.greekTickLabels <- function(ticks) {
  vapply(ticks, function(tick) intToUtf8(tick + 944), character(1))
}

.percentileTickLabels <- function(ticks) {
  paste0(round(100 * stats::pnorm(ticks), 3), "%")
}

.piTickLabels <- function(ticks) {
  multiples <- as.character(ticks %/% pi)
  multiples[multiples == "1"] <- ""
  multiples[multiples == "-1"] <- "-"
  wholePart <- ifelse(multiples == "0", "", paste0(multiples, "π"))
  fractions <- c(
    "0.167" = " + π/6",
    "0.25" = " + π/4",
    "0.333" = " + π/3",
    "0.5" = " + π/2",
    "0.667" = " + 2π/3",
    "0.833" = " + 5π/6"
  )
  fraction <- round((ticks %% pi) / pi, 3)
  fractionPart <- vapply(
    fraction,
    function(f) {
      if (f == 0) {
        return("")
      }
      key <- as.character(f)
      if (key %in% names(fractions)) fractions[[key]] else paste0("+", f, "π")
    },
    character(1)
  )
  labels <- paste0(wholePart, fractionPart)
  labels[labels == ""] <- "0"
  labels
}

# Parse tick labels as plotmath expressions; `NA` ticks give empty labels.
.parseTickLabels <- function(texts) {
  as.expression(lapply(texts, function(text) {
    if (grepl("NA", text, fixed = TRUE)) "" else str2lang(text)
  }))
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
