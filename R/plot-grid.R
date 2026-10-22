# Plot grid ----
#
# `PlotGridConfiguration` and `plotGrid()` lay out a list of `ggplot` objects
# with `{patchwork}`. `createPlots()` builds every Plot Grid definition through
# them, and users can call them directly on plots made with the `{ospsuite}`
# plot functions.

#' Convert angle into label orientation
#'
#' Angles are counter-clockwise, as in `ggplot2`.
#'
#' @param angle numeric angle in degrees
#'
#' @keywords internal
#' @noRd
.convertAngleToOrientation <- function(angle) {
  # Modulo 360 so that negative angles map as well
  return(
    switch(
      as.character(angle %% 360),
      "0" = "upright",
      "90" = "left-rotated",
      "180" = "inverted",
      "270" = "right-rotated",
      "upright"
    )
  )
}

#' List of default text sizes for plot annotations.
#'
#' @keywords internal
#' @noRd
.PlotAnnotationTextSize <- ospsuite.utils::enum(
  c(
    # annotations for the entire grid as a whole
    "plotGridTitleSize" = 14,
    "plotGridSubtitleSize" = 12,
    "plotGridCaptionSize" = 10,
    "plotGridTagSize" = 8,

    # annotations for individual plots
    "plotTitleSize" = 12,
    "plotSubtitleSize" = 10,
    "plotCaptionSize" = 8,
    "plotXLabelSize" = 10,
    "plotYLabelSize" = 10,
    "plotLegendTitleSize" = 10,
    "plotLegendCaptionSize" = 10,
    "plotWatermarkSize" = 20
  )
)

#' Create a plot grid
#'
#' @description
#'
#' Arrange several plots in one figure, with a common title, subtitle, caption,
#' and a tag (a, b, c, ...) on each panel. The plots and the layout come from a
#' `PlotGridConfiguration` object, usually created with
#' [createEsqlabsPlotGridConfiguration()].
#'
#' The grid is drawn with `patchwork::wrap_plots()`. Long titles, subtitles,
#' and captions wrap onto several lines.
#'
#' @param plotGridConfiguration A `PlotGridConfiguration` object that holds the
#'   plots and the properties of the grid (number of rows and columns, tags,
#'   annotations).
#'
#' @returns A `patchwork` object. Print it to show the figure, or save it with
#'   `ggplot2::ggsave()`.
#'
#' @references
#' For more, see: <https://patchwork.data-imaginist.com/articles/patchwork.html>
#'
#' @examples
#' \dontrun{
#' simulation <- loadSimulation(
#'   system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' )
#' simulationResults <- runSimulations(simulation)[[1]]
#' dataCombined <- DataCombined$new()
#' dataCombined$addSimulationResults(simulationResults)
#'
#' plotGridConfiguration <- createEsqlabsPlotGridConfiguration()
#' plotGridConfiguration$title <- "Aciclovir"
#' plotGridConfiguration$addPlots(list(
#'   plotTimeProfile(dataCombined),
#'   plotTimeProfile(dataCombined, yScale = "log")
#' ))
#' plotGrid(plotGridConfiguration)
#' }
#'
#' @family createPlottingConfigurations
#' @export
plotGrid <- function(plotGridConfiguration) {
  ospsuite.utils::validateIsOfType(plotGridConfiguration, "PlotGridConfiguration")

  patchwork::wrap_plots(
    plotGridConfiguration$plotList,
    ncol = plotGridConfiguration$nColumns,
    nrow = plotGridConfiguration$nRows,
    byrow = plotGridConfiguration$byRow,
    widths = plotGridConfiguration$widths,
    heights = plotGridConfiguration$heights,
    guides = plotGridConfiguration$guides,
    design = plotGridConfiguration$design
  ) +
    patchwork::plot_annotation(
      title = plotGridConfiguration$title,
      subtitle = plotGridConfiguration$subtitle,
      caption = plotGridConfiguration$caption,
      tag_levels = plotGridConfiguration$tagLevels,
      tag_prefix = plotGridConfiguration$tagPrefix,
      tag_suffix = plotGridConfiguration$tagSuffix,
      tag_sep = plotGridConfiguration$tagSeparator,
      theme = ggplot2::theme(
        plot.title = ggtext::element_textbox_simple(
          color = plotGridConfiguration$titleColor,
          size = plotGridConfiguration$titleSize,
          face = plotGridConfiguration$titleFontFace,
          family = plotGridConfiguration$titleFontFamily,
          halign = plotGridConfiguration$titleHorizontalJustification,
          valign = plotGridConfiguration$titleVerticalJustification,
          hjust = plotGridConfiguration$titleHorizontalJustification,
          vjust = plotGridConfiguration$titleVerticalJustification,
          orientation = .convertAngleToOrientation(
            plotGridConfiguration$titleAngle
          ),
          margin = ggplot2::margin(
            t = plotGridConfiguration$titleMargin[1],
            r = plotGridConfiguration$titleMargin[2],
            b = plotGridConfiguration$titleMargin[3],
            l = plotGridConfiguration$titleMargin[4],
            unit = "pt"
          )
        ),
        plot.subtitle = ggtext::element_textbox_simple(
          color = plotGridConfiguration$subtitleColor,
          size = plotGridConfiguration$subtitleSize,
          face = plotGridConfiguration$subtitleFontFace,
          family = plotGridConfiguration$subtitleFontFamily,
          halign = plotGridConfiguration$subtitleHorizontalJustification,
          valign = plotGridConfiguration$subtitleVerticalJustification,
          hjust = plotGridConfiguration$subtitleHorizontalJustification,
          vjust = plotGridConfiguration$subtitleVerticalJustification,
          orientation = .convertAngleToOrientation(
            plotGridConfiguration$subtitleAngle
          ),
          margin = ggplot2::margin(
            t = plotGridConfiguration$subtitleMargin[1],
            r = plotGridConfiguration$subtitleMargin[2],
            b = plotGridConfiguration$subtitleMargin[3],
            l = plotGridConfiguration$subtitleMargin[4],
            unit = "pt"
          )
        ),
        plot.caption = ggtext::element_textbox_simple(
          color = plotGridConfiguration$captionColor,
          size = plotGridConfiguration$captionSize,
          face = plotGridConfiguration$captionFontFace,
          family = plotGridConfiguration$captionFontFamily,
          halign = plotGridConfiguration$captionHorizontalJustification,
          valign = plotGridConfiguration$captionVerticalJustification,
          hjust = plotGridConfiguration$captionHorizontalJustification,
          vjust = plotGridConfiguration$captionVerticalJustification,
          orientation = .convertAngleToOrientation(
            plotGridConfiguration$captionAngle
          ),
          margin = ggplot2::margin(
            t = plotGridConfiguration$captionMargin[1],
            r = plotGridConfiguration$captionMargin[2],
            b = plotGridConfiguration$captionMargin[3],
            l = plotGridConfiguration$captionMargin[4],
            unit = "pt"
          )
        )
      )
    ) &
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        color = plotGridConfiguration$tagColor,
        size = plotGridConfiguration$tagSize,
        family = plotGridConfiguration$tagFontFamily,
        face = plotGridConfiguration$tagFontFace,
        hjust = plotGridConfiguration$tagHorizontalJustification,
        vjust = plotGridConfiguration$tagVerticalJustification,
        angle = plotGridConfiguration$tagAngle,
        lineheight = plotGridConfiguration$tagLineHeight
      ),
      plot.tag.position = plotGridConfiguration$tagPosition
    )
}


#' @title Class for creating a plot grid
#'
#' @description
#'
#' An `R6` class defining the configuration for `{patchwork}` plot grid used to
#' create a grid of plots.
#' It holds values for all relevant plot properties.
#'
#' # Customizing
#'
#' You can change the default values present in public fields.
#'
#' For example, if you want to specify a new position for tags, you will have to
#' do the following:
#'
#' ```r
#' myPlotGridConfiguration <- PlotGridConfiguration$new()
#' myPlotGridConfiguration$tagPosition <- "right"
#' ```
#'
#' For more, see examples.
#'
#' # Specifying fonts
#'
#' A font is a particular set of glyphs (character shapes), differentiated from
#' other fonts in the same family by additional properties such as stroke
#' weight, slant, relative width, etc.
#'
#' A font face (aka typeface) is the design of lettering, characterized by
#' variations in size, weight (e.g. bold), slope (e.g. italic), width (e.g.
#' condensed), and so on. The available font faces are `"plain"`, `"bold"`,
#' `"italic"`, and `"bold.italic"`.
#'
#' A font family is a grouping of fonts defined by shared design styles.
#'
#' The available font families will depend on which fonts have been installed on
#' your computer. This information can be extracted by running the following
#' code:
#'
#' ```r
#' # install.packages("systemfonts")
#' library(systemfonts)
#' system_fonts()
#' ```
#'
#' # Saving plot
#'
#' By default, the plots will be shown in plot pane of your IDE, but the plots
#' can also be saved to a file using the `ggplot2::ggsave()` function.
#'
#' ```r
#' myPlot <- plotGrid(...)
#' ggplot2::ggsave(filename = "plot_1.png", plot = myPlot)
#' ```
#'
#' @field plotList A list containing `ggplot` objects.
#' @field title,subtitle,caption Text strings to use for the various plot
#' annotations, where plot refers to the grid of plots as a whole.
#' @field titleColor,titleSize,titleFontFace,titleFontFamily,titleHorizontalJustification,titleVerticalJustification,titleAngle,titleMargin Aesthetic properties for the plot title.
#' @field subtitleColor,subtitleSize,subtitleFontFace,subtitleFontFamily,subtitleHorizontalJustification,subtitleVerticalJustification,subtitleAngle,subtitleMargin Aesthetic properties for the plot subtitle.
#' @field captionColor,captionSize,captionFontFace,captionFontFamily,captionHorizontalJustification,captionVerticalJustification,captionAngle,captionMargin Aesthetic properties for the plot caption.
#' @field tagLevels A character vector defining the enumeration format to use
#' at each level. Possible values are `'a'` for lowercase letters, `'A'` for
#' uppercase letters, `'1'` for numbers, `'i'` for lowercase Roman numerals, and
#' `'I'` for uppercase Roman numerals. It can also be a list containing
#' character vectors defining arbitrary tag sequences. If any element in the
#' list is a scalar and one of `'a'`, `'A'`, `'1'`, `'i`, or `'I'`, this level
#' will be expanded to the expected sequence.
#' @field tagPrefix,tagSuffix Strings that should appear before or after the
#' tag.
#' @field tagSeparator A separator between different tag levels.
#' @field tagPosition Position of the tag for an individual plot with respect to
#'   that plot. Default is top left. Possible values are `"topleft"`, `"top"`,
#'   `"topright"`, `"left"`, `"right"`, `"bottomleft"`, `"bottom"`, and
#'   `"bottomright"`.
#' @field tagColor,tagSize,tagFontFamily,tagFontFace,tagHorizontalJustification,tagVerticalJustification,tagAngle,tagLineHeight,tagMargin Aesthetic properties of individual plot tag text.
#'   For more detailed description of each aesthetic property, see docs for
#'   [element_text()][ggplot2::element_text].
#' @field nColumns,nRows The dimensions of the grid to create - if both are
#'   `NULL` it will use the same logic as [facet_wrap()][ggplot2::facet_wrap] to
#'   set the dimensions
#' @field byRow Analogous to `byrow` in [matrix()][base::matrix]. If `FALSE` the
#' plots will be filled in in column-major order.
#' @field widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid.
#' @field guides A string specifying how guides should be treated in the layout.
#' `'collect'` will collect guides below to the given nesting level, removing
#' duplicates. `'keep'` will stop collection at this level and let guides be
#' placed alongside their plot. `auto` will allow guides to be collected if a
#' upper level tries, but place them alongside the plot if not. If you modify
#' default guide "position" with [theme(legend.position=...)][ggplot2::theme]
#' while also collecting guides you must apply that change to the overall
#' patchwork.
#' @field design Specification of the location of areas in the layout. Can
#'   either be specified as a text string or by concatenating calls to [area()]
#'   together. See the examples in [wrap_plots()][patchwork::wrap_plots] for
#'   further information on use.
#'
#' @examples
#' plotGridConfiguration <- PlotGridConfiguration$new()
#' plotGridConfiguration$title <- "My figure"
#' plotGridConfiguration$nColumns <- 2L
#' plotGridConfiguration
#'
#' @family createPlottingConfigurations
#' @export
PlotGridConfiguration <- R6::R6Class(
  "PlotGridConfiguration",
  public = list(
    plotList = list(),

    # title ------------------------------------

    title = NULL,
    titleColor = "black",
    titleSize = .PlotAnnotationTextSize$plotGridTitleSize,
    titleFontFace = "plain",
    titleFontFamily = "",
    titleHorizontalJustification = 0,
    titleVerticalJustification = 0,
    titleAngle = 0,
    titleMargin = c(20, 2, 10, 2),

    # subtitle ------------------------------------

    subtitle = NULL,
    subtitleColor = "black",
    subtitleSize = .PlotAnnotationTextSize$plotGridSubtitleSize,
    subtitleFontFace = "plain",
    subtitleFontFamily = "",
    subtitleHorizontalJustification = 0,
    subtitleVerticalJustification = 0,
    subtitleAngle = 0,
    subtitleMargin = c(0, 2, 10, 2),

    # caption ------------------------------------

    caption = NULL,
    captionColor = "black",
    captionSize = .PlotAnnotationTextSize$plotGridCaptionSize,
    captionFontFace = "plain",
    captionFontFamily = "",
    captionHorizontalJustification = 1,
    captionVerticalJustification = 0,
    captionAngle = 0,
    captionMargin = c(2, 2, 5, 2),

    # arrangement ------------------------------------

    nColumns = NULL,
    nRows = NULL,
    byRow = NULL,
    widths = NULL,
    heights = NULL,
    guides = NULL,
    design = NULL,

    # tags ------------------------------------

    tagLevels = NULL,
    tagPrefix = NULL,
    tagSuffix = NULL,
    tagSeparator = NULL,
    tagPosition = NULL,

    # tag text ------------------------------------

    tagColor = "black",
    tagSize = .PlotAnnotationTextSize$plotGridTagSize,
    tagFontFace = "plain",
    tagFontFamily = "",
    tagHorizontalJustification = 0,
    tagVerticalJustification = 0,
    tagAngle = 0,
    tagLineHeight = NULL,
    tagMargin = NULL,

    #' @description Create an instance of `PlotGridConfiguration` class.
    #'
    #' @param plotList A list containing `ggplot` objects.
    #'
    #' @returns A `PlotGridConfiguration` object.
    initialize = function(plotList = NULL) {
      self$plotList <- plotList
    },

    #' @description Add a plot object.
    #'
    #' @param plots A single or a list containing `ggplot` object(s).
    #'
    #' @returns The `PlotGridConfiguration` object, invisibly, with the plots
    #'   appended to `$plotList`.
    addPlots = function(plots = NULL) {
      if (!is.null(plots)) {
        ospsuite.utils::validateIsOfType(plots, "ggplot")

        # A single plot is wrapped so that `append()` adds it as one element.
        if (inherits(plots, "ggplot")) {
          plots <- list(plots)
        }

        self$plotList <- append(self$plotList, plots)
      }
      invisible(self)
    },

    #' @description
    #' Print the object to the console.
    #' @param ... Not used.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(
        list(
          "Title" = self$title,
          "Subtitle" = self$subtitle,
          "Caption" = self$caption
        ),
        title = "Plot grid annotations"
      )
      ospsuite.utils::ospPrintItems(
        list(
          "Number of plots included" = length(self$plotList),
          "Number of columns in the grid" = self$nColumns,
          "Number of rows in the grid" = self$nRows,
          "Arranged in row-major order" = self$byRow
        ),
        title = "Plot grid arrangement"
      )
      ospsuite.utils::ospPrintItems(
        list(
          "Tag level format" = self$tagLevels,
          "Tag level prefix" = self$tagPrefix,
          "Tag level suffix" = self$tagSuffix,
          "Tag level separator" = self$tagSeparator
        ),
        title = "Individual plot tags"
      )
      invisible(self)
    }
  )
)
