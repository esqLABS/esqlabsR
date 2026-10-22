test_that(".parseExcelMultiValueField numeric conversion path is covered", {
  result <- .parseExcelMultiValueField(
    value = "72.5, 80.5",
    fieldName = "test",
    plotID = "P1",
    expectedLength = 2,
    expectedType = "numeric"
  )
  expect_equal(result, c(72.5, 80.5))
  expect_true(is.numeric(result))

  expect_error(
    .parseExcelMultiValueField(
      value = "72 80",
      fieldName = "test",
      plotID = "P1",
      expectedLength = 2,
      expectedType = "numeric"
    ),
    regexp = "Invalid format.*Expected.*Values separated by commas",
    fixed = FALSE
  )
})

test_that(".validateClassHasField is NA-safe when the object has NA names", {
  object <- list(1, 2)
  names(object) <- c(NA, "b")

  expect_false(.validateClassHasField(object, "x"))
  expect_true(.validateClassHasField(object, "b"))
})

test_that("the plot-build assertion helpers raise cli (rlang) errors", {
  expect_error(
    .assertPlotConfigurationsBuildable(
      list(a = list(plotId = "p1", plotType = "individual")),
      dataCombinedNames = character()
    ),
    class = "rlang_error"
  )
  expect_error(
    .assertPlotGridsBuildable(
      list(g = list(plotGridId = "g1", plotIds = "ghost")),
      plotIDs = "p1"
    ),
    class = "rlang_error"
  )
})

test_that(".assertPlotGridsBuildable aborts on a grid missing its plotGridId", {
  # A grid declaring plotIds but no plotGridId must be rejected up front,
  # rather than slipping through to fail opaquely at grid-name assignment.
  expect_snapshot(
    error = TRUE,
    .assertPlotGridsBuildable(
      list(list(plotIds = "p1")),
      plotIDs = "p1"
    )
  )
})

test_that(".assertPlotConfigurationsBuildable aborts on a plot missing its plotId", {
  # A single plot with no plotId must be rejected up front. The downstream
  # duplicate check only flags two-or-more missing ids, so one id-less entry
  # would otherwise slip through and fail opaquely at grid build.
  expect_snapshot(
    error = TRUE,
    .assertPlotConfigurationsBuildable(
      list(list(dataCombinedId = "dc", plotType = "individual")),
      dataCombinedNames = "dc"
    )
  )
})

# createPlots(project, ...) tests ----

test_that("createPlots errors on non-Project input", {
  expect_error(createPlots("not a project"), "expected <Project>")
})

test_that("createPlots returns empty list when project has no plots", {
  project <- testProject()
  expect_identical(createPlots(project), list())
})

test_that("createPlots builds plot grids for Example project", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")
  gridName <- names(project$definitions$plotGrids)[[1]]

  result <- createPlots(
    project,
    plotGrids = gridName,
    scenarioResults = simulated
  )

  expect_named(result, gridName)
  expect_s3_class(result[[gridName]], "patchwork")
})

test_that("createPlots succeeds when a plot belongs to no grid", {
  project <- exampleProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_outside",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  # Plot referencing dc_outside is in no grid; createPlots(project) builds
  # all grids and must not abort just because dc_outside is unbuilt.
  addPlot(project, "p_outside", "dc_outside", "individual")
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    scenarioResults = simulated,
    validate = FALSE
  ))

  expect_named(result, "individual_diagnostics")
  expect_s3_class(result[["individual_diagnostics"]], "patchwork")
})

test_that("createPlots grid subset ignores plots in other grids", {
  project <- exampleProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_other",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv_population",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "p_other", "dc_other", "population")
  addPlotGrid(project, "grid_other", plots = "p_other")
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  # Requesting only the original grid must not abort over grid_other's DC,
  # which references a scenario absent from `simulated`.
  result <- suppressWarnings(createPlots(
    project,
    plotGrids = "individual_diagnostics",
    scenarioResults = simulated,
    validate = FALSE
  ))

  expect_named(result, "individual_diagnostics")
})

test_that("createPlots aborts on unknown plotGrids when stopIfNotFound", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  expect_snapshot(
    error = TRUE,
    createPlots(
      project,
      plotGrids = "DoesNotExist",
      scenarioResults = simulated,
      validate = FALSE,
      stopIfNotFound = TRUE
    )
  )
})

test_that("createPlots silently drops unknown plotGrids when not stopIfNotFound", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    plotGrids = "DoesNotExist",
    scenarioResults = simulated,
    validate = FALSE,
    stopIfNotFound = FALSE
  ))
  expect_identical(result, list())
})

test_that("createPlots builds every plot type end to end", {
  project <- testProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  dataSet <- names(loadObservedData(project))

  addDataCombined(
    project,
    "dc_ind",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = path,
      group = "g"
    )),
    observed = list(list(label = "obs", dataSet = dataSet, group = "g"))
  )
  addDataCombined(
    project,
    "dc_pop",
    simulated = list(list(
      label = "sim",
      scenario = "populationscenario",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "p_ind", "dc_ind", "individual")
  addPlot(project, "p_pop", "dc_pop", "population")
  addPlot(project, "p_ovs", "dc_ind", "observedVsSimulated")
  addPlot(project, "p_rvs", "dc_ind", "residualsVsSimulated")
  addPlot(project, "p_rvt", "dc_ind", "residualsVsTime")
  addPlotGrid(
    project,
    "grid_all",
    plots = c("p_ind", "p_pop", "p_ovs", "p_rvs", "p_rvt")
  )
  simulated <- runScenarios(
    project,
    scenarios = c("testscenario", "populationscenario")
  )

  result <- suppressWarnings(createPlots(
    project,
    plotGrids = "grid_all",
    scenarioResults = simulated,
    validate = FALSE
  ))

  expect_named(result, "grid_all")
  expect_s3_class(result[["grid_all"]], "patchwork")
})

test_that("createPlots survives a save/load round trip that drops the title column", {
  dir <- withr::local_tempdir()
  initProject(
    destination = dir,
    type = "example",
    createExcel = FALSE,
    overwrite = TRUE
  )
  project <- loadProject(file.path(dir, "Project.json"))
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_nt",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "p_nt", "dc_nt", "individual")
  addPlotGrid(project, "grid_nt", plots = "p_nt")
  # Drop the original titled plot/grid so no plot or grid sets a title; the
  # title column is then dropped on save and absent on reload.
  removePlotGrid(project, "individual_diagnostics")
  removePlot(project, "p1")
  # Save the in-memory edits so the reload sees them.
  saveProject(project)

  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_false("title" %in% names(reloaded$definitions$plots$p_nt))
  simulated <- runScenarios(reloaded, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    reloaded,
    scenarioResults = simulated,
    validate = FALSE
  ))
  expect_named(result, "grid_nt")
})

test_that("createPlots observedVsSimulated survives a dropped foldDistance column", {
  dir <- withr::local_tempdir()
  initProject(
    destination = dir,
    type = "example",
    createExcel = FALSE,
    overwrite = TRUE
  )
  project <- loadProject(file.path(dir, "Project.json"))
  path <- project$definitions$outputPaths$aciclovir_pvb
  dataSet <- names(loadObservedData(project))
  addDataCombined(
    project,
    "dc_ovs",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    )),
    observed = list(list(label = "obs", dataSet = dataSet, group = "g"))
  )
  # Title kept, foldDistance left unset so its column is dropped on save.
  addPlot(project, "p_ovs", "dc_ovs", "observedVsSimulated", title = "OvS")
  addPlotGrid(project, "grid_ovs", plots = "p_ovs", title = "Grid OvS")
  removePlotGrid(project, "individual_diagnostics")
  removePlot(project, "p1")
  # Save the in-memory edits so the reload sees them.
  saveProject(project)

  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_false(
    "foldDistance" %in% names(reloaded$definitions$plots$p_ovs)
  )
  simulated <- runScenarios(reloaded, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    reloaded,
    scenarioResults = simulated,
    validate = FALSE
  ))
  expect_named(result, "grid_ovs")
})

# createPlots(plots = ...): standalone single plots ----

test_that("createPlots(plots) returns a single plot keyed by plotId", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    plots = "p1",
    scenarioResults = simulated,
    validate = FALSE
  ))

  # One entry, keyed by the plotId, holding the rendered single plot (not a
  # 1-cell grid).
  expect_named(result, "p1")
  expect_false(inherits(result$p1, "patchwork"))
  expect_s3_class(result$p1, "ggplot")
})

test_that("createPlots unions plotGrids and plots, keyed by id", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    plotGrids = "individual_diagnostics",
    plots = "p1",
    scenarioResults = simulated,
    validate = FALSE
  ))

  # The grid (keyed by plotGridId) and the standalone plot (keyed by plotId)
  # both appear: a plotId that is also inside a requested grid still gets its
  # own standalone entry (independent selectors, no de-dup).
  expect_setequal(names(result), c("individual_diagnostics", "p1"))
  expect_s3_class(result$individual_diagnostics, "patchwork")
  expect_s3_class(result$p1, "ggplot")
})

test_that("createPlots(plots) aborts on an unknown plotId when stopIfNotFound", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  expect_snapshot(
    error = TRUE,
    createPlots(
      project,
      plots = "ghost_plot",
      scenarioResults = simulated,
      validate = FALSE,
      stopIfNotFound = TRUE
    )
  )
})

test_that("createPlots(plots) drops an unknown plotId when not stopIfNotFound", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    plots = "ghost_plot",
    scenarioResults = simulated,
    validate = FALSE,
    stopIfNotFound = FALSE
  ))
  expect_identical(result, list())
})

test_that("createPlots with neither argument still returns all grids", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    scenarioResults = simulated,
    validate = FALSE
  ))

  # Default (neither plotGrids nor plots): every grid, no standalone plot.
  expect_named(result, "individual_diagnostics")
  expect_s3_class(result$individual_diagnostics, "patchwork")
})

test_that("createPlots NULL/NULL default returns every grid keyed by gridId", {
  # A second grid so "all grids" is not coincidentally the lone example grid;
  # a regression defaulting to an empty or single-grid result would be caught.
  project <- exampleProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_second",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "p_second", "dc_second", "individual")
  addPlotGrid(project, "grid_second", plots = "p_second")
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  # Both plotGrids and plots omitted (NULL): the result must hold every
  # grid in the project, keyed by gridId, and nothing else.
  result <- suppressWarnings(createPlots(
    project,
    scenarioResults = simulated,
    validate = FALSE
  ))

  expect_length(result, length(project$definitions$plotGrids))
  expect_setequal(names(result), names(project$definitions$plotGrids))
  expect_setequal(
    names(result),
    c("individual_diagnostics", "grid_second")
  )
  expect_s3_class(result$individual_diagnostics, "patchwork")
  expect_s3_class(result$grid_second, "patchwork")
})

test_that("createPlots(plots) builds the DataCombined the standalone plot needs", {
  project <- exampleProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  # A standalone plot whose DataCombined is referenced by no grid: the build
  # must still construct that DataCombined (the scope extends to plots).
  addDataCombined(
    project,
    "dc_solo",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "p_solo", "dc_solo", "individual")
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    plots = "p_solo",
    scenarioResults = simulated,
    validate = FALSE
  ))
  expect_named(result, "p_solo")
  expect_s3_class(result$p_solo, "ggplot")
})

# createPlots: axis labels and plotType validation ----

test_that("createPlots carries xLabel/yLabel onto the built plot", {
  project <- exampleProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_lab",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  # xLabel/yLabel were silently dropped before: they were listed in the
  # excluded styleFields yet never re-applied, so a user's axis labels were
  # ignored. The rendered plot must now carry them.
  addPlot(
    project,
    "p_lab",
    "dc_lab",
    "individual",
    xLabel = "Time [h]",
    yLabel = "Conc"
  )
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    plots = "p_lab",
    scenarioResults = simulated,
    validate = FALSE
  ))

  expect_identical(result$p_lab$labels$x, "Time [h]")
  expect_identical(result$p_lab$labels$y, "Conc")
})

test_that("createPlots keeps a comma-bearing grid subtitle intact", {
  project <- exampleProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_sub",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "p_sub", "dc_sub", "individual")
  # A grid subtitle used to be routed through the comma-splitting scan (it was
  # missing from `gridStyleFields`), so "Model A, Model B" was silently shredded
  # into c("Model A", "Model B"). It must now reach the grid as one string.
  addPlotGrid(
    project,
    "grid_sub",
    plots = "p_sub",
    subtitle = "Model A, Model B"
  )
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    plotGrids = "grid_sub",
    scenarioResults = simulated,
    validate = FALSE
  ))

  subtitle <- patchwork:::get_patches(result$grid_sub)$annotation$subtitle
  expect_identical(subtitle, "Model A, Model B")
})

test_that("createPlots aborts on an unknown plotType even when validate = FALSE", {
  project <- exampleProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_bad",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  # An unknown plotType used to build to NULL invisibly (silently dropped
  # from a grid). It must abort during the buildability check, naming the
  # offending plot and type, regardless of `validate`.
  addPlot(project, "p_bad", "dc_bad", "individual")
  plots <- .getSection(project, "plots")
  plots[["p_bad"]]$plotType <- "timeprofile"
  .setSection(project, "plots", plots)
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  expect_snapshot(
    error = TRUE,
    suppressWarnings(createPlots(
      project,
      plots = "p_bad",
      scenarioResults = simulated,
      validate = FALSE
    ))
  )
})

# Plot definition fields -> ospsuite plot functions ----

test_that(".plotFunctionArgs maps fields to the ospsuite arguments of each plot type", {
  argsFor <- function(entry) {
    .plotFunctionArgs(
      .plotConfigurationForEntry(entry),
      entry$plotType,
      dataCombined = "dc"
    )
  }
  houseErrorbars <- list(width = 2.75, linewidth = 0.65)

  population <- argsFor(list(
    plotType = "population",
    xUnit = "h",
    yUnit = "µmol/l",
    yAxisScale = "log",
    aggregation = "quantiles",
    quantiles = "0.1, 0.5, 0.9",
    nsd = 2,
    linesSize = 2,
    pointsAlpha = 0.4,
    ribbonsAlpha = 0.3,
    ribbonsLinetype = "dashed",
    errorbarsLinetype = "dotted"
  ))
  ovs <- argsFor(list(
    plotType = "observedVsSimulated",
    yAxisScale = "linear",
    foldDistance = 3,
    lloqDirection = "vertical"
  ))
  residuals <- argsFor(list(
    plotType = "residualsVsTime",
    xUnit = "min",
    yUnit = "mg/l",
    linesLinetype = "dotted"
  ))

  expect_equal(
    list(
      population = population,
      ovs = ovs,
      residuals = residuals
    ),
    list(
      population = list(
        plotData = "dc",
        yUnit = "µmol/l",
        xUnit = "h",
        yScale = "log",
        geomPointAttributes = list(size = 1.75, alpha = 0.4),
        geomErrorbarAttributes = c(houseErrorbars, linetype = "dotted"),
        geomLineAttributes = list(linewidth = 2, alpha = 0.75),
        aggregation = "quantiles",
        quantiles = c(0.1, 0.5, 0.9),
        nsd = 2,
        geomRibbonAttributes = list(alpha = 0.3, linetype = "dashed")
      ),
      ovs = list(
        plotData = "dc",
        xyScale = "linear",
        geomPointAttributes = list(size = 1.75),
        geomErrorbarAttributes = houseErrorbars,
        geomComparisonLineAttributes = list(
          linetype = "dashed",
          linewidth = 0.5,
          alpha = 0.75
        ),
        comparisonLineVector = ospsuite.plots::getFoldDistanceList(folds = 3),
        lloqOnBothAxes = FALSE
      ),
      residuals = list(
        plotData = "dc",
        yUnit = "mg/l",
        xUnit = "min",
        geomPointAttributes = list(size = 1.75),
        geomErrorbarAttributes = houseErrorbars,
        geomComparisonLineAttributes = list(
          linetype = "dotted",
          linewidth = 0.5,
          alpha = 0.75
        )
      )
    )
  )
})

test_that(".plotScaleArgs maps limits, ticks, tick labels, and expansion", {
  scaleArgs <- function(entry, axis, plotType = "individual") {
    .plotScaleArgs(.plotConfigurationForEntry(entry), axis, plotType)
  }

  expect_null(scaleArgs(list(), "y"))

  # Value limits only: points outside are removed.
  valuesOnly <- scaleArgs(list(yValuesLimits = "1, 100"), "y")
  expect_equal(valuesOnly$limits, c(1, 100))
  expect_equal(
    valuesOnly$oob(c(0.5, 50, 200), range = c(1, 100)),
    c(NA, 50, NA)
  )

  # Axis limits only: the axis is zoomed, points outside are kept.
  axisOnly <- scaleArgs(list(xAxisLimits = "0, 12"), "x")
  expect_equal(axisOnly$limits, c(0, 12))
  expect_equal(axisOnly$oob(c(-1, 6, 24), range = c(0, 12)), c(-1, 6, 24))

  # Both on a log axis: the axis limits set the range, and the value limits
  # remove points on the transformed (log10) scale the oob function receives.
  both <- scaleArgs(
    list(
      yAxisScale = "log",
      yAxisLimits = "0.1, 1000",
      yValuesLimits = "1, 100"
    ),
    "y"
  )
  expect_equal(both$limits, c(0.1, 1000))
  expect_equal(
    both$oob(log10(c(0.5, 50, 500)), range = log10(c(0.1, 1000))),
    c(NA, log10(50), NA)
  )
  # On an observedVsSimulated plot the axes are logarithmic by default.
  ovs <- scaleArgs(
    list(yAxisLimits = "0.1, 1000", yValuesLimits = "1, 100"),
    "y",
    "observedVsSimulated"
  )
  expect_equal(ovs$oob(log10(c(0.5, 50)), range = 0), c(NA, log10(50)))

  # Ticks, label texts, and no expansion on the time axis, which keeps its
  # start at 0.
  ticks <- scaleArgs(
    list(
      xAxisTicks = "0, 12, 24",
      xAxisTicksLabels = "dose, \"12 h, second dose\", end",
      xAxisExpand = FALSE
    ),
    "x"
  )
  expect_equal(
    ticks,
    list(
      limits = c(0, NA),
      breaks = c(0, 12, 24),
      labels = c("dose", "12 h, second dose", "end"),
      expand = ggplot2::expansion(mult = 0, add = 0)
    )
  )
})

test_that("a plot without title fields gets no title", {
  # The house settings hold `titleSize`, which `$title` would partially match.
  config <- .plotConfigurationForEntry(list(plotType = "individual"))
  plot <- ggplot2::ggplot() +
    .plotStyleComponents(config, "individual", ggplot2::ggplot())

  expect_equal(
    list(title = plot$labels$title, subtitle = plot$labels$subtitle),
    list(title = NULL, subtitle = NULL)
  )
})

test_that("the tick label names give the tlf labels", {
  expect_equal(
    list(
      none = .plotTickLabels("none")(c(1, 10)),
      identity = .plotTickLabels("identity")(c(1, 10)),
      log = .plotTickLabels("log")(c(1, 20)),
      greek = .plotTickLabels("greek")(c(1, 2)),
      pi = .plotTickLabels("pi")(c(0, pi / 2, pi)),
      percentiles = .plotTickLabels("percentiles")(0)
    ),
    list(
      none = c("", ""),
      identity = c("1", "10"),
      log = expression(10^0, 2 %*% 10^1),
      greek = c("\u03b1", "\u03b2"),
      pi = c("0", " + \u03c0/2", "\u03c0"),
      percentiles = "50%"
    )
  )
})

test_that("createPlots applies axis limits, labels, and the title to a time profile", {
  project <- exampleProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_lim",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  addPlot(
    project,
    "p_lim",
    "dc_lim",
    "individual",
    title = "Aciclovir, IV",
    xAxisLimits = c(0, 6),
    yAxisScale = "log"
  )
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")

  plot <- suppressWarnings(createPlots(
    project,
    plots = "p_lim",
    scenarioResults = simulated
  ))$p_lim

  expect_equal(ggplot2::layer_scales(plot)$x$get_limits(), c(0, 6))
  expect_equal(plot$scales$get_scales("y")$trans$name, "log-10")
  expect_identical(plot$labels$title, "Aciclovir, IV")
})

test_that("the build guard rejects field problems even when validate = FALSE", {
  expect_snapshot(
    error = TRUE,
    .assertPlotConfigurationsBuildable(
      list(
        p1 = list(
          plotId = "p1",
          dataCombinedId = "dc",
          plotType = "observedVsSimulated",
          xAxisScale = "log",
          yAxisScale = "linear",
          pointSize = 2
        )
      ),
      dataCombinedNames = "dc"
    )
  )
})

test_that("createPlots draws lines, points, and ribbons in the ESQlabs colors", {
  # `{ospsuite.plots}` adds its own colour and fill scales at build time when a
  # plot has none, so the check is on the built layers, not on the plot object.
  builtColors <- function(plot, aesthetic) {
    unique(unlist(lapply(
      ggplot2::ggplot_build(plot)$data,
      function(layer) layer[[aesthetic]]
    )))
  }
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_pop",
    simulated = list(list(
      label = "Aciclovir population",
      scenario = "aciclovir_iv_population",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  addPlot(project, "p_pop", "dc_pop", "population")
  simulated <- runScenarios(
    project,
    scenarios = c("aciclovir_iv", "aciclovir_iv_population")
  )

  plots <- suppressWarnings(createPlots(
    project,
    plots = c("p1", "p_pop"),
    scenarioResults = simulated
  ))

  expect_equal(
    list(
      individualColour = builtColors(plots$p1, "colour"),
      populationFill = builtColors(plots$p_pop, "fill")
    ),
    list(
      individualColour = esqlabsColors(1),
      populationFill = esqlabsColors(1)
    )
  )
})

# Every plot field changes the drawn plot ----

test_that("every plot field changes the plot of the plot types that apply it", {
  skip_if_not_installed("svglite")
  project <- exampleProject()
  results <- suppressWarnings(runScenarios(
    project,
    scenarios = c("aciclovir_iv", "aciclovir_iv_population")
  ))
  path <- project$definitions$outputPaths$aciclovir_pvb
  # An LLOQ, so that the LLOQ fields have a line to act on.
  observed <- loadObservedData(project)[[1]]
  observed$LLOQ <- 1
  individual <- DataCombined$new()
  individual$addSimulationResults(
    results$aciclovir_iv$results,
    quantitiesOrPaths = path,
    names = "Aciclovir simulated",
    groups = "Aciclovir"
  )
  individual$addDataSets(
    observed,
    names = "Aciclovir observed",
    groups = "Aciclovir"
  )
  population <- DataCombined$new()
  population$addSimulationResults(
    results$aciclovir_iv_population$results,
    quantitiesOrPaths = path,
    names = "Population",
    groups = "Aciclovir"
  )
  population$addDataSets(
    observed,
    names = "Aciclovir observed",
    groups = "Aciclovir"
  )

  render <- function(entry) {
    dataCombined <- if (entry$plotType == "population") {
      population
    } else {
      individual
    }
    plot <- suppressWarnings(.buildPlot(entry, dataCombined))
    svg <- svglite::svgstring(width = 7, height = 5, standalone = FALSE)
    suppressWarnings(print(plot))
    grDevices::dev.off()
    as.character(svg())
  }

  # A value for each field that differs from its house and `ospsuite` default.
  sample <- list(
    xUnit = "min",
    yUnit = "µg/l",
    title = "Aciclovir",
    subtitle = "IV",
    caption = "Source: study",
    xLabel = "Time",
    yLabel = "Concentration",
    legendTitle = "Compound",
    legendPosition = "insideTopRight",
    legendBackgroundColor = "yellow",
    legendBackgroundAlpha = 0.5,
    legendBorderColor = "red",
    legendBorderType = "dotted",
    legendBorderSize = 2,
    xAxisScale = "log",
    yAxisScale = "log",
    xAxisLimits = "1, 6",
    yAxisLimits = "1, 5",
    xValuesLimits = "1, 6",
    yValuesLimits = "1, 5",
    xAxisTicks = "0, 6, 18",
    yAxisTicks = "1, 5",
    xAxisTicksLabels = "none",
    yAxisTicksLabels = "greek",
    xAxisExpand = FALSE,
    yAxisExpand = FALSE,
    watermark = "DRAFT",
    linesColor = "red, navy",
    linesSize = 3,
    linesLinetype = "dotted",
    linesAlpha = 0.2,
    pointsColor = "red",
    pointsShape = "square, triangle",
    pointsSize = 5,
    pointsAlpha = 0.2,
    ribbonsFill = "red",
    ribbonsSize = 2,
    ribbonsLinetype = "dotted",
    ribbonsAlpha = 0.1,
    errorbarsSize = 2,
    errorbarsCapSize = 10,
    errorbarsLinetype = "dotted",
    errorbarsAlpha = 0.1,
    displayLLOQ = FALSE,
    lloqDirection = "vertical",
    foldLinesLegend = FALSE,
    foldLinesLegendDiagonal = FALSE,
    aggregation = "arithmetic",
    quantiles = "0.25, 0.5, 0.75",
    nsd = 3,
    foldDistance = 3,
    lloqOnBothAxes = FALSE
  )
  for (element in c(.plotTextElements, "watermark")) {
    style <- list(
      Color = "red",
      Size = 17,
      FontFace = "bold.italic",
      FontFamily = "mono",
      Angle = 45,
      Align = if (element %in% c("caption", "yAxisLabelTicks")) {
        "left"
      } else {
        "right"
      },
      Margin = if (element == "watermark") "10, 0, 60, 0" else "25, 25, 25, 25"
    )
    sample[paste0(element, names(style))] <- style
  }
  for (prefix in c("xAxis", "yAxis", "xGrid", "yGrid")) {
    sample[paste0(prefix, c("Color", "Size", "Linetype"))] <- list(
      "red",
      3,
      "dotted"
    )
  }
  for (prefix in c("plotBackground", "plotPanelBackground")) {
    sample[paste0(prefix, c("Fill", "Color", "Size", "Linetype"))] <-
      list("lightyellow", "red", 3, "dotted")
  }
  expect_setequal(names(sample), names(.plotFields))

  # Fields that need another field to have something to act on.
  companions <- function(field) {
    element <- sub(
      paste0("(", paste(.plotTextStyleSuffixes, collapse = "|"), ")$"),
      "",
      field
    )
    textElements <- c(
      "title",
      "subtitle",
      "caption",
      "legendTitle",
      "watermark"
    )
    switch(
      field,
      nsd = list(aggregation = "arithmetic"),
      if (element != field && element %in% textElements) {
        stats::setNames(list(sample[[element]]), element)
      } else {
        list()
      }
    )
  }
  # Each field on the first plot type that applies it, plus the plot types on
  # which it acts through another mechanism.
  supportedFields <- setdiff(names(.plotFields), .plotFieldsNotSupportedYet)
  # Whether a font family changes the drawn text depends on the fonts installed
  # on the machine, so the font family fields are checked on the theme below.
  fontFamilyFields <- grep("FontFamily$", supportedFields, value = TRUE)
  checks <- lapply(setdiff(supportedFields, fontFamilyFields), function(field) {
    plotType <- .plotFields[[field]]$appliesTo[[1]]
    if (field == "lloqDirection") {
      plotType <- "observedVsSimulated"
    }
    list(field = field, plotType = plotType)
  })
  otherMechanisms <- list(
    c("linesLinetype", "residualsVsTime"),
    c("linesSize", "observedVsSimulated"),
    c("linesAlpha", "residualsVsSimulated"),
    c("pointsColor", "residualsVsTime"),
    c("xAxisScale", "residualsVsSimulated"),
    c("xAxisLimits", "observedVsSimulated"),
    c("yValuesLimits", "observedVsSimulated"),
    c("xAxisTicks", "residualsVsTime"),
    c("errorbarsCapSize", "observedVsSimulated")
  )
  checks <- c(
    checks,
    lapply(otherMechanisms, function(x) list(field = x[[1]], plotType = x[[2]]))
  )

  baselines <- list()
  withoutEffect <- character()
  for (check in checks) {
    base <- c(
      list(plotId = "p", plotType = check$plotType),
      companions(check$field)
    )
    key <- paste(check$plotType, names(base)[-(1:2)], collapse = "/")
    baselines[[key]] <- baselines[[key]] %||% render(base)
    entry <- base
    entry[[check$field]] <- sample[[check$field]]
    if (
      check$plotType == "observedVsSimulated" &&
        grepl("AxisScale$", check$field)
    ) {
      entry[[check$field]] <- "linear"
    }
    if (identical(render(entry), baselines[[key]])) {
      withoutEffect <- c(
        withoutEffect,
        paste(check$field, "on", check$plotType)
      )
    }
  }

  expect_identical(withoutEffect, character())

  themeElements <- c(
    title = "plot.title",
    subtitle = "plot.subtitle",
    caption = "plot.caption",
    xLabel = "axis.title.x",
    yLabel = "axis.title.y",
    legendTitle = "legend.title",
    legendKeys = "legend.text",
    xAxisLabelTicks = "axis.text.x",
    yAxisLabelTicks = "axis.text.y"
  )
  families <- vapply(
    fontFamilyFields,
    function(field) {
      element <- sub("FontFamily$", "", field)
      entry <- c(
        list(plotId = "p", plotType = "individual"),
        companions(field),
        stats::setNames(list(sample[[field]]), field)
      )
      plot <- suppressWarnings(.buildPlot(entry, individual))
      ggplot2::calc_element(
        themeElements[[element]],
        ggplot2::complete_theme(plot$theme)
      )$family
    },
    character(1)
  )
  expect_equal(
    families,
    stats::setNames(rep("mono", length(fontFamilyFields)), fontFamilyFields)
  )

  # The fields the OSP Suite plot functions cannot draw yet change nothing.
  withEffect <- character()
  for (field in .plotFieldsNotSupportedYet) {
    plotType <- .plotFields[[field]]$appliesTo[[1]]
    base <- list(plotId = "p", plotType = plotType)
    entry <- c(base, sample[field])
    if (!identical(render(entry), render(base))) {
      withEffect <- c(withEffect, field)
    }
  }
  expect_identical(withEffect, character())
})
