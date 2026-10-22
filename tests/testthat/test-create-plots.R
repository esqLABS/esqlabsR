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

test_that(".buildPlot passes each field to the ospsuite argument of its plot type", {
  calls <- list()
  capture <- function(fn) {
    function(...) {
      calls[[fn]] <<- list(...)
      NULL
    }
  }
  local_mocked_bindings(
    plotTimeProfile = capture("plotTimeProfile"),
    plotPredictedVsObserved = capture("plotPredictedVsObserved"),
    plotResidualsVsCovariate = capture("plotResidualsVsCovariate"),
    .package = "ospsuite"
  )

  .buildPlot(
    list(
      plotId = "p_pop",
      plotType = "population",
      xUnit = "h",
      yUnit = "µmol/l",
      yAxisScale = "log",
      aggregation = "quantiles",
      quantiles = "0.1, 0.5, 0.9",
      nsd = 2
    ),
    dataCombined = "dc"
  )
  .buildPlot(
    list(
      plotId = "p_ovs",
      plotType = "observedVsSimulated",
      yAxisScale = "linear",
      foldDistance = 3
    ),
    dataCombined = "dc"
  )
  .buildPlot(
    list(
      plotId = "p_rvt",
      plotType = "residualsVsTime",
      xUnit = "min",
      yUnit = "mg/l"
    ),
    dataCombined = "dc"
  )

  expect_equal(
    calls,
    list(
      plotTimeProfile = list(
        plotData = "dc",
        yUnit = "µmol/l",
        xUnit = "h",
        yScale = "log",
        aggregation = "quantiles",
        quantiles = c(0.1, 0.5, 0.9),
        nsd = 2
      ),
      plotPredictedVsObserved = list(
        plotData = "dc",
        xyScale = "linear",
        comparisonLineVector = ospsuite.plots::getFoldDistanceList(folds = 3),
        lloqOnBothAxes = TRUE
      ),
      plotResidualsVsCovariate = list(
        plotData = "dc",
        yUnit = "mg/l",
        xUnit = "min",
        xAxis = "time"
      )
    )
  )
})

test_that(".plotScaleArgs maps value limits and axis limits", {
  expect_null(.plotScaleArgs(list(plotId = "p"), "x"))

  # Value limits only: points outside are removed.
  valuesOnly <- .plotScaleArgs(
    list(plotId = "p", yValuesLimits = "1, 100"),
    "y"
  )
  expect_equal(valuesOnly$limits, c(1, 100))
  expect_equal(
    valuesOnly$oob(c(0.5, 50, 200), range = c(1, 100)),
    c(NA, 50, NA)
  )

  # Axis limits only: the axis is zoomed, points outside are kept.
  axisOnly <- .plotScaleArgs(list(plotId = "p", xAxisLimits = "0, 12"), "x")
  expect_equal(axisOnly$limits, c(0, 12))
  expect_equal(axisOnly$oob(c(-1, 6, 24), range = c(0, 12)), c(-1, 6, 24))

  # Both on a log axis: the axis limits set the range, and the value limits
  # remove points on the transformed (log10) scale the oob function receives.
  both <- .plotScaleArgs(
    list(
      plotId = "p",
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
          pointsSize = 2
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
