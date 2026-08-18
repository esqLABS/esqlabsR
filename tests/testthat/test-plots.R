test_that(".parseNestedDataCombined re-keys by id and drops the id field", {
  raw <- list(
    list(
      dataCombinedId = "DC1",
      simulated = list(list(label = "a")),
      observed = list()
    ),
    list(
      dataCombinedId = "DC2",
      simulated = list(),
      observed = list(list(label = "b"))
    )
  )
  parsed <- .parseNestedDataCombined(raw)
  expect_named(parsed, c("DC1", "DC2"))
  expect_named(parsed$DC1, c("simulated", "observed"))
  expect_equal(parsed$DC1$simulated[[1]]$label, "a")
  expect_equal(parsed$DC2$observed[[1]]$label, "b")
})

test_that(".parseNestedDataCombined returns empty list for NULL or empty input", {
  expect_identical(.parseNestedDataCombined(NULL), list())
  expect_identical(.parseNestedDataCombined(list()), list())
})

test_that(".parsePlotEntries re-keys by id and drops absent fields", {
  raw <- list(
    list(plotId = "P1", plotType = "individual", title = "T1"),
    list(plotId = "P2", plotType = "population")
  )
  parsed <- .parsePlotEntries(raw, "plotId", "Plot")
  expect_named(parsed, c("P1", "P2"))
  expect_s3_class(parsed$P1, "Plot")
  expect_equal(parsed$P1$title, "T1")
  # An absent optional field is simply absent (no NA cell).
  expect_false("title" %in% names(parsed$P2))
})

test_that(".parsePlotEntries returns an empty list for NULL or empty", {
  expect_identical(.parsePlotEntries(NULL, "plotId", "Plot"), list())
  expect_identical(
    .parsePlotEntries(list(), "plotId", "Plot"),
    list()
  )
})

test_that("addPlotGrid aborts when no plots are defined", {
  project <- testProject()
  .setSection(project, "plots", list())
  expect_snapshot(
    error = TRUE,
    addPlotGrid(project, "G1", plots = "MissingPlot")
  )
})

test_that("addPlot stores a vector-valued field as one comma-separated string", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_vec",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv_population",
      path = project$definitions$outputPaths$aciclovir_pvb,
      group = "g"
    ))
  )
  addPlot(
    project,
    "p_vec",
    "dc_vec",
    "population",
    quantiles = c(0.05, 0.5, 0.95)
  )

  cfg <- project$definitions$plots
  # One new keyed entry, classed Plot, with the vector stored as a
  # comma-separated string to match the parser shape and the dispatcher's
  # `strsplit(quantiles, ",")`.
  expect_true("p_vec" %in% names(cfg))
  expect_s3_class(cfg$p_vec, "Plot")
  expect_equal(cfg$p_vec$quantiles, "0.05, 0.5, 0.95")
})

# Overwrite policy ----

test_that("addDataCombined aborts on an existing id, replaces it with overwrite", {
  project <- exampleProject()
  mk <- function(lbl) {
    list(list(
      label = lbl,
      scenario = "aciclovir_iv_population",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  }
  addDataCombined(project, "dc", simulated = mk("first"))
  expect_snapshot(
    error = TRUE,
    addDataCombined(project, "dc", simulated = mk("second"))
  )
  before <- length(project$definitions$dataCombined)
  addDataCombined(project, "dc", simulated = mk("second"), overwrite = TRUE)
  expect_length(project$definitions$dataCombined, before)
  expect_identical(
    project$definitions$dataCombined$dc$simulated[[1]]$label,
    "second"
  )
})

test_that("addPlot aborts on an existing id, replaces it with overwrite", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv_population",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  addPlot(project, "p", "dc", "population", title = "first")
  expect_snapshot(
    error = TRUE,
    addPlot(project, "p", "dc", "population", title = "second")
  )
  before <- length(project$definitions$plots)
  addPlot(project, "p", "dc", "population", title = "second", overwrite = TRUE)
  expect_length(project$definitions$plots, before)
  expect_identical(project$definitions$plots$p$title, "second")
})

test_that("addPlotGrid aborts on an existing id, replaces it with overwrite", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv_population",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  addPlot(project, "p", "dc", "population")
  addPlotGrid(project, "g", plots = "p", title = "first")
  expect_snapshot(
    error = TRUE,
    addPlotGrid(project, "g", plots = "p", title = "second")
  )
  before <- length(project$definitions$plotGrids)
  addPlotGrid(project, "g", plots = "p", title = "second", overwrite = TRUE)
  expect_length(project$definitions$plotGrids, before)
  expect_identical(project$definitions$plotGrids$g$title, "second")
})

# Vectorized plot mutators ----

test_that("addPlot adds N plots in one call (recycle + align + whole-vector)", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_a",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  addDataCombined(
    project,
    "dc_b",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv_population",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )

  addPlot(
    project,
    id = c("pa", "pb"),
    dataCombined = c("dc_a", "dc_b"), # length-N, aligned
    plotType = "individual", # length-1, recycled
    title = list("Plot A", "Plot B"), # per-definition list aligns
    xUnit = "h", # scalar recycled
    quantiles = c(0.05, 0.5, 0.95) # whole-vector applied to both
  )

  cfg <- project$definitions$plots
  expect_true(all(c("pa", "pb") %in% names(cfg)))
  expect_identical(cfg$pa$dataCombinedId, "dc_a")
  expect_identical(cfg$pb$dataCombinedId, "dc_b")
  expect_identical(cfg$pa$plotType, "individual")
  expect_identical(cfg$pa$title, "Plot A")
  expect_identical(cfg$pb$title, "Plot B")
  expect_identical(cfg$pa$xUnit, "h")
  expect_identical(cfg$pb$xUnit, "h")
  # Whole-vector field applied to both, stored as CSV.
  expect_identical(cfg$pa$quantiles, "0.05, 0.5, 0.95")
  expect_identical(cfg$pb$quantiles, "0.05, 0.5, 0.95")
})

test_that("addPlot vectorized equals N scalar adds and persists once", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_x",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )

  addPlot(
    project,
    id = c("p_one", "p_two"),
    dataCombined = "dc_x",
    plotType = c("individual", "observedVsSimulated")
  )

  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_true(all(
    c("p_one", "p_two") %in% names(reloaded$definitions$plots)
  ))
  expect_identical(
    reloaded$definitions$plots$p_two$plotType,
    "observedVsSimulated"
  )
})

test_that("addPlot aborts on a mismatched scalar-field length", {
  project <- exampleProject()
  expect_snapshot(
    error = TRUE,
    addPlot(
      project,
      id = c("p1x", "p2x", "p3x"),
      dataCombined = c("dc_a", "dc_b"),
      plotType = "individual"
    )
  )
})

test_that("addPlot is all-or-nothing: an invalid entry writes nothing", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_ok",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  before <- names(project$definitions$plots)
  plotsDir <- file.path(project$info$projectDirPath, "definitions", "plots")
  filesBefore <- list.files(plotsDir)

  # The second plot references an unknown dataCombined, so the whole call aborts
  # and neither plot is folded in or written.
  expect_error(
    addPlot(
      project,
      id = c("good", "bad"),
      dataCombined = c("dc_ok", "ghost_dc"),
      plotType = "individual"
    ),
    "not found"
  )
  expect_identical(names(project$definitions$plots), before)
  expect_setequal(list.files(plotsDir), filesBefore)
})

test_that("addPlotGrid vectorizes with per-grid plots and one write-through", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_g",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  addPlot(project, c("g_p1", "g_p2"), "dc_g", "individual")

  addPlotGrid(
    project,
    id = c("grid_one", "grid_two"),
    plots = list("g_p1", c("g_p1", "g_p2")), # per-grid list
    title = list("One", "Two")
  )

  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_true(all(
    c("grid_one", "grid_two") %in% names(reloaded$definitions$plotGrids)
  ))
  expect_identical(reloaded$definitions$plotGrids$grid_one$plotIds, "g_p1")
  expect_identical(
    reloaded$definitions$plotGrids$grid_two$plotIds,
    "g_p1, g_p2"
  )
})

test_that("addPlotGrid accepts a plain list of plot ids for a single grid", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_s",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  addPlot(project, c("sp1", "sp2"), "dc_s", "individual")

  addPlotGrid(project, "single_grid", plots = list("sp1", "sp2"))

  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(
    .splitPlotIDs(
      reloaded$definitions$plotGrids$single_grid$plotIds
    ),
    c("sp1", "sp2")
  )
})

test_that("addPlotGrid canonicalizes a comma out of a plot id and still resolves it", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_c",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  # A comma is canonicalized to `_` (#1158): the plot's id and the grid's
  # membership reference are canonicalized identically, so the grid still
  # resolves to the plot after both drop the comma.
  suppressWarnings(addPlot(project, "p,evil", "dc_c", "individual"))
  suppressWarnings(addPlotGrid(project, "comma_grid", plots = c("p,evil")))
  expect_true("p_evil" %in% names(project$definitions$plots))

  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(
    .splitPlotIDs(reloaded$definitions$plotGrids$comma_grid$plotIds),
    "p_evil"
  )
})

test_that(".joinPlotIDs / .splitPlotIDs round-trip delimiter-bearing ids", {
  ids <- c("plain", "has,comma", "has\\backslash", "trailing,")
  expect_identical(
    .splitPlotIDs(.joinPlotIDs(ids)),
    ids
  )
  expect_identical(.joinPlotIDs(character()), "")
  expect_identical(.splitPlotIDs(""), character())
})

# Validation ----

test_that(".validatePlots names the closest existing id for a dangling reference", {
  result <- .validatePlots(
    dataCombined = list(aciclovir_pvb = list()),
    plotConfig = list(
      p1 = list(
        plotId = "p1",
        dataCombinedId = "aciclovir_pbv",
        plotType = "individual"
      )
    ),
    plotGrids = list(grid = list(plotGridId = "grid", plotIds = "p2"))
  )

  expect_snapshot(cat(
    vapply(result$critical_errors, function(e) e$message, character(1)),
    sep = "\n"
  ))
})

test_that(".validatePlots reports an empty reference, which the missing-field loop lets through", {
  # `""` is not `NULL`, so the required-field loop accepts it. It must still be
  # reported here: `createPlots()` aborts on it, and nothing else flags it.
  result <- .validatePlots(
    dataCombined = list(aciclovir_pvb = list()),
    plotConfig = list(
      p1 = list(plotId = "p1", dataCombinedId = "", plotType = "individual")
    ),
    plotGrids = list(grid = list(plotGridId = "grid", plotIds = "p1, "))
  )

  messages <- vapply(
    result$critical_errors,
    function(e) e$message,
    character(1)
  )
  expect_true(any(grepl("unknown dataCombinedId", messages)))
  expect_true(any(grepl("unknown plotIds", messages)))
})

test_that("removePlot removes a vector of ids in one pass and warns on misses", {
  project <- exampleProject()
  addDataCombined(
    project,
    "dc_r",
    simulated = list(list(
      label = "s",
      scenario = "aciclovir_iv",
      path = project$definitions$outputPaths$aciclovir_pvb
    ))
  )
  addPlot(project, c("rp1", "rp2"), "dc_r", "individual")

  suppressWarnings(removePlot(project, c("rp1", "rp2", "ghost")))
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_false(any(
    c("rp1", "rp2") %in% names(reloaded$definitions$plots)
  ))
})

test_that("addDataCombined vectorizes with per-id entry lists", {
  project <- exampleProject()
  addDataCombined(
    project,
    id = c("dcm1", "dcm2"),
    simulated = list(
      list(list(
        label = "s1",
        scenario = "aciclovir_iv",
        path = project$definitions$outputPaths$aciclovir_pvb
      )),
      list(list(
        label = "s2",
        scenario = "aciclovir_iv_population",
        path = project$definitions$outputPaths$aciclovir_pvb
      ))
    )
  )

  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_true(all(
    c("dcm1", "dcm2") %in% names(reloaded$definitions$dataCombined)
  ))
  expect_identical(
    reloaded$definitions$dataCombined$dcm2$simulated[[1]]$scenario,
    "aciclovir_iv_population"
  )
})

# removeDataCombined ----

test_that("removeDataCombined drops the entry and deletes its definition file", {
  project <- exampleProject()
  dcFile <- file.path(
    project$info$projectDirPath,
    "definitions",
    "data-combined",
    "aciclovir_individual.json"
  )
  # The example defines the "aciclovir_individual" dataCombined.
  expect_true(
    "aciclovir_individual" %in% names(project$definitions$dataCombined)
  )
  expect_true(file.exists(dcFile))

  # It is still referenced by plot p1, so removal warns about the dangling
  # reference but removes the dataCombined record itself.
  suppressWarnings(removeDataCombined(project, "aciclovir_individual"))
  expect_false(
    "aciclovir_individual" %in% names(project$definitions$dataCombined)
  )

  # The data-combined kind is one-file-per-entry: on save, removing the entry
  # deletes its file (the plotConfiguration row that referenced it is left
  # intact, that is the dangling reference the warning named) and it is gone from
  # a fresh reload.
  saveProject(project)
  expect_false(file.exists(dcFile))
  reloaded <- loadProject(project$info$projectFilePath)
  expect_false(
    "aciclovir_individual" %in% names(reloaded$definitions$dataCombined)
  )
  # The referencing plot survives (lazy referential), so reload still has p1.
  expect_true("p1" %in% names(reloaded$definitions$plots))
})

test_that("removeDataCombined warns and is a no-op on an unknown id", {
  project <- exampleProject()
  before <- project$definitions$dataCombined
  expect_warning(removeDataCombined(project, "ghost"), "not found")
  expect_identical(project$definitions$dataCombined, before)
})

# Print methods ----

test_that("print.Plot renders a single plot configuration", {
  project <- exampleProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$plots[["p1"]]))
})

test_that("print.PlotGrid renders a single plot grid", {
  project <- exampleProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$plotGrids[[
    "individual_diagnostics"
  ]]))
})

test_that("print.DataCombined renders simulated and observed counts", {
  project <- exampleProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$dataCombined[[
    "aciclovir_individual"
  ]]))
})

test_that("classed plot definitions still behave as lists", {
  project <- exampleProject()
  plot <- project$definitions$plots[["p1"]]
  grid <- project$definitions$plotGrids[["individual_diagnostics"]]
  dc <- project$definitions$dataCombined[["aciclovir_individual"]]
  expect_type(plot, "list")
  expect_identical(plot[["plotId"]], "p1")
  expect_type(grid, "list")
  expect_identical(grid[["plotGridId"]], "individual_diagnostics")
  expect_type(dc, "list")
  expect_true(all(c("simulated", "observed") %in% names(dc)))
})
