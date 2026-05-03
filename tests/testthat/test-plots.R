# Shared fixtures used by addDataCombined tests below.
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
observedDataForSetup <- loadObservedData(testProject())

# Parse / serialize: dataCombined JSON <-> named list ----

test_that(".parseNestedDataCombined re-keys by name and preserves entries", {
  nested <- list(
    list(
      name = "DC1",
      simulated = list(
        list(
          label = "Sim1",
          scenario = "Scenario1",
          path = "Path1",
          group = "Group1"
        )
      ),
      observed = list(
        list(
          label = "Obs1",
          dataSet = "DataSet1",
          group = "Group1",
          xOffsets = 1,
          xOffsetsUnits = "h"
        )
      )
    )
  )

  result <- .parseNestedDataCombined(nested)

  expect_equal(names(result), "DC1")
  expect_length(result$DC1$simulated, 1)
  expect_length(result$DC1$observed, 1)
  expect_equal(result$DC1$simulated[[1]]$label, "Sim1")
  expect_equal(result$DC1$simulated[[1]]$scenario, "Scenario1")
  expect_equal(result$DC1$simulated[[1]]$path, "Path1")
  expect_equal(result$DC1$observed[[1]]$dataSet, "DataSet1")
  expect_equal(result$DC1$observed[[1]]$xOffsets, 1)
  expect_equal(result$DC1$observed[[1]]$xOffsetsUnits, "h")
})

test_that(".parseNestedDataCombined handles empty input", {
  expect_identical(.parseNestedDataCombined(NULL), list())
  expect_identical(.parseNestedDataCombined(list()), list())
})

test_that(".parseNestedDataCombined defaults missing simulated/observed to empty lists", {
  result <- .parseNestedDataCombined(list(
    list(name = "DC1", simulated = list(list(label = "S", scenario = "X", path = "P")))
  ))
  expect_identical(result$DC1$observed, list())
})

test_that(".dataCombinedToNestedJson re-adds the name field from list keys", {
  dc <- list(
    DC1 = list(
      simulated = list(
        list(label = "Sim1", scenario = "Scenario1", path = "Path1")
      ),
      observed = list(
        list(label = "Obs1", dataSet = "DataSet1", xOffsets = 1)
      )
    ),
    DC2 = list(
      simulated = list(list(label = "Sim2", scenario = "Scenario2", path = "Path2")),
      observed = list()
    )
  )

  result <- .dataCombinedToNestedJson(dc)

  expect_length(result, 2)
  expect_equal(result[[1]]$name, "DC1")
  expect_length(result[[1]]$simulated, 1)
  expect_length(result[[1]]$observed, 1)
  expect_equal(result[[1]]$simulated[[1]]$scenario, "Scenario1")
  expect_equal(result[[1]]$observed[[1]]$xOffsets, 1)
  expect_equal(result[[2]]$name, "DC2")
  expect_length(result[[2]]$simulated, 1)
  # Empty observed is omitted from the JSON entry to keep it terse.
  expect_null(result[[2]]$observed)
})

test_that(".dataCombinedToNestedJson handles empty input", {
  expect_identical(.dataCombinedToNestedJson(NULL), list())
  expect_identical(.dataCombinedToNestedJson(list()), list())
})

test_that("dataCombined round-trips: JSON -> named list -> JSON", {
  original <- list(
    list(
      name = "DC1",
      simulated = list(
        list(label = "Sim1", scenario = "Scenario1", path = "Path1", group = "G")
      ),
      observed = list(
        list(label = "Obs1", dataSet = "DataSet1", xOffsets = 1, xOffsetsUnits = "h")
      )
    )
  )

  parsed <- .parseNestedDataCombined(original)
  roundtrip <- .dataCombinedToNestedJson(parsed)

  expect_equal(roundtrip[[1]]$name, original[[1]]$name)
  expect_equal(
    roundtrip[[1]]$simulated[[1]],
    original[[1]]$simulated[[1]]
  )
  expect_equal(
    roundtrip[[1]]$observed[[1]],
    original[[1]]$observed[[1]]
  )
})

# Parse: load-time shape ----

test_that("Project parses dataCombined as a named list and other plots tables as data.frames", {
  project <- testProject()
  expect_type(project$plots$dataCombined, "list")
  expect_false(is.data.frame(project$plots$dataCombined))
  expect_length(project$plots$dataCombined, 2)
  expect_s3_class(project$plots$plotConfiguration, "data.frame")
  expect_equal(nrow(project$plots$plotConfiguration), 4)
})

# Round-trip ----

test_that("saveProject produces round-trip fidelity for plots", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(
    length(project$plots$dataCombined),
    length(pc2$plots$dataCombined)
  )
  expect_equal(
    nrow(project$plots$plotConfiguration),
    nrow(pc2$plots$plotConfiguration)
  )
  expect_equal(nrow(project$plots$plotGrids), nrow(pc2$plots$plotGrids))
})

# Public CRUD: plots ----

test_that("addPlot appends a row and marks the project modified", {
  pc <- testProject()
  pc$.markSaved()
  before <- nrow(pc$plots$plotConfiguration)

  addPlot(
    project = pc,
    plotID = "PNew",
    dataCombinedName = "AciclovirPVB",
    plotType = "individual",
    title = "Hello"
  )

  expect_equal(nrow(pc$plots$plotConfiguration), before + 1L)
  newRow <- pc$plots$plotConfiguration[
    pc$plots$plotConfiguration$plotID == "PNew",
  ]
  expect_equal(newRow$DataCombinedName, "AciclovirPVB")
  expect_equal(newRow$plotType, "individual")
  expect_equal(newRow$title, "Hello")
  expect_true(pc$modified)
})

test_that("addPlot errors on duplicate plotID", {
  pc <- testProject()
  expect_error(
    addPlot(pc, "P1", "AciclovirPVB", "individual"),
    regexp = "already exists"
  )
})

test_that("addPlot errors on unknown DataCombinedName", {
  pc <- testProject()
  expect_error(
    addPlot(pc, "PX", "NoSuchDC", "individual"),
    regexp = "not found"
  )
})

test_that("addPlot errors on unknown plotType", {
  pc <- testProject()
  expect_error(
    addPlot(pc, "PX", "AciclovirPVB", "bogusType"),
    regexp = "plotType"
  )
})

test_that("addPlot keeps optional NULL args as NA columns", {
  pc <- testProject()
  addPlot(
    pc,
    "PNullArg",
    "AciclovirPVB",
    "individual",
    title = NULL,
    subtitle = "set"
  )
  newRow <- pc$plots$plotConfiguration[
    pc$plots$plotConfiguration$plotID == "PNullArg",
  ]
  expect_true("title" %in% names(newRow))
  expect_true(is.na(newRow$title))
  expect_equal(newRow$subtitle, "set")
})

test_that("addPlot R6 method delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addPlot(pc1, "PD1", "AciclovirPVB", "individual")
  pc2$addPlot("PD1", "AciclovirPVB", "individual")
  expect_equal(
    nrow(pc1$plots$plotConfiguration),
    nrow(pc2$plots$plotConfiguration)
  )
})

test_that("addPlot returns project invisibly", {
  pc <- testProject()
  out <- withVisible(addPlot(pc, "PInv", "AciclovirPVB", "individual"))
  expect_false(out$visible)
  expect_identical(out$value, pc)
})

test_that("removePlot drops the row and marks modified", {
  pc <- testProject()
  pc$.markSaved()
  before <- nrow(pc$plots$plotConfiguration)
  # P3 is referenced by plotGrid "Aciclovir"; the dangling-reference warning
  # is asserted in a dedicated test below.
  suppressWarnings(removePlot(pc, "P3"))
  expect_equal(nrow(pc$plots$plotConfiguration), before - 1L)
  expect_false("P3" %in% pc$plots$plotConfiguration$plotID)
  expect_true(pc$modified)
})

test_that("removePlot warns and is a no-op for unknown plotID", {
  pc <- testProject()
  pc$.markSaved()
  expect_warning(
    removePlot(pc, "NoSuchPlot_ZZZ"),
    regexp = "not found"
  )
  expect_false(pc$modified)
})

test_that("removePlot warns when the plot is referenced in a plotGrid", {
  pc <- testProject()
  expect_warning(
    removePlot(pc, "P1"),
    regexp = "referenced"
  )
})

# Public CRUD: plot grids ----

test_that("addPlotGrid joins plotIDs into a comma string and marks modified", {
  pc <- testProject()
  pc$.markSaved()
  before <- nrow(pc$plots$plotGrids)

  addPlotGrid(
    project = pc,
    name = "GridNew",
    plotIDs = c("P1", "P2"),
    title = "T"
  )

  expect_equal(nrow(pc$plots$plotGrids), before + 1L)
  newRow <- pc$plots$plotGrids[pc$plots$plotGrids$name == "GridNew", ]
  expect_equal(newRow$plotIDs, "P1, P2")
  expect_equal(newRow$title, "T")
  expect_true(pc$modified)
})

test_that("addPlotGrid errors on duplicate name", {
  pc <- testProject()
  expect_error(
    addPlotGrid(pc, "Aciclovir", c("P1")),
    regexp = "already exists"
  )
})

test_that("addPlotGrid errors when plotIDs reference unknown plots", {
  pc <- testProject()
  expect_error(
    addPlotGrid(pc, "GridX", c("P1", "NoSuchPlot")),
    regexp = "not found"
  )
})

test_that("addPlotGrid R6 method delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addPlotGrid(pc1, "G1", c("P1"))
  pc2$addPlotGrid("G1", c("P1"))
  expect_equal(nrow(pc1$plots$plotGrids), nrow(pc2$plots$plotGrids))
})

test_that("addPlotGrid returns project invisibly", {
  pc <- testProject()
  out <- withVisible(addPlotGrid(pc, "GInv", c("P1")))
  expect_false(out$visible)
  expect_identical(out$value, pc)
})

test_that("removePlotGrid drops the row and marks modified", {
  pc <- testProject()
  pc$.markSaved()
  before <- nrow(pc$plots$plotGrids)
  removePlotGrid(pc, "Aciclovir2")
  expect_equal(nrow(pc$plots$plotGrids), before - 1L)
  expect_false("Aciclovir2" %in% pc$plots$plotGrids$name)
  expect_true(pc$modified)
})

test_that("removePlotGrid warns and is a no-op for unknown name", {
  pc <- testProject()
  pc$.markSaved()
  expect_warning(
    removePlotGrid(pc, "NoSuchGrid_ZZZ"),
    regexp = "not found"
  )
  expect_false(pc$modified)
})

# Public CRUD: dataCombined ----

test_that("addDataCombined appends a new entry and marks the project modified", {
  pc <- testProject()
  pc$.markSaved()
  beforeCount <- length(pc$plots$dataCombined)
  beforeNames <- names(pc$plots$dataCombined)

  addDataCombined(
    project = pc,
    name = "NewDC",
    simulated = list(
      list(label = "sim1", scenario = "TestScenario", path = outputPaths)
    ),
    observed = list(
      list(label = "obs1", dataSet = names(observedDataForSetup)[[1]])
    )
  )

  expect_equal(length(pc$plots$dataCombined), beforeCount + 1L)
  expect_true("NewDC" %in% names(pc$plots$dataCombined))
  expect_length(pc$plots$dataCombined$NewDC$simulated, 1)
  expect_length(pc$plots$dataCombined$NewDC$observed, 1)
  expect_true(pc$modified)
  expect_false("NewDC" %in% beforeNames)
})

test_that("addDataCombined errors when the DataCombined name already exists", {
  pc <- testProject()
  expect_error(
    addDataCombined(
      project = pc,
      name = "AciclovirPVB",
      simulated = list(
        list(label = "sim1", scenario = "TestScenario", path = outputPaths)
      )
    ),
    regexp = "already exists"
  )
})

test_that("addDataCombined errors when simulated entry misses required fields", {
  pc <- testProject()
  expect_error(
    addDataCombined(
      project = pc,
      name = "BadDC",
      simulated = list(list(label = "x", scenario = "TestScenario"))
    ),
    regexp = "path"
  )
})

test_that("addDataCombined errors when observed entry misses required fields", {
  pc <- testProject()
  expect_error(
    addDataCombined(
      project = pc,
      name = "BadDC2",
      observed = list(list(label = "x"))
    ),
    regexp = "dataSet"
  )
})

test_that("addDataCombined R6 method delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  sim <- list(
    list(label = "s", scenario = "TestScenario", path = outputPaths)
  )
  addDataCombined(pc1, name = "DelegateDC", simulated = sim)
  pc2$addDataCombined(name = "DelegateDC", simulated = sim)
  expect_equal(
    length(pc1$plots$dataCombined),
    length(pc2$plots$dataCombined)
  )
  expect_equal(
    pc1$plots$dataCombined$DelegateDC,
    pc2$plots$dataCombined$DelegateDC
  )
})

test_that("addDataCombined returns project invisibly", {
  pc <- testProject()
  out <- withVisible(
    addDataCombined(
      project = pc,
      name = "InvisDC",
      simulated = list(
        list(label = "s", scenario = "TestScenario", path = outputPaths)
      )
    )
  )
  expect_false(out$visible)
  expect_identical(out$value, pc)
})

test_that("addDataCombined survives a JSON round-trip", {
  pc <- testProject()
  addDataCombined(
    project = pc,
    name = "RoundTripDC",
    simulated = list(
      list(
        label = "s1",
        scenario = "TestScenario",
        path = outputPaths,
        group = "g",
        xOffsets = 2
      )
    ),
    observed = list(
      list(label = "o1", dataSet = names(observedDataForSetup)[[1]])
    )
  )

  tmp <- withr::local_tempfile(fileext = ".json")
  saveProject(pc, path = tmp)
  reloaded <- loadProject(tmp)

  expect_true("RoundTripDC" %in% names(reloaded$plots$dataCombined))
  rt <- reloaded$plots$dataCombined$RoundTripDC
  expect_length(rt$simulated, 1)
  expect_length(rt$observed, 1)
  expect_equal(rt$simulated[[1]]$label, "s1")
  expect_equal(rt$observed[[1]]$label, "o1")
})

test_that("removeDataCombined drops the named DataCombined", {
  pc <- testProject()
  pc$.markSaved()
  beforeCount <- length(pc$plots$dataCombined)
  expect_true("AciclovirPop" %in% names(pc$plots$dataCombined))

  suppressWarnings(removePlot(pc, "P4"))
  removeDataCombined(pc, "AciclovirPop")

  expect_equal(length(pc$plots$dataCombined), beforeCount - 1L)
  expect_false("AciclovirPop" %in% names(pc$plots$dataCombined))
  expect_true(pc$modified)
})

test_that("removeDataCombined warns and is a no-op for unknown name", {
  pc <- testProject()
  pc$.markSaved()
  expect_warning(
    removeDataCombined(pc, "NoSuchDC_ZZZ"),
    regexp = "not found"
  )
  expect_false(pc$modified)
})

test_that("removeDataCombined warns when referenced by plotConfiguration", {
  pc <- testProject()
  expect_warning(
    removeDataCombined(pc, "AciclovirPVB"),
    regexp = "referenced"
  )
})
