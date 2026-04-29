# Shared fixtures used by addDataCombined tests below.
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
observedDataForSetup <- loadObservedData(testProject())

# Parse / serialize: dataCombined nested-JSON <-> flat data.frame ----

test_that(".parseNestedDataCombined converts nested structure to flat data.frame", {
  nested <- list(
    list(
      name = "DC1",
      simulated = list(
        list(
          label = "Sim1",
          scenario = "Scenario1",
          path = "Path1",
          group = "Group1",
          xOffsets = NULL,
          xOffsetsUnits = NULL,
          yOffsets = NULL,
          yOffsetsUnits = NULL,
          xScaleFactors = NULL,
          yScaleFactors = NULL
        )
      ),
      observed = list(
        list(
          label = "Obs1",
          dataSet = "DataSet1",
          group = "Group1",
          xOffsets = 1,
          xOffsetsUnits = "h",
          yOffsets = NULL,
          yOffsetsUnits = NULL,
          xScaleFactors = NULL,
          yScaleFactors = NULL
        )
      )
    )
  )

  result <- .parseNestedDataCombined(nested)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$DataCombinedName, c("DC1", "DC1"))
  expect_equal(result$dataType, c("simulated", "observed"))
  expect_equal(result$label, c("Sim1", "Obs1"))
  expect_equal(result$scenario, c("Scenario1", NA))
  expect_equal(result$path, c("Path1", NA))
  expect_equal(result$dataSet, c(NA, "DataSet1"))
  expect_equal(result$xOffsets, c(NA, 1))
  expect_equal(result$xOffsetsUnits, c(NA, "h"))
})

test_that(".parseNestedDataCombined handles empty input", {
  expect_equal(nrow(.parseNestedDataCombined(NULL)), 0)
  expect_equal(nrow(.parseNestedDataCombined(list())), 0)
})

test_that(".dataCombinedToNestedJson converts flat data.frame to nested structure", {
  df <- data.frame(
    DataCombinedName = c("DC1", "DC1", "DC2"),
    dataType = c("simulated", "observed", "simulated"),
    label = c("Sim1", "Obs1", "Sim2"),
    scenario = c("Scenario1", NA, "Scenario2"),
    path = c("Path1", NA, "Path2"),
    dataSet = c(NA, "DataSet1", NA),
    group = c("Group1", "Group1", "Group2"),
    xOffsets = c(NA, 1, NA),
    xOffsetsUnits = c(NA, "h", NA),
    yOffsets = c(NA, NA, NA),
    yOffsetsUnits = c(NA, NA, NA),
    xScaleFactors = c(NA, NA, NA),
    yScaleFactors = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  result <- .dataCombinedToNestedJson(df)

  expect_length(result, 2)
  expect_equal(result[[1]]$name, "DC1")
  expect_length(result[[1]]$simulated, 1)
  expect_length(result[[1]]$observed, 1)
  expect_equal(result[[1]]$simulated[[1]]$label, "Sim1")
  expect_equal(result[[1]]$simulated[[1]]$scenario, "Scenario1")
  expect_equal(result[[1]]$observed[[1]]$dataSet, "DataSet1")
  expect_equal(result[[1]]$observed[[1]]$xOffsets, 1)
  expect_equal(result[[2]]$name, "DC2")
  expect_length(result[[2]]$simulated, 1)
  expect_length(result[[2]]$observed, 0)
})

test_that(".dataCombinedToNestedJson handles empty input", {
  expect_equal(.dataCombinedToNestedJson(NULL), list())
  expect_equal(.dataCombinedToNestedJson(data.frame()), list())
})

test_that("dataCombined round-trip: nested JSON -> flat df -> nested JSON", {
  original <- list(
    list(
      name = "DC1",
      simulated = list(
        list(
          label = "Sim1",
          scenario = "Scenario1",
          path = "Path1",
          group = "Group1",
          xOffsets = NULL,
          xOffsetsUnits = NULL,
          yOffsets = NULL,
          yOffsetsUnits = NULL,
          xScaleFactors = NULL,
          yScaleFactors = NULL
        )
      ),
      observed = list(
        list(
          label = "Obs1",
          dataSet = "DataSet1",
          group = "Group1",
          xOffsets = 1,
          xOffsetsUnits = "h",
          yOffsets = NULL,
          yOffsetsUnits = NULL,
          xScaleFactors = NULL,
          yScaleFactors = NULL
        )
      )
    )
  )

  flat <- .parseNestedDataCombined(original)
  roundtrip <- .dataCombinedToNestedJson(flat)

  expect_equal(roundtrip[[1]]$name, original[[1]]$name)
  expect_equal(
    roundtrip[[1]]$simulated[[1]]$label,
    original[[1]]$simulated[[1]]$label
  )
  expect_equal(
    roundtrip[[1]]$simulated[[1]]$scenario,
    original[[1]]$simulated[[1]]$scenario
  )
  expect_equal(
    roundtrip[[1]]$observed[[1]]$dataSet,
    original[[1]]$observed[[1]]$dataSet
  )
  expect_equal(
    roundtrip[[1]]$observed[[1]]$xOffsets,
    original[[1]]$observed[[1]]$xOffsets
  )
})

# Parse: load-time shape ----

test_that("Project parses plots into data.frames", {
  project <- testProject()
  expect_s3_class(project$plots$dataCombined, "data.frame")
  expect_equal(nrow(project$plots$dataCombined), 4)
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

  expect_equal(nrow(project$plots$dataCombined), nrow(pc2$plots$dataCombined))
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

test_that("addDataCombined appends rows and marks the project modified", {
  pc <- testProject()
  pc$.markSaved()
  beforeRows <- nrow(pc$plots$dataCombined)
  beforeNames <- unique(pc$plots$dataCombined$DataCombinedName)

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

  expect_equal(nrow(pc$plots$dataCombined), beforeRows + 2L)
  expect_true("NewDC" %in% pc$plots$dataCombined$DataCombinedName)
  expect_setequal(
    pc$plots$dataCombined$dataType[
      pc$plots$dataCombined$DataCombinedName == "NewDC"
    ],
    c("simulated", "observed")
  )
  expect_true(pc$modified)
  expect_false(any(beforeNames == "NewDC"))
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
    nrow(pc1$plots$dataCombined),
    nrow(pc2$plots$dataCombined)
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

  expect_true(
    "RoundTripDC" %in% reloaded$plots$dataCombined$DataCombinedName
  )
  rtRows <- reloaded$plots$dataCombined[
    reloaded$plots$dataCombined$DataCombinedName == "RoundTripDC",
  ]
  expect_setequal(rtRows$dataType, c("simulated", "observed"))
  expect_setequal(rtRows$label, c("s1", "o1"))
})

test_that("removeDataCombined drops all rows for the named DataCombined", {
  pc <- testProject()
  pc$.markSaved()
  beforeRows <- nrow(pc$plots$dataCombined)
  removedRows <- sum(pc$plots$dataCombined$DataCombinedName == "AciclovirPop")
  expect_gt(removedRows, 0L)

  suppressWarnings(removePlot(pc, "P4"))
  removeDataCombined(pc, "AciclovirPop")

  expect_equal(nrow(pc$plots$dataCombined), beforeRows - removedRows)
  expect_false("AciclovirPop" %in% pc$plots$dataCombined$DataCombinedName)
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
