test_that(".parseNestedDataCombined re-keys by name and drops the name field", {
  raw <- list(
    list(name = "DC1", simulated = list(list(label = "a")), observed = list()),
    list(name = "DC2", simulated = list(), observed = list(list(label = "b")))
  )
  parsed <- esqlabsR:::.parseNestedDataCombined(raw)
  expect_named(parsed, c("DC1", "DC2"))
  expect_named(parsed$DC1, c("simulated", "observed"))
  expect_equal(parsed$DC1$simulated[[1]]$label, "a")
  expect_equal(parsed$DC2$observed[[1]]$label, "b")
})

test_that(".parseNestedDataCombined returns empty list for NULL or empty input", {
  expect_identical(esqlabsR:::.parseNestedDataCombined(NULL), list())
  expect_identical(esqlabsR:::.parseNestedDataCombined(list()), list())
})

test_that(".listOfListsToDataFrame pads missing fields with NA", {
  raw <- list(
    list(plotID = "P1", plotType = "individual", title = "T1"),
    list(plotID = "P2", plotType = "population")
  )
  df <- esqlabsR:::.listOfListsToDataFrame(raw)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_setequal(names(df), c("plotID", "plotType", "title"))
  expect_true(is.na(df$title[df$plotID == "P2"]))
})

test_that(".listOfListsToDataFrame returns empty data.frame for NULL or empty", {
  expect_equal(nrow(esqlabsR:::.listOfListsToDataFrame(NULL)), 0L)
  expect_equal(nrow(esqlabsR:::.listOfListsToDataFrame(list())), 0L)
})

test_that("addPlotGrid aborts when no plots are defined", {
  project <- testProject()
  project$plots <- list(
    dataCombined = list(),
    plotConfiguration = data.frame(),
    plotGrids = data.frame()
  )
  expect_snapshot(
    error = TRUE,
    addPlotGrid(project, "G1", plotIds = "MissingPlot")
  )
})

test_that("addPlot stores a vector-valued field as a single row", {
  project <- exampleProject()
  addDataCombined(
    project,
    "DC_vec",
    simulated = list(list(
      label = "sim",
      scenario = "Aciclovir_iv_population",
      path = project$outputPaths$Aciclovir_PVB,
      group = "g"
    ))
  )
  addPlot(
    project,
    "P_vec",
    "DC_vec",
    "population",
    quantiles = c(0.05, 0.5, 0.95)
  )

  cfg <- project$plots$plotConfiguration
  # One new row, no recycling into three, and no mangled column name. The
  # vector is stored as a comma-separated string to match the parser shape
  # and the dispatcher's `strsplit(quantiles, ",")`.
  expect_equal(sum(cfg$plotID == "P_vec"), 1L)
  expect_true("quantiles" %in% names(cfg))
  expect_equal(
    cfg$quantiles[[which(cfg$plotID == "P_vec")]],
    "0.05, 0.5, 0.95"
  )
})
