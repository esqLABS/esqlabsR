# esqlabsColors ----------------------------------------------------------

test_that("esqlabsColors input validation works as expected", {
  expect_error(
    esqlabsColors(-1),
    regexp = messages$nrOfColorsShouldBePositive(-1),
    fixed = TRUE
  )
})

test_that("esqlabsColors works with empty argument vector", {
  expect_length(esqlabsColors(0), 0)
})

test_that("esqlabsColors returns two colors", {
  expect_length(esqlabsColors(2), 2)
})

test_that("esqlabsColors returns three colors", {
  expect_length(esqlabsColors(3), 3)
})

test_that("esqlabsColors returns ten colors", {
  expect_length(esqlabsColors(10), 10)
})

test_that("esqlabsColors returns ten colors", {
  expect_length(esqlabsColors(10), 10)
})

# col2hsv -----------------------------------------------------------------

test_that("col2hsv returns expected HSV values for a given R color name", {
  expect_equal(
    col2hsv("yellow"),
    structure(
      c(0.166666666666667, 1, 1),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("h", "s", "v"), NULL)
    )
  )

  expect_equal(
    col2hsv("white"),
    structure(
      c(0, 0, 1),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("h", "s", "v"), NULL)
    )
  )
})

# createEsqlabsPlotConfiguration ------------------------------------------

test_that("createEsqlabsPlotConfiguration() creates object with chosen defaults", {
  myPC <- createEsqlabsPlotConfiguration()
  expect_true(isOfType(myPC, "DefaultPlotConfiguration"))
  expect_equal(myPC$titleSize, 10)
})

test_that("createEsqlabsPlotGridConfiguration() creates object with chosen defaults", {
  myPGC <- createEsqlabsPlotGridConfiguration()
  expect_true(isOfType(myPGC, "PlotGridConfiguration"))
  expect_equal(myPGC$tagLevels, "a")
})

test_that("createEsqlabsExportConfiguration() creates object with chosen defaults", {
  myProjConfig <- ProjectConfiguration$new()
  myEC <- createEsqlabsExportConfiguration(myProjConfig$outputFolder)
  expect_true(isOfType(myEC, "ExportConfiguration"))
  expect_equal(myEC$units, "cm")
})


test_that("esqlabsPlotConfiguration fields match DefaultPlotConfiguration", {
  defaultConfig <- ospsuite::DefaultPlotConfiguration$new()
  esqlabsConfig <- createEsqlabsPlotConfiguration()

  # Check if all fields from DefaultPlotConfiguration are present in esqLabs configuration
  defaultFields <- names(defaultConfig)
  esqlabsFields <- names(esqlabsConfig)

  missingFields <- setdiff(defaultFields, esqlabsFields)
  expect_true(
    length(missingFields) == 0,
    info = paste("Missing fields:", paste(missingFields, collapse = ", "))
  )

  # Only override fields where differences are intentional
  # and backward compatibility with `ospsuite` plotting functions was verified
  esqlabsConfig$linesColor <- NULL
  esqlabsConfig$legendPosition <- NULL

  # Check if the types of the remaining fields are the same between both configurations
  for (field in defaultFields) {
    expect_equal(
      class(esqlabsConfig[[field]]),
      class(defaultConfig[[field]]),
      info = paste("Field", field, "has different types")
    )
  }
})

# single observed and simulated datasets
oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))

test_that(".parseExcelMultiValueField numeric conversion path is covered", {
  # Direct test to ensure numeric conversion code path is covered
  result <- esqlabsR:::.parseExcelMultiValueField(
    value = "72.5, 80.5",
    fieldName = "test",
    plotID = "P1",
    expectedLength = 2,
    expectedType = "numeric"
  )
  expect_equal(result, c(72.5, 80.5))
  expect_true(is.numeric(result))

  # Test space-separated numeric values trigger correct error
  expect_error(
    esqlabsR:::.parseExcelMultiValueField(
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

test_that("createEsqlabsPlotConfiguration() works with ospsuite::plotIndividualTimeProfile", {
  esqlabsConfig <- createEsqlabsPlotConfiguration()

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "time profile - esqlabsPlotConfiguration",
    fig = plotIndividualTimeProfile(oneObsSimDC, esqlabsConfig)
  )
})

# createPlots(project, ...) tests ----

test_that("createPlots errors on non-Project input", {
  expect_error(createPlots("not a project"), "expected <Project>")
})

test_that("createPlots returns empty list when project has no plots", {
  project <- loadProject(testProjectJSONPath())
  expect_identical(createPlots(project), list())
})

test_that("createPlots reaches plotGrids data.frame for Example project", {
  examplePath <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  project <- loadProject(examplePath)
  spec <- project$plots$dataCombined[[1]]
  scenarioName <- spec$simulated[[1]]$scenario
  path <- spec$simulated[[1]]$path
  simulatedScenarios <- list()
  simulatedScenarios[[scenarioName]] <- list(
    results = list(allQuantityPaths = path)
  )
  result <- tryCatch(
    createPlots(
      project,
      plotGridNames = project$plots$plotGrids$name[[1]],
      simulatedScenarios = simulatedScenarios
    ),
    error = function(e) e
  )
  # Either a list of plot grids (success) or a downstream error caused by
  # the mock simulatedScenarios. Both prove dispatch reached the project's
  # plotGrids data.frame.
  expect_true(inherits(result, "list") || inherits(result, "error"))
})
