# The numeric-snapshot blocks below capture axis ranges and labels whose
# formatting depends on print/precision options. Each such block sets them
# locally with `withr::local_options()` (see `.localSnapshotOptions()`), so the
# state is scoped to the test rather than leaked at file scope.
.localSnapshotOptions <- function(.local_envir = parent.frame()) {
  withr::local_options(
    tibble.width = Inf,
    pillar.min_title_chars = Inf,
    pillar.sigfig = 4,
    digits = 4,
    scipen = 999,
    .local_envir = .local_envir
  )
}

# Single output path ------------------------------------------------------

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
)
variationRange <- c(0.1, 2, 20) # 1.0 is deliberately left out for testing

# Both `loadSimulation()` and the Excel observed-data importer initialize a
# PK-Sim native session; running them at file source time (as `test_dir()`
# sources every test file up front) bleeds native state across files. Defer them
# behind memoized accessors so the native loads and the baseline
# `sensitivityCalculation()` happen inside a `test_that()` block on first use,
# computed once and cached for the rest of the file.
sensFixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      simulation <- loadSimulation(simPath)
      set.seed(123)
      results <- sensitivityCalculation(
        simulation = simulation,
        outputPaths = outputPaths,
        parameterPaths = parameterPaths,
        variationRange = variationRange
      )
      cache <<- list(simulation = simulation, results = results)
    }
    cache
  }
})

sensFixtureMultiple <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      simulation <- sensFixture()$simulation
      outputPathsMultiple <- c(
        "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
        "Organism|Age",
        "Organism|ArterialBlood|Plasma|Aciclovir"
      )
      parameterPathsMultiple <- c(
        "Aciclovir|Lipophilicity",
        "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
        "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
      )
      resultsMultiple <- sensitivityCalculation(
        simulation = simulation,
        outputPaths = outputPathsMultiple,
        parameterPaths = parameterPathsMultiple,
        variationRange = c(0.2, 1, 5, 10)
      )
      cache <<- list(resultsMultiple = resultsMultiple)
    }
    cache
  }
})

# Observed-data accessors. `loadDataSetsFromExcel()` initializes the native data
# importer, so it is deferred here as well. `obsDataFixture()` returns the
# memoized single data set; `loadObsData()` returns a fresh load (used to build
# the mutated multi-data-set fixtures inside the relevant test blocks).
loadObsData <- local({
  config <- NULL
  function() {
    if (is.null(config$filePath)) {
      filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
      dataConfiguration <- createImporterConfigurationForFile(
        filePath = filePath
      )
      dataConfiguration$sheets <- "Laskin 1982.Group A"
      dataConfiguration$namingPattern <- "{Source}.{Sheet}"
      config <<- list(
        filePath = filePath,
        dataConfiguration = dataConfiguration
      )
    }
    loadDataSetsFromExcel(
      xlsFilePath = config$filePath,
      importerConfigurationOrPath = config$dataConfiguration
    )
  }
})

obsDataFixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- loadObsData()
    }
    cache
  }
})

# Build the two-element observed-data set used by the multi-data-set tests, with
# its second element renamed and shifted (mirrors the original setup).
buildObsDataMultiple <- function() {
  obsDataMultiple <- c(loadObsData(), loadObsData())
  names(obsDataMultiple)[2] <- "AciclovirLaskinData.Laskin 1982.Group A - Mock"
  obsDataMultiple[[2]]$name <- "AciclovirLaskinData.Laskin 1982.Group A - Mock"
  obsDataMultiple[[2]]$addMetaData("Study Id", "Laskin 1982.Group A - Mock")
  obsDataMultiple[[2]]$setValues(
    obsDataMultiple[[2]]$xValues,
    obsDataMultiple[[2]]$yValues + 0.1
  )
  obsDataMultiple
}

# Validate plotting arguments ---------------------------------------------

test_that("sensitivityTimeProfiles fails with invalid input", {
  expect_error(
    sensitivityTimeProfiles("x"),
    regexp = messages$errorWrongType(
      "sensitivityCalculation",
      "character",
      "SensitivityCalculation"
    ),
    fixed = TRUE
  )
})

# Default plot ------------------------------------------------------------

test_that("sensitivityTimeProfiles creates expected default plot", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  set.seed(123)
  p <- sensitivityTimeProfiles(results)

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTimeProfiles works as expected",
      fig = p
    )
  )

  pb <- suppressWarnings(
    ggplot_build(
      p$`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`
    )
  )
  expect_snapshot(pb$plot$labels)
})

test_that("sensitivityTimeProfiles applies user-defined parameter labels", {
  simulation <- sensFixture()$simulation
  # Work on a labelled local copy so the file-scope `parameterPaths` is never
  # mutated for other tests in this file.
  namedParameterPaths <- parameterPaths
  names(namedParameterPaths) <- c("Lipophilicity", "Dose", "GFR fraction")

  resultsLab <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = namedParameterPaths,
    variationRange = variationRange
  )

  p <- sensitivityTimeProfiles(resultsLab)

  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTimeProfiles works as expected - user labels",
      fig = p
    )
  )
})

# Parameterized plots -----------------------------------------------------

n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("sensitivityTimeProfiles applies linear y-axis scaling correctly", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  set.seed(123)
  p <- sensitivityTimeProfiles(results, yAxisScale = "lin")

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTimeProfiles linear y-axis",
      fig = p
    )
  )

  expect_snapshot(extractAxisRange(p))
})

test_that("sensitivityTimeProfiles works with observed data", {
  results <- sensFixture()$results
  obsData <- obsDataFixture()
  set.seed(123)
  p <- sensitivityTimeProfiles(results, observedData = obsData[[1]])

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTimeProfiles works with observed data",
      fig = p
    )
  )
})

# Unit conversion ---------------------------------------------------------

test_that("sensitivityTimeProfiles accepts non-list units", {
  results <- sensFixture()$results
  # x-axis units: scalar vs list should result in the same axis range
  set.seed(123)
  p_x_list <- sensitivityTimeProfiles(results, xUnits = list("h"))
  set.seed(123)
  p_x_scalar <- sensitivityTimeProfiles(results, xUnits = "h")

  x_range_list <- extractAxisRange(p_x_list)$x
  x_range_scalar <- extractAxisRange(p_x_scalar)$x
  expect_equal(x_range_scalar, x_range_list)

  # y-axis units: scalar vs list should result in the same axis range
  set.seed(123)
  p_y_list <- sensitivityTimeProfiles(results, yUnits = list("mol/l"))
  set.seed(123)
  p_y_scalar <- sensitivityTimeProfiles(results, yUnits = "mol/l")

  y_range_list <- extractAxisRange(p_y_list)$y
  y_range_scalar <- extractAxisRange(p_y_scalar)$y
  expect_equal(y_range_scalar, y_range_list)
})

test_that("sensitivityTimeProfiles errors on invalid units", {
  results <- sensFixture()$results
  # invalid unit (list form)
  expect_error(
    sensitivityTimeProfiles(results, yUnits = list("mol/kg")),
    regexp = "is not a valid"
  )
  # invalid x-axis unit (list form)
  expect_error(
    sensitivityTimeProfiles(results, xUnits = list("mol/l")),
    regexp = "is not a valid"
  )
  # invalid scalar y-units: incompatible dimension
  expect_error(
    sensitivityTimeProfiles(results, yUnits = "mol/kg"),
    regexp = "is not a valid"
  )
  # invalid scalar y-units: unrecognised string
  expect_error(
    sensitivityTimeProfiles(results, yUnits = "invalid"),
    regexp = "is not a valid"
  )
  # invalid scalar x-units: numeric value
  expect_error(
    sensitivityTimeProfiles(results, xUnits = 52),
    regexp = "is not a valid"
  )
  # invalid scalar x-units: logical value
  expect_error(
    sensitivityTimeProfiles(results, xUnits = TRUE),
    regexp = "is not a valid"
  )
})

test_that("sensitivityTimeProfiles applies unit conversion", {
  results <- sensFixture()$results
  set.seed(123)
  p <- sensitivityTimeProfiles(
    results,
    xUnits = list("h"),
    yUnits = list("mol/l")
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTimeProfiles unit conversion",
      fig = p
    )
  )
})

test_that("sensitivityTimeProfiles handles non-convertible y-units", {
  results <- sensFixture()$results
  p1 <- sensitivityTimeProfiles(results) # default
  p2 <- sensitivityTimeProfiles(results, yUnits = list("mol")) # no conversion

  expect_identical(
    extractAxisRange(p1),
    extractAxisRange(p2)
  )
})

# Multiple output paths ---------------------------------------------------

test_that("sensitivityTimeProfiles plots are correct for multiple output paths", {
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  set.seed(123)
  plotsMultiple <- sensitivityTimeProfiles(resultsMultiple)

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path profiles",
      fig = plotsMultiple
    )
  )
})

test_that("sensitivityTimeProfiles works with multiple outputs and observed data", {
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  obsData <- obsDataFixture()
  set.seed(123)
  plotsMultiple <- sensitivityTimeProfiles(
    resultsMultiple,
    observedData = obsData
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path profiles with observed data",
      fig = plotsMultiple
    )
  )
})

# multiple output paths unit conversion

test_that("sensitivityTimeProfiles applies y-unit conversion for multiple paths", {
  .localSnapshotOptions()
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  p <- sensitivityTimeProfiles(
    resultsMultiple,
    yUnits = list("mol/l", "month(s)", "nmol")
  )

  expect_snapshot(extractAxisRange(p))
})

test_that("sensitivityTimeProfiles handles y-unit conversion with `NULL` for multiple paths", {
  .localSnapshotOptions()
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  p <- sensitivityTimeProfiles(
    resultsMultiple,
    yUnits = list("mol/l", NULL, "mol")
  )

  expect_snapshot(extractAxisRange(p))
})

test_that("sensitivityTimeProfiles applies y-unit conversion with a single unit for multiple paths", {
  .localSnapshotOptions()
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  p1 <- sensitivityTimeProfiles(resultsMultiple, yUnits = list("mg/ml"))
  p2 <- sensitivityTimeProfiles(resultsMultiple, yUnits = list("mg/ml", NULL))

  expect_snapshot(extractAxisRange(p1))
  expect_identical(
    extractAxisRange(p1),
    extractAxisRange(p2)
  )
})

test_that("sensitivityTimeProfiles handles non-convertible y-units for multiple paths", {
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  p1 <- sensitivityTimeProfiles(resultsMultiple) # default
  p2 <- sensitivityTimeProfiles(
    resultsMultiple, # not converted: all wrong units
    yUnits = list("mol", "kg", "µmol/h")
  )
  p3 <- sensitivityTimeProfiles(
    resultsMultiple, # not converted: correct unit wrong path
    yUnits = list("mol", "mol")
  )

  expect_identical(
    extractAxisRange(p1),
    extractAxisRange(p2)
  )
  expect_identical(
    extractAxisRange(p1),
    extractAxisRange(p3)
  )
})

# multiple output paths with multiple observed data

test_that("sensitivityTimeProfiles works with multiple observed data with same dimension", {
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  obsDataMultiple <- buildObsDataMultiple()
  set.seed(123)
  plotsMultiple <- sensitivityTimeProfiles(
    resultsMultiple,
    observedData = obsDataMultiple
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path profiles with 2 observed data same dimension - concentration",
      fig = plotsMultiple[[3]]
    )
  )
})

test_that("sensitivityTimeProfiles works with multiple observed data with different dimensions", {
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  obsDataMultiple <- buildObsDataMultiple()
  # create mock observed data with "Amount" dimension
  obsDataMultiple[[2]]$yDimension <- "Amount"
  obsDataMultiple[[2]]$yUnit <- ospUnits$Amount$µmol

  set.seed(123)
  plotsMultiple <- sensitivityTimeProfiles(
    resultsMultiple,
    observedData = obsDataMultiple
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path profiles with 2 observed data - amount",
      fig = plotsMultiple[[2]]
    )
  )
  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path profiles with 2 observed data - concentration",
      fig = plotsMultiple[[3]]
    )
  )
})

# Filter data to be plotted -----------------------------------------------

outputPathsFilter <- "Organism|ArterialBlood|Plasma|Aciclovir"
parameterPathsFilter <- "Aciclovir|Lipophilicity"

test_that("sensitivityTimeProfiles plots are as expected with filters", {
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  set.seed(123)
  plotFiltered <- sensitivityTimeProfiles(
    resultsMultiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "filtered profile",
      fig = plotFiltered
    )
  )
})
