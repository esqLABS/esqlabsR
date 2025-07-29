# Save old options
old_opts <- options()

options(
  tibble.width = Inf,
  pillar.min_title_chars = Inf,
  pillar.sigfig = 4,
  digits = 4,
  scipen = 999
)

# Single output path ------------------------------------------------------

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)
variationRange <- c(0.1, 2, 20) # 1.0 is deliberately left out for testing

set.seed(123)
results <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = variationRange
)

# load observed data
filePath <- testthat::test_path("../data/AciclovirLaskinData.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
obsData <<- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

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
  names(parameterPaths) <- c("Lipophilicity", "Dose", "GFR fraction")

  resultsLab <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
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

test_that("sensitivityTimeProfiles errors for non-list units", {
  expect_error(
    sensitivityTimeProfiles(results, xUnits = "h"),
    regexp = messages$errorWrongType("xUnits", "character", "list"),
    fixed = TRUE
  )
  expect_error(
    sensitivityTimeProfiles(results, yUnits = "mol/l"),
    regexp = messages$errorWrongType("yUnits", "character", "list"),
    fixed = TRUE
  )
})

test_that("sensitivityTimeProfiles errors on invalid units", {
  # invalid unit
  expect_error(
    sensitivityTimeProfiles(results, yUnits = list("mol/kg")),
    regexp = "is not in defined enumeration values"
  )
  # invalid x-axis unit
  expect_error(
    sensitivityTimeProfiles(results, xUnits = list("mol/l")),
    regexp = "is not in defined enumeration values"
  )
})

test_that("sensitivityTimeProfiles applies unit conversion", {
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
  p1 <- sensitivityTimeProfiles(results) # default
  p2 <- sensitivityTimeProfiles(results, yUnits = list("mol")) # no conversion

  expect_identical(
    extractAxisRange(p1),
    extractAxisRange(p2)
  )
})

# Multiple output paths ---------------------------------------------------

outputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Age",
  "Organism|ArterialBlood|Plasma|Aciclovir"
)
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)

resultsMultiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(0.2, 1, 5, 10)
)

test_that("sensitivityTimeProfiles plots are correct for multiple output paths", {
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
  p <- sensitivityTimeProfiles(
    resultsMultiple,
    yUnits = list("mol/l", "month(s)", "nmol")
  )

  expect_snapshot(extractAxisRange(p))
})

test_that("sensitivityTimeProfiles handles y-unit conversion with `NULL` for multiple paths", {
  p <- sensitivityTimeProfiles(
    resultsMultiple,
    yUnits = list("mol/l", NULL, "mol")
  )

  expect_snapshot(extractAxisRange(p))
})

test_that("sensitivityTimeProfiles applies y-unit conversion with a single unit for multiple paths", {
  p1 <- sensitivityTimeProfiles(resultsMultiple, yUnits = list("mg/ml"))
  p2 <- sensitivityTimeProfiles(resultsMultiple, yUnits = list("mg/ml", NULL))

  expect_snapshot(extractAxisRange(p1))
  expect_identical(
    extractAxisRange(p1),
    extractAxisRange(p2)
  )
})

test_that("sensitivityTimeProfiles handles non-convertible y-units for multiple paths", {
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
obsData1 <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)
obsData2 <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

obsDataMultiple <- c(obsData1, obsData2)
# Rename one of the data sets and shift its values
names(obsDataMultiple)[2] <- "AciclovirLaskinData.Laskin 1982.Group A - Mock"
obsDataMultiple[[2]]$name <- "AciclovirLaskinData.Laskin 1982.Group A - Mock"
obsDataMultiple[[2]]$addMetaData("Study Id", "Laskin 1982.Group A - Mock")
obsDataMultiple[[2]]$setValues(
  obsDataMultiple[[2]]$xValues,
  obsDataMultiple[[2]]$yValues + 0.1
)

test_that("sensitivityTimeProfiles works with multiple observed data with same dimension", {
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

# create mock observed data with "Amount" dimension
obsDataMultiple[[2]]$yDimension <- "Amount"
obsDataMultiple[[2]]$yUnit <- ospUnits$Amount$µmol

test_that("sensitivityTimeProfiles works with multiple observed data with different dimensions", {
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

# Restore old options
on.exit(options(old_opts), add = TRUE)
