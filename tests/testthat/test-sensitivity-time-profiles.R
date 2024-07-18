# save old options
old <- options()

options(
  tibble.width = Inf,
  pillar.min_title_chars = Inf,
  pillar.sigfig = 4,
  digits = 4,
  scipen = 999
)

# single output path -------------------------------------

# run time-consuming simulations just once
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

# validating plotting arguments -------------------------

test_that("sensitivityTimeProfiles fails with incorrect input objects", {
  expect_error(
    sensitivityTimeProfiles("x"),
    "argument 'sensitivityCalculation' is of type 'character', but expected 'SensitivityCalculation'"
  )
})

# default plot -----------------------------------------------

test_that("sensitivityTimeProfiles plots are as expected", {
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

# parameterized plots ---------------------------------

n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("sensitivityTimeProfiles correctly applies linear y-axis scaling", {
  set.seed(123)
  p <- sensitivityTimeProfiles(results, yAxisScale = "lin")

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTimeProfiles linear y-axis",
      fig = p
    )
  )

  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)

  expect_snapshot(unlist(plotParams))
})

test_that("sensitivityTimeProfiles plots correctly with inclusion of observed data", {
  set.seed(123)
  p <- sensitivityTimeProfiles(results, observedData = obsData)

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTimeProfiles works with observed data",
      fig = p
    )
  )
})

# multiple output paths -------------------------------------

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

test_that("sensitivityTimeProfiles plots are as expected for multiple output paths", {
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

test_that("sensitivityTimeProfiles plots correctly for multiple outputs with observed data", {
  set.seed(123)
  plotsMultiple <- sensitivityTimeProfiles(resultsMultiple, observedData = obsData)

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path profiles with observed data",
      fig = plotsMultiple
    )
  )
})


# multiple output paths with multiple observed data
obsData1 <<- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)
obsData2 <<- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)
# create mock observed data with "Amount" dimension
obsDataMultiple <- c(obsData1, obsData2)
names(obsDataMultiple)[2] <- "AciclovirLaskinData.Laskin 1982.Group A - Mock"
obsDataMultiple[[2]]$name <- "AciclovirLaskinData.Laskin 1982.Group A - Mock"
obsDataMultiple[[2]]$yDimension <- "Amount"
obsDataMultiple[[2]]$yUnit <- ospUnits$Amount$µmol

resultsMultiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(0.2, 1, 5, 10)
)

test_that("sensitivityTimeProfiles plots correctly for multiple outputs with multiple observed data", {
  set.seed(123)
  plotsMultiple <- sensitivityTimeProfiles(resultsMultiple,
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

# filter data to be plotted -------------------------------------

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

# restore old options
options(old)
