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

# validating plotting arguments -------------------------

test_that("sensitivityTimeProfiles fails with incorrect input objects", {
  expect_error(
    sensitivityTimeProfiles("x"),
    "argument 'sensitivityCalculation' is of type 'character', but expected 'SensitivityCalculation'"
  )
})

# default plots ---------------------------------------

test_that("sensitivityTimeProfiles plots are as expected", {
  set.seed(123)
  p <- suppressWarnings(sensitivityTimeProfiles(results))

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivityTimeProfiles works as expected",
    fig = suppressWarnings(p)
  )

  pb <- suppressWarnings(ggplot_build(p$`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`))

  expect_snapshot(pb$plot$labels)
})

# saving plots: default ---------------------------------------

test_that("sensitivityTimeProfiles saves plot file", {
  path <- "Profile_OutputPath1.png"

  p <- suppressWarnings(sensitivityTimeProfiles(results, savePlots = TRUE))

  expect_true(file.exists(path))

  on.exit(unlink(path))
})

# saving plots: folder ---------------------------------------

test_that("sensitivityTimeProfiles saves plot file to a specified folder", {
  dir.create("newFolder")
  path <- "newFolder/Profile_OutputPath1.png"

  p <- suppressWarnings(sensitivityTimeProfiles(results, outputFolder = "newFolder/", savePlots = TRUE))

  expect_true(file.exists(path))

  on.exit(unlink("newFolder", recursive = TRUE))
})

# multiple output paths -------------------------------------

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
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

results_multiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(1, 5, 10)
)

test_that("sensitivityTimeProfiles plots are as expected for multiple output paths", {
  set.seed(123)
  p_list <- suppressWarnings(sensitivityTimeProfiles(results))

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple output path profiles",
    fig = suppressWarnings(p_list)
  )
})

test_that("sensitivityTimeProfiles saves plot files for multiple output paths", {
  path1 <- "Profile_OutputPath1.png"
  path2 <- "Profile_OutputPath2.png"
  path3 <- "Profile_OutputPath3.png"

  p <- suppressWarnings(sensitivityTimeProfiles(results_multiple, savePlots = TRUE))

  expect_true(file.exists(path1))
  expect_true(file.exists(path2))
  expect_true(file.exists(path3))

  on.exit(unlink(c(path1, path2, path3)))
})

# filter data to be plotted -------------------------------------

outputPathsFilter <- "Organism|ArterialBlood|Plasma|Aciclovir"
parameterPathsFilter <- "Aciclovir|Lipophilicity"
pkParametersFilter <- c("C_max", "t_max")

test_that("sensitivityTimeProfiles plots are as expected with filters", {
  set.seed(123)
  profile_plot_filtered <- sensitivityTimeProfiles(
    results_multiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "filtered profile",
    fig = suppressWarnings(profile_plot_filtered)
  )
})

# restore old options
options(old)
