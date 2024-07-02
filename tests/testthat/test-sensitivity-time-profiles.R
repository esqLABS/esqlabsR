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
  p_list <- sensitivityTimeProfiles(results)

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path profiles",
      fig = suppressWarnings(p_list)
    )
  )
})

# filter data to be plotted -------------------------------------

outputPathsFilter <- "Organism|ArterialBlood|Plasma|Aciclovir"
parameterPathsFilter <- "Aciclovir|Lipophilicity"

test_that("sensitivityTimeProfiles plots are as expected with filters", {
  set.seed(123)
  profile_plot_filtered <- sensitivityTimeProfiles(
    results_multiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "filtered profile",
      fig = suppressWarnings(profile_plot_filtered)
    )
  )
})

# restore old options
options(old)
