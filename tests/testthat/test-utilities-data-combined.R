projectConfiguration <- testProjectConfiguration()

# Define which scenarios to run
scenarioNames <- c("TestScenario", "PopulationScenario")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# Create `ScenarioConfiguration` objects from excel files
scenarioConfigurations <- readScenarioConfigurationFromExcel(
  scenarioNames = scenarioNames,
  projectConfiguration = projectConfiguration
)

# Set output paths for each scenario
for (scenarioConfiguration in scenarioConfigurations) {
  scenarioConfiguration$outputPaths <- outputPaths
}

# Run scenarios
scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

simulatedScenarios <- runScenarios(
  scenarios = scenarios
)

importerConfiguration <- ospsuite::loadDataImporterConfiguration(
  configurationFilePath = projectConfiguration$dataImporterConfigurationFile
)

# Load observed data
dataSheets <- "Laskin 1982.Group A"
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets,
  importerConfiguration = importerConfiguration
)

# Correct data combined
dataCombinedDf <- data.frame(list(
  "DataCombinedName" = c("AciclovirPVB", "AciclovirPVB"),
  "dataType" = c("simulated", "observed"),
  "label" = c("Aciclovir simulated", "Aciclovir observed"),
  "scenario" = c(scenarioNames[1], NA),
  "path" = c(outputPaths, NA),
  "dataSet" = c(NA, names(observedData)),
  "group" = c("Aciclovir PVB", "Aciclovir PVB"),
  "xOffsets" = c(NA, NA),
  "xOffsetsUnits" = c(NA, NA),
  "yOffsets" = c(NA, NA),
  "yOffsetsUnits" = c(NA, NA),
  "xScaleFactors" = c(NA, NA),
  "yScaleFactors" = c(NA, NA)
))

# Data combined with a missing path
dataCombinedDf_missingPath <- data.frame(list(
  "DataCombinedName" = c("AciclovirPVB", "AciclovirPVB", "DC_missingPath", "DC_missingPath"),
  "dataType" = c("simulated", "observed", "simulated", "observed"),
  "label" = c("Aciclovir simulated", "Aciclovir observed", "Aciclovir simulated", "Aciclovir observed"),
  "scenario" = c(scenarioNames[1], NA, scenarioNames[1], NA),
  "path" = c(outputPaths, NA, NA, NA),
  "dataSet" = c(NA, names(observedData), NA, names(observedData)),
  "group" = c("Aciclovir PVB", "Aciclovir PVB"),
  "xOffsets" = c(NA, NA),
  "xOffsetsUnits" = c(NA, NA),
  "yOffsets" = c(NA, NA),
  "yOffsetsUnits" = c(NA, NA),
  "xScaleFactors" = c(NA, NA),
  "yScaleFactors" = c(NA, NA)
))

plotConfigurationDf <- data.frame(list(
  "plotID" = "P1",
  "DataCombinedName" = "AciclovirPVB",
  "plotType" = "individual",
  "title" = NA,
  "xUnit" = NA,
  "yUnit" = NA,
  "xAxisScale" = NA,
  "yAxisScale" = NA,
  "xValuesLimits" = NA,
  "yValuesLimits" = NA,
  "quantiles" = NA,
  "nsd" = NA,
  "foldDistance" = NA
))

plotGridsDf <- data.frame(list(
  "name" = "Aciclovir",
  "plotIDs" = "P1",
  "title" = "Aciclovir PVB"
))

test_that("It throws an error with correct names of data combined when a path is not specified for one simulated scenario", {
  expect_error(.validateDataCombinedFromExcel(dataCombinedDf_missingPath, observedData), regexp = messages$stopNoPathProvided("DC_missingPath"))
})

test_that("It throws an error when no data is available and stopIfNotFound = TRUE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- data.frame(
        plotGridName = c(NA),
        outputName = c(NA)
      )
      .writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )
      expect_error(
        createDataCombinedFromExcel(
          simulatedScenarios = simulatedScenarios,
          dataCombinedNames = "AciclovirPVB",
          projectConfiguration = projectConfiguration
        ),
        regexp = messages$stopInvalidDataSetName("Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_")
      )
    }
  )
})

test_that("It throws an error when no simulated results are available and stopIfNotFound = TRUE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- data.frame(
        plotGridName = c(NA),
        outputName = c(NA)
      )
      .writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )
      expect_error(
        createDataCombinedFromExcel(
          observedData = observedData,
          dataCombinedNames = "AciclovirPVB",
          projectConfiguration = projectConfiguration
        ),
        regexp = messages$warningInvalidScenarioName("TestScenario")
      )
    }
  )
})

test_that("It returns an empty DataCombined when no data is available and stopIfNotFound = FALSE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- data.frame(
        plotGridName = c(NA),
        outputName = c(NA)
      )
      .writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )
      # Warnings are suppressed because they are expected but not relevant for
      # this test.
      suppressWarnings({
        dataCombined <- createDataCombinedFromExcel(
          projectConfiguration = projectConfiguration,
          dataCombinedNames = "AciclovirPVB",
          stopIfNotFound = FALSE
        )
      })


      expect_equal(dataCombined[[1]], DataCombined$new())
    }
  )
})

test_that("It returns an empty list when no DataCombined or plotGridNames are specified", {
  dataCombined <- createDataCombinedFromExcel(
    projectConfiguration = projectConfiguration
  )

  expect_equal(dataCombined, list())
})

test_that("It creates correct DataCombined when only data combined names are provided", {
  dataCombined <- createDataCombinedFromExcel(
    projectConfiguration = projectConfiguration,
    dataCombinedNames = "AciclovirPop",
    simulatedScenarios = simulatedScenarios, ,
    observedData = observedData
  )

  expect_true(ospsuite.utils::isOfType(dataCombined, "DataCombined"))
  expect_equal(names(dataCombined)[[1]], "AciclovirPop")
})

test_that("It creates correct DataCombined when only plotGridNames are provided", {
  dataCombined <- createDataCombinedFromExcel(
    projectConfiguration = projectConfiguration,
    plotGridNames = "Aciclovir3",
    simulatedScenarios = simulatedScenarios,
    observedData = observedData
  )

  expect_true(ospsuite.utils::isOfType(dataCombined, "DataCombined"))
  expect_equal(names(dataCombined)[[1]], "AciclovirPop")
})

test_that("It creates correct DataCombined when both plotGridNames and data combined names are provided", {
  dataCombined <- createDataCombinedFromExcel(
    projectConfiguration = projectConfiguration,
    plotGridNames = "Aciclovir3",
    dataCombinedNames = "AciclovirPVB",
    simulatedScenarios = simulatedScenarios,
    observedData = observedData
  )

  expect_true(ospsuite.utils::isOfType(dataCombined, "DataCombined"))
  expect_true(all(names(dataCombined) %in% c("AciclovirPop", "AciclovirPVB")))
})

test_that("It does not create duplicates when both plotGridNames and data combined names are provided", {
  dataCombined <- createDataCombinedFromExcel(
    projectConfiguration = projectConfiguration,
    plotGridNames = "Aciclovir3",
    dataCombinedNames = "AciclovirPop",
    simulatedScenarios = simulatedScenarios,
    observedData = observedData
  )

  expect_true(ospsuite.utils::isOfType(dataCombined, "DataCombined"))
  expect_equal(names(dataCombined), "AciclovirPop")
})
