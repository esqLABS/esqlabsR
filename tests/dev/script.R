# Plots tests
setwd("D:/Documents/esqlabs GmbH/S_KyowaKirin_2022-125_Ocular Model - Documents/V00.10/Code")

pkSimPath <- "PKSim"
# For local testing, if required

initPKSim(pkSimPath)
sourceAll(file.path(getwd(), "utils"))
sourceAll(file.path(getwd(), "InputCode"))
sourceAll(file.path(getwd(), "Scenarios"))
sourceAll(file.path(getwd(), "TransferFunctions"))

projectConfiguration <- createDefaultProjectConfiguration()

loadPreSimulatedResults <- TRUE

# Set Output Folder
# Optional sub-folder which contains simulated results and simulations
resultsSubFolder <- "2023-06-03 21-04"

outputFolder <- file.path(
  projectConfiguration$outputFolder,
  "SimulationResults",
  # "Kd_2.2"
  format(Sys.time(), "%F %H-%M")
)

scenarioNames <- c(
  "SBL465-041-sd-ocular",
  "SBL465-041-qd-ocular",
  "r-19-0240-ocular-sd-0.1mg",
  "r-19-0240-ocular-sd-0.1mg_microcrystal",
  "r-20-0070-ocular-sd-0.1mg",
  "r-22-0217-iv-0.05mgkg",
  "r-22-0217-iv-0.1mgkg",
  "r-22-0217-po-0.05mgkg"
)

scenarioResults <- loadScenarioResults(scenarioNames, file.path(projectConfiguration$outputFolder, "SimulationResults", loadResultsFolder = resultsSubFolder))

dataSheets <- c(
  "14.SBL465-041",
  "15.r-19-0240",
  "17.r-20-0070",
  "20.r-22-0217"
)

# Load observed data
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets
)

# For single plot, create a DataCombined only
dcSinglePlot <- createDataCombinedFromExcel(
  file = projectConfiguration$plotsFile,
  dataCombinedNames = "r-19-0240-sd-0.1mg-Conjunctiva_nanocrystal",
  simulatedScenarios = scenarioResults,
  observedData = observedData
)

plotConfig <- createEsqlabsPlotConfiguration()
plotGridConfig <- createEsqlabsPlotGridConfiguration()
exportConfig <- createEsqlabsExportConfiguration(projectConfiguration = projectConfiguration)

plotConfig$yAxisScale <- "log"
plotConfig$title <- "Main title"
individualPlot <- plotIndividualTimeProfile(dcSinglePlot$`r-19-0240-sd-0.1mg-Conjunctiva_nanocrystal`, plotConfig)
# predVsObs
predVsObsPlot <- plotObservedVsSimulated(dcSinglePlot$`r-19-0240-sd-0.1mg-Conjunctiva_nanocrystal`, plotConfig)

exportConfig$savePlot(plotObject = individualPlot, fileName = "individualPlot.png")

# Multigrid
plots <- createPlotsFromExcel(
  simulatedScenarios = scenarioResults,
  observedData = observedData,
  projectConfiguration = projectConfiguration,
  stopIfNotFound = TRUE,
  plotGridNames = c(
    "r-19-0240-sd-0.1mg_nanocrystal_mean"
    # "r-19-0240-sd-0.1mg_microcrystal_mean",
    # "r-20-0070-sd-0.1mg_mean",
    # "r-22-0217_mean",
    # "SBL465-041-sd-ocular",
    # "SBL465-041-qd-ocular",
    # "SBL465-041-qd-ocular_day14"
    # "0.1mg-sd-rabbit"
  )
)

exportConfig$savePlot(plotObject = plots$`r-19-0240-sd-0.1mg_nanocrystal_mean`, fileName = "multiPlot.png")
