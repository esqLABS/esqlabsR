setwd("D:/Documents/esqlabs GmbH/Parexel/Dextromethorphan/V00.01/Code")

source("Scenarios/initEsqlabsProject.R")
projectConfiguration <- initEsqlabsProject()

loadPreSimulatedResults <- TRUE

# Path to the folder where pre-computed results are located
resultsSubFolder <- file.path(
  "2023-06-19 11-11")
# Path to the folder where simulation results whould be stored. If `NULL`,
# default results folder is used.
# outputFolder <- file.path(
#   projectConfiguration$outputFolder,
#   "SimulationResults", "Trial simulation",
#   format(Sys.time(), "%F %H-%M")
# )
outputFolder <- NULL

setTestParameters <- FALSE

scenarioResults <- simulateScenarios(
  projectConfiguration = projectConfiguration,
  setTestParameters = setTestParameters,
  loadPreSimulatedResults = loadPreSimulatedResults,
  scenarioNames = NULL,
  loadResultsFolder = resultsSubFolder,
  saveResultsFolder = outputFolder
)

observedData <- esqlabsR::loadObservedDataFromPKML(projectConfiguration)

plotGridNames <- "Capon 1996 EM, 30 mg dextromethorphan hydrobromide (capsule/solution), n=6"
plotGridNames <- NULL
allPlots <- createPlotsFromExcel(simulatedScenarios = scenarioResults$simulatedScenarios,
                                 observedData = observedData,
                                 projectConfiguration = projectConfiguration,
                                 plotGridNames = plotGridNames)
