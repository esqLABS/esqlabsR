# Message strings used in the setup script
packageInstallationMessages <- list(
  installRTools = "Install Rtools compatible with your R version, then run updateEnvironment() again.)",
  packageLoadFails = "esqlabsR fails to load. Returning to initial package configuration",
  PKSimLoadFails = "PK-Sim fails to load. The installation might be incompatible with your current version of PK-Sim",
  simulationsFailed = "Simulations failed. The installation might be incompatible with your
  project folder structure, or the test scenario `TestScenario` is not specified.",
  cannotInitRenv = "Could not initializt environment. Make sure your current
  working directory is the root of project `Code` folder"
)

# List of packages that will be installed from CRAN
.cranPackages <- c("R6", "stringr", "readr", "hash", "readxl", "shiny", "shinyjs", "vctrs", "writexl", "dplyr", "tidyr", "ggplot2", "FME", "patchwork", "jsonlite", "purrr")
#Download paths of released package versions
.releasePaths <- list(ospsuite.utils = "https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/download/v1.3.17/ospsuite.utils_1.3.17.tar.gz",
                      tlf = "https://github.com/Open-Systems-Pharmacology/TLF-Library/releases/download/v1.4.89/tlf_1.4.89.tar.gz",
                      ospsuite = "https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases/download/v11.0.123/ospsuite_11.0.123.zip",
                      ospsuite.parameteridentification = "https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/releases/download/v1.1.0/ospsuite.parameteridentification_1.1.0.9002.zip",
                      esqlabsR = "https://github.com/esqLABS/esqlabsR/releases/download/3.0.89/esqlabsR_3.0.89.zip")
#Download paths of latest develop package versions
.developPaths <- list(ospsuite.utils = "https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-rutils/artifacts/ospsuite.utils.zip",
                      tlf = "https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/tlf-library/artifacts/tlf.zip",
                      ospsuite = "https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/ospsuite.zip",
                      ospsuite.parameteridentification = "https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-parameteridentification/artifacts/ospsuite.parameteridentification.zip",
                      esqlabsR = "https://ci.appveyor.com/api/projects/StephanSchaller/esqlabsr/artifacts/esqlabsR.zip")

#' Test if installed packages can be loaded
#'
#' @details Tries to load `esqlabsR`
#'
#' @return `TRUE` if the packages can be loaded without error, `FALSE` otherwise.
testInstalledPackages <- function() {
  # returns TRUE if esqlabsR can be loaded,
  # or raises errors otherwise
  library(esqlabsR)
  return(TRUE)
}

#' Test connection to PK-Sim
#'
#' @param pkSimPath Path where PK-Sim is installed. If this is not specified
#' (`NULL`), path is estimated by the `ospsuite` package.
#'
#' @return `TRUE` if `ospsuite::initPKSim(pkSimPath)` is successful.
testPKSIMConnection <- function(pkSimPath = NULL) {
  # returns TRUE if PK-Sim.R dll can be loaded,
  # or raises errors otherwise
  library(esqlabsR)
  ospsuite::initPKSim(pkSimPath)
  return(TRUE)
}

# Remove functions created by this script from the environment
cleanEnvironment <- function(){
  rm(packageInstallationMessages,
     .releasePaths,
     .developPaths,
     .cranPackages,
     testInstalledPackages,
     testPKSIMConnection,
     cleanEnvironment,
     testSimulationsRunning,
     displayProgress,
     installOSPPackages,
     installPackagesGlobally
  )
}

#' Test if a standard simulation workflow can be executed
#'
#' @return `TRUE` if no errors were produced during execution of a standard
#' simulation workflow.
testSimulationsRunning <- function() {
  # returns TRUE if simulations run correctly,
  # or raises errors otherwise
  library(esqlabsR)
  sourceAll(file.path(getwd(), "utils"))
  sourceAll(file.path(getwd(), "InputCode"))
  sourceAll(file.path(getwd(), "Scenarios"))
  sourceAll(file.path(getwd(), "TransferFunctions"))
  projectConfiguration <- createDefaultProjectConfiguration()
  scenarioNames <- c("TestScenario")
  scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)
  scenarioConfiguration$setTestParameters <- FALSE
  simulations <- vector("list", length(scenarioNames))
  for (i in seq_along(simulations)) {
    scenarioConfiguration$scenarioName <- scenarioNames[[i]]
    simulations[[i]] <- initializeScenario(scenarioConfiguration = scenarioConfiguration)
  }
  names(simulations) <- scenarioNames
  simulationResults <- runSimulations(simulations = simulations, simulationRunOptions = scenarioConfiguration$simulationRunOptions)
  return(TRUE)
}

displayProgress <- function(current, success = TRUE, message = NULL, suppressOutput = TRUE) {
  states <- c("Installing RENV", "Installing CRAN packages",  "Checking RTOOLS",
             "Installing rClr", "Installing ospsuite.utils",
              "Installing tlf", "Installing ospsuite", "Installing ospsuite.PI",
              "Installing esqlabsR", "Testing installed packages", "Testing PK-Sim connection",
              "Testing simulations",
              "Installation successful")
  if (suppressOutput) {
    cat("\014")
    for (i in seq_along(states)) {
      cat(paste0(ifelse(i < which(states == current), "V", ifelse(i > which(states == current), ".", ifelse(success == FALSE, "X", ".")))), " ", states[[i]], "\n")
    }
  } else {
    cat(paste0(current, "\n"))
  }
  if (!is.null(message)) {
    message(message)
  }
}

#' Install all osps packages and their dependencies.
#'
#' @param rclrVersion Version of rClr package. Default is 0.9.2 for Windows R4.
#' @param developerVersion If `FALSE` (default), release verions of the packages
#' will be installed. If `TRUE`, latest developer builds of the osps packages
#' will be installed
#' @param lib character vector giving the library directories where to install
#' the packages. Recycled as needed. Defaults to the first element
#' of .libPaths().
#' @param suppressOutput
#'
#' @return
#' @export
#'
#' @examples
installOSPPackages <- function(rclrVersion = "0.9.2",
                               suppressOutput = TRUE,
                               developerVersion = FALSE,
                               lib = .libPaths()[[1]]){
  # pkgbuild is only needed for the installation script itself, can be installed once
  # install.packages("pkgbuild", lib = lib)
  displayProgress("Checking RTOOLS", suppressOutput = suppressOutput)
  if (!pkgbuild::has_rtools()) { # rtools is not found
    displayProgress("Checking RTOOLS", success = FALSE, message = packageInstallationMessages$installRTools, suppressOutput = suppressOutput)
    return()
  }

  # Install dependencies from CRAN
  displayProgress("Installing CRAN packages", suppressOutput = suppressOutput)
  install.packages(.cranPackages,
                   dependencies = TRUE,
                   lib = lib)

  displayProgress("Installing rClr", suppressOutput = suppressOutput)
  if (version$major == "4") {
    install.packages(paste0("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v", rclrVersion, "/rClr_", rclrVersion, ".zip"), repos = NULL,
                     lib = lib)
  }
  if (version$major == "3") {
    install.packages(paste0("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v", rclrVersion, "-R3/rClr_", rclrVersion, ".zip"), repos = NULL,
                     lib = lib)
  }

  packagePaths <- .releasePaths
  if (developerVersion) {
    packagePaths <- .developPaths
  }

  displayProgress("Installing ospsuite.utils", suppressOutput = suppressOutput)
  install.packages(packagePaths$ospsuite.utils, repos = NULL, lib = lib)
  displayProgress("Installing tlf", suppressOutput = suppressOutput)
  install.packages(packagePaths$tlf, repos = NULL, lib = lib)
  displayProgress("Installing ospsuite", suppressOutput = suppressOutput)
  install.packages(packagePaths$ospsuite, repos = NULL, lib = lib)
  displayProgress("Installing ospsuite.PI", suppressOutput = suppressOutput)
  install.packages(packagePaths$ospsuite.parameteridentification, repos = NULL, lib = lib)
  displayProgress("Installing esqlabsR", suppressOutput = suppressOutput)
  install.packages(packagePaths$esqlabsR, repos = NULL, lib = lib)
}

#' Install osps packages and their dependencies into project library.
#'
#' @details This function will install a local library using `renv`. After installation,
#' it will perform a check to load the package, establish connection to PK-Sim,
#' and (if `testExampleSimulation` is `TRUE`) run a test scenario. If loading
#' the package fails, the previous state of the library is restored.
#'
#' @param updatePackages If `TRUE`, all installed packages will be
#' updated prior to installation. Default is `FALSE`.
#' @param pkSimPath Path where PK-Sim is installed. If this is not specified
#' (`NULL`), path is estimated by the `ospsuite` package.
#' @param rclrVersion Version of rClr package. Default is 0.9.2 for Windows R4.
#' @param testExampleSimulation If `TRUE` (default), try to run a scenario `TestScenario`.
#' This will fail if either no such scenario is defined, or the project structure
#' is not compatible with the current package.
#' @param developerVersion If `TRUE` (default), latest developer builds of the
#' osps packages will be installed.
#' @param lib character vector giving the library directories where to install
#' the packages. Recycled as needed. Defaults to the first element of .libPaths().
#' @param suppressOutput
installPackagesLocally <- function(updatePackages = FALSE, pkSimPath = NULL,
                                   rclrVersion = "0.9.2",
                                   suppressOutput = TRUE,
                                   testExampleSimulation = TRUE,
                                   developerVersion = TRUE,
                                   lib = .libPaths()[[1]]){
  # Always install renv to make sure that the project is using the most
  # recent version
  displayProgress("Installing RENV")
  install.packages("renv")
  flagConnection <- FALSE
  try({renv::init(bare = TRUE, restart = FALSE)
      flagConnection <- TRUE})
  if (!flagConnection) {
    error(packageInstallationMessages$cannotInitRenv)
  }

  # Use RENV to snapshot the existing environment
  installationLockfile <- paste0("pre.", as.integer(Sys.time()), ".lock")
  renv::snapshot(lockfile = installationLockfile, prompt = FALSE)

  # Install packages
  installPackagesGlobally(updatePackages = updatePackages,
                          pkSimPath = pkSimPath,
                          rclrVersion = rclrVersion,
                          suppressOutput = suppressOutput,
                          developerVersion = developerVersion,
                          lib = lib)

  # Test if a new environment is working
  # If it does, snapshot the environment with an interactive prompt
  # If it does not, revert to a previous state
  displayProgress("Testing installed packages", suppressOutput = suppressOutput)
  flagPackages <- FALSE
  try(flagPackages <- testInstalledPackages())
  if (!flagPackages) {
    displayProgress("Testing installed packages", success = FALSE,
                    message = packageInstallationMessages$packageLoadFails,
                    suppressOutput = suppressOutput)
    renv::restore(installationLockfile, prompt = FALSE, clean = TRUE)
    if (file.exists(installationLockfile)) {
      file.remove(installationLockfile)
    }
    return()
  }

  if(testExampleSimulation){
    displayProgress("Testing simulations", suppressOutput = suppressOutput)
    flagSimulations <- FALSE
    try(flagSimulations <- testSimulationsRunning())
    if (!flagSimulations) {
      displayProgress("Testing simulations", success = FALSE, message = packageInstallationMessages$simulationsFailed, suppressOutput = suppressOutput)
      #renv::restore(installationLockfile, prompt = FALSE, clean = TRUE)
      if (file.exists(installationLockfile)) {
        file.remove(installationLockfile)
      }
      return()
    }
  }

  renv::snapshot(prompt = FALSE)
  if (file.exists(installationLockfile)) {
    file.remove(installationLockfile)
  }

  displayProgress("Installation sucessful", suppressOutput = suppressOutput)
  return(invisible(TRUE))
}

#' Install osps packages and their dependencies into global library.
#'
#' @param updatePackages If `TRUE`, all installed packages will be
#' updated prior to installation. Default is `FALSE`.
#' @param pkSimPath Path where PK-Sim is installed. If this is not specified
#' (`NULL`), path is estimated by the `ospsuite` package.
#' @param rclrVersion Version of rClr package. Default is 0.9.2 for Windows R4.
#' @param developerVersion If `TRUE` (default), latest developer builds of the
#' osps packages will be installed.
#' @param lib character vector giving the library directories where to install
#' the packages. Recycled as needed. Defaults to the first element of .libPaths().
#' @param suppressOutput
installPackagesGlobally <- function(updatePackages = FALSE, pkSimPath = NULL,
                                    rclrVersion = "0.9.2",
                                    suppressOutput = TRUE,
                                    developerVersion = TRUE,
                                    lib = .libPaths()[[1]]){
  installOSPPackages(rclrVersion = rclrVersion,
                     suppressOutput = suppressOutput,
                     developerVersion = developerVersion)

  displayProgress("Testing PK-Sim connection", suppressOutput = suppressOutput)
  flagConnection <- FALSE
  try(flagConnection <- testPKSIMConnection(pkSimPath = pkSimPath))
  if (!flagConnection) {
    stop(message = packageInstallationMessages$PKSimLoadFails)
  }

  # Update all installed packages from CRAN
  if (updatePackages) {
    update.packages(ask = FALSE)
  }

  displayProgress("Installation successful", suppressOutput = suppressOutput)
}

pkSimPath <- NULL
pkSimPath <- "c:\\Program Files\\Open Systems Pharmacology\\PK-Sim 11.1\\"
#installPackagesGlobally(pkSimPath = pkSimPath, suppressOutput = TRUE, developerVersion = TRUE)
#installPackagesLocally(pkSimPath = pkSimPath, suppressOutput = TRUE, developerVersion = TRUE)

#Clean the workspace
#cleanEnvironment()
