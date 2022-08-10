testInstalledPackages <- function() {
  # returns TRUE if esqlabsR can be loaded, 
  # or raises errors otherwise
  library(esqlabsR)
  return(TRUE)
}

testPKSIMConnection <- function() {
  pkSimPath <- NULL
  # returns TRUE if esqlabsR can be loaded, 
  # or raises errors otherwise
  library(esqlabsR)
  library(ospsuite)
  initPKSim(pkSimPath)
  return(TRUE)
}

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
  states <- c("Installing RENV", "Installing CRAN packages", 
              "Checking RTOOLS", "Installing rClr", "Installing ospsuite.utils", 
              "Installing tlf", "Installing ospsuite", "Installing ospsuite.PI", 
              "Installing esqlabsR", "Testing installed packages", "Testing PK-Sim connection",
              "Testing simulations")
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

updateEnvironment <- function(rtoolsPath = NULL, rclrVersion = "0.9.2", suppressOutput = TRUE) {
  # Use RENV to snapshot the existing environment
  installationLockfile <- paste0("pre.", as.integer(Sys.time()), ".lock")
  renv::snapshot(lockfile = installationLockfile, prompt = FALSE)
  
  # Update the packages
  displayProgress("Installing CRAN packages", suppressOutput = suppressOutput)
  install.packages(c("R6", "stringr", "readr", "hash", "readxl", "shiny", "shinyjs", "vctrs", "writexl", "dplyr", "tidyr", "ggplot2", "FME", "patchwork"), 
                   dependencies = TRUE)
  
  displayProgress("Checking RTOOLS", suppressOutput = suppressOutput)
  if (Sys.which("make") == "") { # rtools is not found
    if (!is.null(rtoolsPath)) { # adding an existing installation of rtools to path
      Sys.setenv(PATH = paste(rtoolsPath, Sys.getenv("PATH"), sep = ";"))
    } else {
      displayProgress("Checking RTOOLS", success = FALSE, message = "Install Rtools and / or specify a path to an existing installation, then run updateEnvironment(rtoolsPath = path)", suppressOutput = suppressOutput)
      return()
    }
  }
  if (Sys.which("make") == "") {
    displayProgress("Checking RTOOLS", success = FALSE, message = paste0("Rtools not found at ", rtoolsPath, ", cannot continue"), suppressOutput = suppressOutput)
    return()
  }

  displayProgress("Installing rClr", suppressOutput = suppressOutput)
  if (version$major == "4") {
    install.packages(paste0("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v", rclrVersion, "/rClr_", rclrVersion, ".zip"), repos = NULL)
  } 
  if (version$major == "3") {
    install.packages(paste0("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v", rclrVersion, "-R3/rClr_", rclrVersion, ".zip"), repos = NULL)
  }
  displayProgress("Installing ospsuite.utils", suppressOutput = suppressOutput)
  install.packages("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-rutils/artifacts/ospsuite.utils.zip", repos = NULL)
  displayProgress("Installing tlf", suppressOutput = suppressOutput)
  install.packages("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/tlf-library/artifacts/tlf.zip", repos = NULL)
  displayProgress("Installing ospsuite", suppressOutput = suppressOutput)
  install.packages("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/ospsuite.zip", repos = NULL)
  displayProgress("Installing ospsuite.PI", suppressOutput = suppressOutput)
  install.packages("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-parameteridentification/artifacts/ospsuite.parameteridentification.zip", repos = NULL)
  displayProgress("Installing esqlabsR", suppressOutput = suppressOutput)
  install.packages("https://ci.appveyor.com/api/projects/StephanSchaller/esqlabsr/artifacts/esqlabsR.zip", repos = NULL)
  
  # Test if a new environment is working
  # If it does, snapshot the environment with an interactive prompt
  # If it does not, revert to a previous state
  displayProgress("Testing installed packages", suppressOutput = suppressOutput)
  flagPackages <- FALSE
  try(flagPackages <- testInstalledPackages())
  if (!flagPackages) {
    displayProgress("Testing installed packages", success = FALSE, message = "esqlabsR fails to load. Returning to initial package configuration", suppressOutput = suppressOutput)
    renv::restore(installationLockfile, prompt = FALSE, clean = TRUE)
    if (file.exists(installationLockfile)) {
      file.remove(installationLockfile)
    }
    return()
  }
  
  displayProgress("Testing PK-Sim connection", suppressOutput = suppressOutput)
  flagConnection <- FALSE
  try(flagConnection <- testPKSIMConnection())
  if (!flagConnection) {
    displayProgress("Testing PK-Sim connection", success = FALSE, message = "PK-Sim fails to load. The installation might be incompatible with your current version of PK-Sim", suppressOutput = suppressOutput)
    #renv::restore(installationLockfile, prompt = FALSE, clean = TRUE)
    if (file.exists(installationLockfile)) {
      file.remove(installationLockfile)
    }
    return()
  }

  displayProgress("Testing simulations", suppressOutput = suppressOutput)
  flagSimulations <- FALSE
  try(flagSimulations <- testSimulationsRunning())
  if (!flagSimulations) {
    displayProgress("Testing simulations", success = FALSE, message = "Simulations failed. The installation might be incompatible with your current version of PK-Sim", suppressOutput = suppressOutput)
    #renv::restore(installationLockfile, prompt = FALSE, clean = TRUE)
    if (file.exists(installationLockfile)) {
      file.remove(installationLockfile)
    }
    return()
  }
  renv::snapshot(prompt = FALSE)
  if (file.exists(installationLockfile)) {
    file.remove(installationLockfile)
  }
  return(invisible(TRUE))
}

displayProgress("Installing RENV")
install.packages("renv")
renv::init(bare = TRUE, restart = FALSE)
invisible(updateEnvironment())
