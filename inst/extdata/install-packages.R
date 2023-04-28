# install dependencies
installationDeps <- function(installOption) {
  cat("Installing installation dependencies.\n")
  
  # if user chose local installation, install renv and initialize it
  if (installOption == 1) {
    install.packages("renv")
    renv::init(bare = TRUE, restart = FALSE)
  }
  install.packages("pkgbuild", quiet = TRUE)
  install.packages("cli", quiet = TRUE)
  install.packages("remotes", quiet = TRUE)
  install.packages("rstudioapi", quiet = TRUE)

}

# check if rtools is installed
checkRtools <- function() {
  if (!pkgbuild::find_rtools()) {
    cli::cli_alert_danger("Rtools is not installed. Please install it before continuing.")
    cli::cli_alert_info("You can download Rtools from https://cran.r-project.org/bin/windows/Rtools/")
    cli::cli_alert_info("Please install the latest version of Rtools.")
    cli::cli_alert_info("After installing Rtools, please restart R and run this script again.")
    stop("Rtools is not installed.")
  }
}

# Get PKSim minimal
getPKSimMinimal <- function() {
  download.file("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/pksim_minimal.zip",
                destfile = "pksim_minimal.zip"
  )
  unzip("pksim_minimal.zip", exdir = "PKSim")
  file.remove("pksim_minimal.zip")
}

# Run test simulation
runTestSimulation <- function() {
  sourceAll(file.path(getwd(), "utils"))
  sourceAll(file.path(getwd(), "InputCode"))
  sourceAll(file.path(getwd(), "Scenarios"))
  sourceAll(file.path(getwd(), "TransferFunctions"))
  projectConfiguration <- createDefaultProjectConfiguration()
  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Set scenario names to NULL if you want to simulate all scenarios defined in the
  # excel file
  # scenarioNames <- NULL

  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarioConfigurations[[1]]$setTestParameters <- FALSE
  simulatedScenarios <- runScenarios(
    scenarioConfigurations = scenarioConfigurations,
    customParams = NULL, saveSimulationsToPKML = TRUE
  )
}

# Main installation script
installEsqLabsR <- function() {
  # Display a menu asking if user wants to install packages in local env or  globally
  installOption <- utils::menu(c("In local environment (available for one project)", "In Glocal environment (available for all projects)"),
                               title = "Where do you want to install {esqlabsR} and other packages?"
  )

  installationDeps(installOption)

  checkRtools()


  cli::cli_progress_step("Installing esqlabsR and dependencies")
  install.packages("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip",
                   repos = NULL,
                   type = "binary",
                   quiet = TRUE
  )
  remotes::install_github("esqLABS/esqlabsR",
                          quiet = TRUE,
                          build = TRUE
  )

  # Only for local installation
  if (installOption == 1) {

    cli::cli_progress_step("Getting minimal version of PKSim.")
    getPKSimMinimal()

    cli::cli_progress_step("Verifying PKSim installation")
    # test if PK-Sim.R dll can be loaded
    ospsuite::initPKSim()

    renv::snapshot(prompt = FALSE)
  }

  cli::cli_progress_step("Loading esqlabsR")
  # test if installed packages can be loaded
  library("esqlabsR")


  cli::cli_progress_step("Running test simulation.")
  # run test simulations if the working directory path ends with "Code"
  if (endsWith(getwd(), "Code")) {
    runTestSimulation()
    cli::cli_progress_done()
  } else {
    cli::cli_alert_warning("Test simulation files could not be found.")
    cli::cli_progress_done(result = "failed")
  }

  cli::cli_alert_success("Installation successful.")

  # if running in RStudio, restart R session
  if (rstudioapi::isAvailable()) {
    cli::cli_alert_info("Restarting R session.")
    rstudioapi::restartSession()
  } else {
    cli::cli_alert_info("Please restart R to complete the installation.")
  }
}

# run installation function
installEsqLabsR()
