install_script_deps <- function(install_option) {

  # install dependencies required for this script

  cat("Installing prerequisite dependencies.\n")

  # if user chose local installation, install renv and initialize it
  if (install_option == 1) {
    install.packages("renv")
    require(renv)
    init(bare = TRUE, restart = FALSE)
  }

  # Declare packages
  packages <- c("cli","remotes","rstudioapi","pkgbuild")

  # Loop through each package
  for (package in packages) {

    # Install package
    # Note: `installed.packages()` returns a vector of all the installed packages
    if (!require(package,character.only = T, quietly = T)) {
      # Install it
      install.packages(
        package,
        dependencies = TRUE
      )
    }

    # Load package
    # Note: `.packages()` returns a vector of all the loaded packages
    if (!(package %in% .packages())) {
      # Load it
      library(
        package,
        character.only = TRUE
      )
    }
  }

  cli_alert_success(text = "Prerequesite dependencies available.")
}

check_RTools <- function() {
  cli_progress_step("Verifying RTools installation")
  if (!pkgbuild::find_rtools()) {
    cli_alert_danger("Rtools is not installed. Please install it before continuing.")
    cli_alert_info("You can download Rtools from https://cran.r-project.org/bin/windows/Rtools/")
    cli_alert_info("Please install the latest version of Rtools.")
    cli_alert_info("After installing Rtools, please restart R and run this script again.")
    stop("Rtools is not installed.")
  }
}

get_PKSim_Minimal <- function(install_option) {

  # Only for local installation
  if (install_option == 1) {

    cli_progress_step("Getting minimal version of PKSim.")

    download.file("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/pksim_minimal.zip",
                  destfile = "pksim_minimal.zip"
    )
    unzip("pksim_minimal.zip", exdir = "PKSim")
    file.remove("pksim_minimal.zip")

    cli_progress_step("Verifying PKSim installation")
    # test if PK-Sim.R dll can be loaded
    initPKSim()

    snapshot(prompt = FALSE)

  }


}

get_esqlabsR <- function(){
  # package installation
  cli_progress_step("Installing esqlabsR and dependencies")

  install.packages("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip",
                   repos = NULL,
                   type = "binary",
                   quiet = TRUE
  )
  install_github("esqLABS/esqlabsR",
                          quiet = TRUE,
                          build = TRUE
  )
}

load_esqlabsR <- function(){
  cli_progress_step("Loading esqlabsR")
  # test if installed packages can be loaded
  library("esqlabsR")
}

run_test_simulation <- function() {
  # Run test simulation to verify installation

  cli_progress_step("Running test simulation.")

  tryCatch(expr = {
    require(esqlabsR)

    suppressWarnings({
      projectConfiguration <- createDefaultProjectConfiguration(path = esqlabsR:::test_ProjectConfiguration())
    })
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

    scenarios <- createScenarios(scenarioConfigurations)

    simulatedScenarios <- runScenarios(scenarios = scenarios)

    cli_progress_done(result = "done")
    cli_alert_success("Installation successful.")

  },
  error = function(error){
    cli_progress_done(result = "failed")
    cli_alert_danger(text = paste("Simulation test failed: ", error))
  })
}

initialize_project <- function(){
  initialize_option <- utils::menu(c("Yes", "No"),
                               title = "Do you want to initialize project folder structure ?"
  )
  if (initialize_option == 1) {
    init_project()
  }
}

restart_rstudio <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  # if running in RStudio, restart R session
  if (isAvailable()) {
    cli_alert_info("Restarting R session.")
    restartSession(command = "library(esqlabsR)")
  } else {
    cli_alert_info("Please restart R to complete the installation.")
  }
}

# Main installation script
setup_esqlabsR <- function() {
  # Display a menu asking if user wants to install packages in local env or  globally
  install_option <- utils::menu(c("In local environment (available for one project)", "In Glocal environment (available for all projects)"),
                               title = "Where do you want to install {esqlabsR} and other packages?"
  )

  install_script_deps(install_option)

  check_RTools()

  get_PKSim_Minimal(install_option)

  get_esqlabsR()

  load_esqlabsR()

  run_test_simulation()

  initialize_project()

  restart_rstudio()

}

# run installation function
setup_esqlabsR()
