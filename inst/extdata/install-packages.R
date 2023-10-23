verify_long_path_enabled <- function() {
  suppressWarnings({
    reg <- readRegistry(
      "SYSTEM\\CurrentControlSet\\Control\\FileSystem\\",
      "HLM"
    )
  })

  if (reg$LongPathsEnabled != 1) {
    stop("Long paths are not enabled. Please enable them and restart R.\r
To enable long path, follow instructions at this adress: https://www.microfocus.com/documentation/filr/filr-4/filr-desktop/t47bx2ogpfz7.html")
  }
}


install_script_deps <- function(install_option) {
  # install dependencies required for this script

  # Do not prompt user if packages must be built
  options(install.packages.compile.from.source = "always")

  cat("Install prerequisite dependencies\n")
  # if user chose local installation, install renv and initialize it
  if (install_option == 1) {
    install.packages("renv")
    require(renv)
    init(
      bare = TRUE,
      restart = FALSE,
      force = TRUE
    )
  }

  # Declare packages
  packages <- c("cli", "remotes", "rstudioapi", "pkgbuild")

  # Loop through each package
  for (package in packages) {
    # Install package
    # Note: `installed.packages()` returns a vector of all the installed packages
    if (!require(package, character.only = T, quietly = T)) {
      # Install it
      install.packages(
        package,
        dependencies = TRUE,
        quiet = TRUE
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
  cli_alert_success("Install prerequisite dependencies")
}

check_RTools <- function() {
  cli_progress_step("Verify RTools installation")
  if (!pkgbuild::find_rtools()) {
    cli_alert_danger("Rtools is not installed. Please install it before continuing.")
    cli_alert_info("You can download Rtools from https://cran.r-project.org/bin/windows/Rtools/")
    cli_alert_info("Please install the latest version of Rtools.")
    cli_alert_info("After installing Rtools, please restart R and run this script again.")
    stop("Rtools is not installed.")
  }
  cli_progress_done(result = "done")
}

get_PKSim_Minimal <- function(install_option) {
  # Only for local installation
  if (install_option == 1) {
    cli_progress_step("Get minimal version of PKSim.")

    download.file("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/pksim_minimal.zip",
      destfile = "pksim_minimal.zip"
    )
    unzip("pksim_minimal.zip", exdir = "PKSim")
    file.remove("pksim_minimal.zip")

    cli_progress_step("Verify PKSim installation")
    # test if PK-Sim.R dll can be loaded
    ospsuite::initPKSim(pksimFolderPath = "PKSim")

    cli_progress_done(result = "done")
  }
}

get_esqlabsR <- function() {
  # package installation
  cli_progress_step("Install esqlabsR and dependencies")

  install.packages("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip",
    repos = NULL,
    type = "binary",
    quiet = TRUE
  )
  remotes::install_github("esqLABS/esqlabsR@*release",
    build = TRUE,
    upgrade = "always"
  )

  cli_progress_done(result = "done")
}

load_esqlabsR <- function() {
  cli_progress_step("Load esqlabsR")
  unloadNamespace(ns = "esqlabsR")
  unloadNamespace(ns = "ospsuite.parameteridentification")
  # test if installed packages can be loaded
  library("esqlabsR")

  cli_progress_done(result = "done")
}

run_test_simulation <- function() {
  # Run test simulation to verify installation

  cli_progress_step("Run test simulation.")

  tryCatch(
    expr = {
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
    error = function(error) {
      cli_progress_done(result = "failed")
      cli_alert_danger(text = paste("Simulation test failed: ", error))
    }
  )
}

initialize_project <- function() {
  initialize_option <- utils::menu(c("Yes", "No"),
    title = "Do you want to initialize project folder structure ?"
  )

  if (initialize_option == 1) {
    cli_progress_step("Initialize project structure")
    init_project(destination = "../")
    update_project_conf()
    cli_progress_done(result = "done")
  }
}

update_project_conf <- function() {
  compoundpropertiesinternal_file <-
    list.files("../Data",
      pattern = "*_Compound Properties \\(Internal\\).xlsx"
    )[1]

  project_configuration <-
    createDefaultProjectConfiguration(path = "../ProjectConfiguration.xlsx")

  new_timevalue_name <- rename_timevalue_file()

  project_configuration$dataFile <- new_timevalue_name

  project_configuration$compoundPropertiesFile <- compoundpropertiesinternal_file

  project_configuration$save()
}

rename_timevalue_file <- function() {
  proj_name <- get_project_name()

  timevalues_file <-
    list.files("../Data",
      pattern = "*TimeValuesData.xlsx",
      full.names = TRUE
    )[1]

  new_name <- gsub(
    basename(timevalues_file),
    paste0(proj_name, "_TimeValuesData.xlsx"),
    timevalues_file
  )

  file.rename(timevalues_file, new_name)

  return(new_name)
}

get_project_name <- function() {
  rproj_name <- list.files(pattern = ".Rproj")
  proj_name <- sub(
    pattern = "_V.*?.Rproj",
    replacement = "",
    x = rproj_name
  )

  return(proj_name)
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

setup_esqlabsR <- function() {
  verify_long_path_enabled()

  # Display a menu asking if user wants to install packages in local env or  globally
  install_option <- utils::menu(c("In local environment (available for one project)", "In Global environment (available for all projects)"),
    title = "Where do you want to install {esqlabsR} and other packages?"
  )

  install_script_deps(install_option)

  check_RTools()

  get_esqlabsR()

  get_PKSim_Minimal(install_option)

  load_esqlabsR()

  run_test_simulation()

  initialize_project()

  if (install_option == 1) {
    renv::snapshot(prompt = FALSE) # snapshot environment
  }

  restart_rstudio()
}

# Run this function to setup esqlabsR
setup_esqlabsR()
