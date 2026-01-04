#' Validate all configuration files in a project
#' @param projectConfiguration ProjectConfiguration object or path to ProjectConfiguration.xlsx
#' @return Named list of validationResult objects
#' @export
validateAllConfigurations <- function(projectConfiguration) {
  results <- list()

  # Handle both path and object input
  if (is.character(projectConfiguration)) {
    results$projectConfiguration <- .validateProjectConfiguration(projectConfiguration)
    if (!results$projectConfiguration$is_valid()) {
      class(results) <- c("ValidationResults", class(results))
      return(results)
    }
    projectConfiguration <- createDefaultProjectConfiguration(projectConfiguration)
  }

  # Define required configuration files with their display names
  required_files <- list(
    scenarios = list(
      path = projectConfiguration$scenariosFile,
      name = "Scenarios.xlsx",
      validate = .validateScenariosFile
    ),
    plots = list(
      path = projectConfiguration$plotsFile,
      name = "Plots.xlsx",
      validate = .validatePlotsFile
    ),
    individuals = list(
      path = projectConfiguration$individualsFile,
      name = "Individuals.xlsx",
      validate = .validateIndividualsFile
    ),
    populations = list(
      path = projectConfiguration$populationsFile,
      name = "Populations.xlsx",
      validate = .validatePopulationsFile
    ),
    models = list(
      path = projectConfiguration$modelParamsFile,
      name = "ModelParameters.xlsx",
      validate = .validateModelsFile
    ),
    applications = list(
      path = projectConfiguration$applicationsFile,
      name = "Applications.xlsx",
      validate = .validateApplicationsFile
    )
  )

  # Validate each required configuration file
  for (config_name in names(required_files)) {
    file_info <- required_files[[config_name]]
    file_path <- file_info$path

    # Check if file path is not configured (NA)
    if (is.na(file_path)) {
      results[[config_name]] <- validationResult$new()
      results[[config_name]]$add_critical_error(
        category = "Missing File",
        message = messages$validationRequiredFileNotConfigured(file_info$name)
      )
    } else {
      # Path is configured - validate the file
      results[[config_name]] <- file_info$validate(file_path)
    }
  }

  # Add cross-reference validation
  results$crossReferences <- .validateCrossReferences(projectConfiguration, results)

  class(results) <- c("ValidationResults", class(results))
  return(results)
}

#' Check if validation results contain any critical errors
#' @param validationResults Output from validateAllConfigurations
#' @return Logical indicating if there are critical errors
#' @export
isAnyCriticalErrors <- function(validationResults) {
  any(sapply(validationResults, function(r) {
    if (inherits(r, "validationResult")) {
      r$has_critical_errors()
    } else {
      FALSE
    }
  }))
}

#' Get summary of all validation results
#' @param validationResults Output from validateAllConfigurations
#' @return List with summary statistics
#' @export
validationSummary <- function(validationResults) {
  summary <- list(
    total_critical_errors = 0,
    total_warnings = 0,
    files_with_errors = character(),
    files_with_warnings = character()
  )

  for (name in names(validationResults)) {
    result <- validationResults[[name]]
    if (inherits(result, "validationResult")) {
      if (result$has_critical_errors()) {
        summary$total_critical_errors <- summary$total_critical_errors +
          length(result$critical_errors)
        summary$files_with_errors <- c(summary$files_with_errors, name)
      }
      if (length(result$warnings) > 0) {
        summary$total_warnings <- summary$total_warnings + length(result$warnings)
        summary$files_with_warnings <- c(summary$files_with_warnings, name)
      }
    }
  }

  summary
}
