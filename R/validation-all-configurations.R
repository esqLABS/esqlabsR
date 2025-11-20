#' Validate all configuration files in a project
#' @param projectConfiguration ProjectConfiguration object or path to ProjectConfiguration.xlsx
#' @return Named list of ValidationResult objects
#' @export
validateAllConfigurations <- function(projectConfiguration) {
  results <- list()

  # Handle both path and object input
  if (is.character(projectConfiguration)) {
    results$projectConfiguration <- validateProjectConfiguration(projectConfiguration)
    if (!results$projectConfiguration$is_valid()) {
      class(results) <- c("ValidationResults", class(results))
      return(results)
    }
    projectConfiguration <- createDefaultProjectConfiguration(projectConfiguration)
  }

  # Validate each configuration file
  if (!is.na(projectConfiguration$scenariosFile)) {
    results$scenarios <- validateScenariosFile(projectConfiguration$scenariosFile)
  }

  if (!is.na(projectConfiguration$plotsFile)) {
    results$plots <- validatePlotsFile(projectConfiguration$plotsFile)
  }

  if (!is.na(projectConfiguration$individualsFile)) {
    results$individuals <- validateIndividualsFile(projectConfiguration$individualsFile)
  }

  if (!is.na(projectConfiguration$populationsFile)) {
    results$populations <- validatePopulationsFile(projectConfiguration$populationsFile)
  }

  if (!is.na(projectConfiguration$modelParamsFile)) {
    results$models <- validateModelsFile(projectConfiguration$modelParamsFile)
  }

  if (!is.na(projectConfiguration$applicationsFile)) {
    results$applications <- validateApplicationsFile(projectConfiguration$applicationsFile)
  }

  # Add cross-reference validation
  results$crossReferences <- validateCrossReferences(projectConfiguration, results)

  class(results) <- c("ValidationResults", class(results))
  return(results)
}

#' Check if validation results contain any critical errors
#' @param validationResults Output from validateAllConfigurations
#' @return Logical indicating if there are critical errors
#' @export
hasAnyCriticalErrors <- function(validationResults) {
  any(sapply(validationResults, function(r) {
    if (inherits(r, "ValidationResult")) {
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
getValidationSummary <- function(validationResults) {
  summary <- list(
    total_critical_errors = 0,
    total_warnings = 0,
    files_with_errors = character(),
    files_with_warnings = character()
  )

  for (name in names(validationResults)) {
    result <- validationResults[[name]]
    if (inherits(result, "ValidationResult")) {
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
