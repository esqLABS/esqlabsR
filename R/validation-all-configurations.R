#' Validate all configuration files in a project
#' @param project Project object or path to Project.xlsx
#' @return Named list of validationResult objects
#' @export
validateAllConfigurations <- function(project) {
  results <- list()

  # Handle both path and object input
  if (is.character(project)) {
    results$project <- .validateProject(
      project
    )
    if (!results$project$is_valid()) {
      class(results) <- c("ValidationResults", class(results))
      return(results)
    }
    project <- loadProject(
      path = project
    )
  }

  # Validate each configuration file
  if (!is.na(project$scenariosFile)) {
    results$scenarios <- .validateScenariosFile(
      project$scenariosFile
    )
  }

  if (!is.na(project$plotsFile)) {
    results$plots <- .validatePlotsFile(project$plotsFile)
  }

  if (!is.na(project$individualsFile)) {
    results$individuals <- .validateIndividualsFile(
      project$individualsFile
    )
  }

  if (!is.na(project$populationsFile)) {
    results$populations <- .validatePopulationsFile(
      project$populationsFile
    )
  }

  if (!is.na(project$modelParamsFile)) {
    results$models <- .validateModelsFile(project$modelParamsFile)
  }

  if (!is.na(project$applicationsFile)) {
    results$applications <- .validateApplicationsFile(
      project$applicationsFile
    )
  }

  # Add cross-reference validation
  results$crossReferences <- .validateCrossReferences(
    project,
    results
  )

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
        summary$total_warnings <- summary$total_warnings +
          length(result$warnings)
        summary$files_with_warnings <- c(summary$files_with_warnings, name)
      }
    }
  }

  summary
}
