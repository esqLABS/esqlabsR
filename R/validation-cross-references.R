#' Validate cross-references between configuration files
#' @param projectConfiguration ProjectConfiguration object
#' @param validationResults Previous validation results
#' @return validationResult object with cross-reference validation
#' @keywords internal
#' @export
validateCrossReferences <- function(projectConfiguration, validationResults) {
  result <- validationResult$new()

  # Skip if previous validations failed
  if (isAnyCriticalErrors(validationResults)) {
    result$add_warning("Skipped", "Cross-reference validation skipped due to critical errors")
    return(result)
  }

  # Check scenarios reference valid individuals and populations
  if (!is.null(validationResults$scenarios) && validationResults$scenarios$is_valid()) {
    scenarios_data <- validationResults$scenarios$data

    # Check IndividualId references
    if (!is.null(validationResults$individuals) && validationResults$individuals$is_valid()) {
      individuals_data <- validationResults$individuals$data
      invalid_individuals <- setdiff(
        scenarios_data$IndividualId,
        individuals_data$IndividualId
      )
      if (length(invalid_individuals) > 0) {
        result$add_critical_error(
          "Invalid Reference",
          messages$validationCrossReference(
            "Scenarios",
            "IndividualBiometrics",
            invalid_individuals
          )
        )
      }
    }

    # Check PopulationId references
    if (!is.null(validationResults$populations) && validationResults$populations$is_valid()) {
      populations_data <- validationResults$populations$data
      invalid_populations <- setdiff(
        scenarios_data$PopulationId,
        populations_data$PopulationName
      )
      if (length(invalid_populations) > 0) {
        result$add_critical_error(
          "Invalid Reference",
          messages$validationCrossReference(
            "Scenarios",
            "Demographics",
            invalid_populations
          )
        )
      }
    }
  }

  # Check plots reference valid scenarios
  if (!is.null(validationResults$plots) && validationResults$plots$is_valid() &&
      !is.null(validationResults$scenarios) && validationResults$scenarios$is_valid()) {

    plots_data <- validationResults$plots$data
    scenarios_data <- validationResults$scenarios$data

    # Get scenario names from DataCombined sheet
    if (!is.null(plots_data$DataCombined)) {
      simulated_data <- plots_data$DataCombined[
        plots_data$DataCombined$dataType == "simulated",
      ]

      invalid_scenarios <- setdiff(
        simulated_data$scenario,
        scenarios_data$ScenarioName
      )

      if (length(invalid_scenarios) > 0) {
        result$add_critical_error(
          "Invalid Reference",
          messages$validationCrossReference(
            "DataCombined",
            "Scenarios",
            invalid_scenarios
          )
        )
      }
    }
  }

  return(result)
}
