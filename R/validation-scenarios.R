#' Validate scenarios configuration file
#' @param filePath Path to scenarios Excel file
#' @return validationResult object
#' @keywords internal
.validateScenariosFile <- function(filePath) {
  result <- validationResult$new()

  if (!file.exists(filePath)) {
    result$add_critical_error("File", messages$validationFileNotFound(filePath))
    return(result)
  }

  # Check required sheets
  sheets <- readxl::excel_sheets(filePath)
  required_sheets <- c("Scenarios", "OutputPaths")
  missing_sheets <- setdiff(required_sheets, sheets)

  if (length(missing_sheets) > 0) {
    result$add_critical_error(
      "Structure",
      messages$validationMissingSheets(missing_sheets)
    )
    return(result)
  }

  # Validate Scenarios sheet
  .safe_validate(
    quote({
      scenarios_df <- readExcel(filePath, sheet = "Scenarios")

      # Check required columns (subset needed for basic validation)
      required_cols <- c(
        "IndividualId",
        "PopulationId",
        "ApplicationProtocol",
        "SteadyStateTime"
      )
      missing_cols <- setdiff(required_cols, names(scenarios_df))

      if (length(missing_cols) > 0) {
        stop(messages$validationMissingColumns("Scenarios", missing_cols))
      }

      # Full column structure check matching readScenarioConfigurationFromExcel
      expected_full_cols <- c(
        "Scenario_name",
        "IndividualId",
        "PopulationId",
        "ReadPopulationFromCSV",
        "ModelParameterSheets",
        "ApplicationProtocol",
        "SimulationTime",
        "SimulationTimeUnit",
        "SteadyState",
        "SteadyStateTime",
        "SteadyStateTimeUnit",
        "ModelFile",
        "OutputPathsIds"
      )
      missing_full_cols <- setdiff(expected_full_cols, names(scenarios_df))
      if (length(missing_full_cols) > 0) {
        result$add_warning(
          "Structure",
          paste0(
            "Scenarios sheet is missing columns expected by ",
            "readScenarioConfigurationFromExcel: ",
            paste(missing_full_cols, collapse = ", ")
          )
        )
      }

      # Check for empty sheet
      if (nrow(scenarios_df) == 0) {
        result$add_warning("Data", messages$validationEmptySheet("Scenarios"))
      } else {
        # Check Scenario_name uniqueness if column exists
        if ("Scenario_name" %in% names(scenarios_df)) {
          # Filter out empty rows
          valid_rows <- scenarios_df[!is.na(scenarios_df$Scenario_name), ]
          if (nrow(valid_rows) > 0) {
            duplicated_names <- valid_rows$Scenario_name[duplicated(
              valid_rows$Scenario_name
            )]
            if (length(duplicated_names) > 0) {
              result$add_critical_error(
                "Uniqueness",
                messages$stopScenarioNameNonUnique(
                  paste(unique(duplicated_names), collapse = ", ")
                )
              )
            }
          }
        }
      }

      scenarios_df
    }),
    result
  )

  # Validate OutputPaths sheet
  .safe_validate(
    quote({
      output_paths_df <- readExcel(filePath, sheet = "OutputPaths")

      # Check required columns
      required_cols <- c("OutputPathId", "OutputPath")
      missing_cols <- setdiff(required_cols, names(output_paths_df))

      if (length(missing_cols) > 0) {
        stop(messages$validationMissingColumns("OutputPaths", missing_cols))
      }

      # Check for duplicate OutputPathId
      if (any(duplicated(output_paths_df$OutputPathId))) {
        duplicates <- output_paths_df$OutputPathId[duplicated(
          output_paths_df$OutputPathId
        )]
        stop(paste(
          "Duplicate OutputPathId values:",
          paste(duplicates, collapse = ", ")
        ))
      }

      output_paths_df
    }),
    result
  )

  return(result)
}
