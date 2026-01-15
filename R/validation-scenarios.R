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

      # Check required columns
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

      # Check for empty sheet
      if (nrow(scenarios_df) == 0) {
        result$add_warning("Data", messages$validationEmptySheet("Scenarios"))
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
