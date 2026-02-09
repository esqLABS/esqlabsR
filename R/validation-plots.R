#' Validate plots configuration file
#' @param filePath Path to plots Excel file
#' @return validationResult object
#' @keywords internal
.validatePlotsFile <- function(filePath) {
  result <- validationResult$new()

  if (!file.exists(filePath)) {
    result$add_critical_error("File", messages$validationFileNotFound(filePath))
    return(result)
  }

  sheets <- readxl::excel_sheets(filePath)
  required_sheets <- c("DataCombined", "plotConfiguration")
  missing_sheets <- setdiff(required_sheets, sheets)

  if (length(missing_sheets) > 0) {
    result$add_critical_error(
      "Structure",
      messages$validationMissingSheets(missing_sheets)
    )
    return(result)
  }

  # Validate DataCombined sheet
  .safe_validate(
    quote({
      df <- readExcel(filePath, sheet = "DataCombined")

      # Check required columns
      required_cols <- c("DataCombinedName", "dataType")
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        stop(messages$validationMissingColumns("DataCombined", missing_cols))
      }

      # Check for empty sheet
      if (nrow(df) == 0) {
        result$add_warning(
          "Data",
          messages$validationEmptySheet("DataCombined")
        )
      }

      df
    }),
    result
  )

  # Validate plotConfiguration sheet
  .safe_validate(
    quote({
      df <- readExcel(filePath, sheet = "plotConfiguration")

      # Check required columns
      required_cols <- c("DataCombinedName", "plotID", "plotType")
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        stop(messages$validationMissingColumns(
          "plotConfiguration",
          missing_cols
        ))
      }

      # Check for empty sheet
      if (nrow(df) == 0) {
        result$add_warning(
          "Data",
          messages$validationEmptySheet("plotConfiguration")
        )
      }

      df
    }),
    result
  )

  # Check optional sheets exist (just warn if missing)
  if (!"plotGrids" %in% sheets) {
    result$add_warning("Structure", "Optional sheet 'plotGrids' not found")
  }

  if (!"exportConfiguration" %in% sheets) {
    result$add_warning(
      "Structure",
      "Optional sheet 'exportConfiguration' not found"
    )
  }

  return(result)
}
