#' Validate models parameter file
#' @param filePath Path to models Excel file
#' @return validationResult object
#' @export
validateModelsFile <- function(filePath) {
  result <- validationResult$new()

  if (!file.exists(filePath)) {
    result$add_critical_error("File", messages$validationFileNotFound(filePath))
    return(result)
  }

  sheets <- readxl::excel_sheets(filePath)

  if (length(sheets) == 0) {
    result$add_critical_error("Structure", "Models file must contain at least one sheet")
    return(result)
  }

  # Validate each sheet has required structure
  for (sheet in sheets) {
    .safe_validate(
      quote({
        df <- readExcel(filePath, sheet = sheet)

        # Check if sheet is empty
        if (nrow(df) == 0 || ncol(df) == 0) {
          result$add_warning("Data", messages$validationEmptySheet(sheet))
        } else {
          # Models sheets should have parameter paths and values
          if (!"paths" %in% names(df) && !"ParameterPath" %in% names(df)) {
            result$add_warning("Structure",
              paste0("Sheet '", sheet, "' may be missing parameter path column"))
          }
        }

        df
      }),
      result
    )
  }

  return(result)
}

#' Validate applications configuration file
#' @param filePath Path to applications Excel file
#' @return validationResult object
#' @export
validateApplicationsFile <- function(filePath) {
  result <- validationResult$new()

  if (!file.exists(filePath)) {
    result$add_critical_error("File", messages$validationFileNotFound(filePath))
    return(result)
  }

  sheets <- readxl::excel_sheets(filePath)

  if (length(sheets) == 0) {
    result$add_critical_error("Structure", "Applications file must contain at least one sheet")
    return(result)
  }

  # Each sheet represents an application protocol
  for (sheet in sheets) {
    .safe_validate(
      quote({
        df <- readExcel(filePath, sheet = sheet)

        if (nrow(df) == 0 || ncol(df) == 0) {
          result$add_warning("Data", messages$validationEmptySheet(sheet))
        }

        df
      }),
      result
    )
  }

  return(result)
}
