#' Validate plots configuration file
#' @param filePath Path to plots Excel file
#' @return validationResult object
#' @export
validatePlotsFile <- function(filePath) {
  result <- validationResult$new()

  if (!file.exists(filePath)) {
    result$add_critical_error("File", messages$validationFileNotFound(filePath))
    return(result)
  }

  sheets <- readxl::excel_sheets(filePath)
  required_sheets <- c("DataCombined", "plotConfiguration")
  missing_sheets <- setdiff(required_sheets, sheets)

  if (length(missing_sheets) > 0) {
    result$add_critical_error("Structure", messages$validationMissingSheets(missing_sheets))
    return(result)
  }

  # Read and validate DataCombined
  dataCombined_result <- .safe_validate(
    quote({
      df <- readExcel(filePath, sheet = "DataCombined")
      .validateDataCombinedFromExcel(df, NULL, NULL, FALSE)
    })
  )

  # Copy errors/warnings to main result
  result$critical_errors <- append(result$critical_errors, dataCombined_result$critical_errors)
  result$warnings <- append(result$warnings, dataCombined_result$warnings)

  # Continue only if DataCombined is valid
  if (dataCombined_result$is_valid()) {
    plotConfig_result <- .safe_validate(
      quote({
        df <- readExcel(filePath, sheet = "plotConfiguration")
        dataCombinedNames <- unique(dataCombined_result$data$DataCombinedName)
        .validatePlotConfigurationFromExcel(df, dataCombinedNames)
      })
    )

    result$critical_errors <- append(result$critical_errors, plotConfig_result$critical_errors)
    result$warnings <- append(result$warnings, plotConfig_result$warnings)

    # Check optional sheets
    if ("plotGrids" %in% sheets) {
      .safe_validate(
        quote({
          df <- readExcel(filePath, sheet = "plotGrids")
          plotIDs <- unique(plotConfig_result$data$plotID)
          .validatePlotGridsFromExcel(df, plotIDs)
        }),
        result
      )
    } else {
      result$add_warning("Structure", "Optional sheet 'plotGrids' not found")
    }

    if ("exportConfiguration" %in% sheets) {
      .safe_validate(
        quote({
          df <- readExcel(filePath, sheet = "exportConfiguration")
          .validateExportConfigurationsFromExcel(df, NULL)
        }),
        result
      )
    } else {
      result$add_warning("Structure", "Optional sheet 'exportConfiguration' not found")
    }
  }

  return(result)
}
