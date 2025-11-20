#' Validate individuals configuration file
#' @param filePath Path to individuals Excel file
#' @return ValidationResult object
#' @export
validateIndividualsFile <- function(filePath) {
  result <- ValidationResult$new()

  if (!file.exists(filePath)) {
    result$add_critical_error("File", messages$validationFileNotFound(filePath))
    return(result)
  }

  sheets <- readxl::excel_sheets(filePath)

  if (!"IndividualBiometrics" %in% sheets) {
    result$add_critical_error("Structure", "Missing required sheet: IndividualBiometrics")
    return(result)
  }

  .safe_validate(
    quote({
      df <- readExcel(filePath, sheet = "IndividualBiometrics")

      required_cols <- c("IndividualId", "Species", "Population", "Gender",
                        "Age [year(s)]", "Height [cm]", "Weight [kg]")
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        stop(messages$validationMissingColumns("IndividualBiometrics", missing_cols))
      }

      # Check for duplicate IndividualId
      if (any(duplicated(df$IndividualId))) {
        duplicates <- df$IndividualId[duplicated(df$IndividualId)]
        stop(paste("Duplicate IndividualId values:", paste(duplicates, collapse = ", ")))
      }

      # Validate numeric columns
      numeric_cols <- c("Age [year(s)]", "Height [cm]", "Weight [kg]")
      for (col in numeric_cols) {
        if (!is.numeric(df[[col]])) {
          result$add_warning("Data Type", paste0("Column '", col, "' should be numeric"))
        }
      }

      df
    }),
    result
  )

  return(result)
}
