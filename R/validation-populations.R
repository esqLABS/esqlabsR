#' Validate populations configuration file
#' @param filePath Path to populations Excel file
#' @return ValidationResult object
#' @export
validatePopulationsFile <- function(filePath) {
  result <- ValidationResult$new()

  if (!file.exists(filePath)) {
    result$add_critical_error("File", messages$validationFileNotFound(filePath))
    return(result)
  }

  sheets <- readxl::excel_sheets(filePath)

  if (!"Demographics" %in% sheets) {
    result$add_critical_error("Structure", "Missing required sheet: Demographics")
    return(result)
  }

  .safe_validate(
    quote({
      df <- readExcel(filePath, sheet = "Demographics")

      required_cols <- c("PopulationName", "species", "population", "numberOfIndividuals",
                        "proportionOfFemales", "ageMin", "ageMax", "weightMin", "weightMax",
                        "heightMin", "heightMax", "BMIMin", "BMIMax")
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        stop(messages$validationMissingColumns("Demographics", missing_cols))
      }

      # Check for duplicate PopulationName
      if (any(duplicated(df$PopulationName))) {
        duplicates <- df$PopulationName[duplicated(df$PopulationName)]
        stop(paste("Duplicate PopulationName values:", paste(duplicates, collapse = ", ")))
      }

      # Validate proportionOfFemales is between 0 and 1
      if (any(df$proportionOfFemales < 0 | df$proportionOfFemales > 1, na.rm = TRUE)) {
        result$add_warning("Data Range", "proportionOfFemales should be between 0 and 1")
      }

      df
    }),
    result
  )

  return(result)
}
