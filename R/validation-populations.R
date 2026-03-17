#' Validate populations configuration file
#' @param filePath Path to populations Excel file
#' @return validationResult object
#' @keywords internal
.validatePopulationsFile <- function(filePath) {
  result <- validationResult$new()

  if (!file.exists(filePath)) {
    result$add_critical_error("File", messages$validationFileNotFound(filePath))
    return(result)
  }

  sheets <- readxl::excel_sheets(filePath)

  if (!"Demographics" %in% sheets) {
    result$add_critical_error(
      "Structure",
      "Missing required sheet: Demographics"
    )
    return(result)
  }

  .safe_validate(
    quote({
      df <- readExcel(filePath, sheet = "Demographics")

      required_cols <- c(
        "PopulationName",
        "species",
        "population",
        "numberOfIndividuals",
        "proportionOfFemales",
        "ageMin",
        "ageMax",
        "weightMin",
        "weightMax",
        "heightMin",
        "heightMax",
        "BMIMin",
        "BMIMax"
      )
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        stop(messages$validationMissingColumns("Demographics", missing_cols))
      }

      # Check for columns expected by readPopulationCharacteristicsFromXLS
      extended_cols <- c(
        "weightUnit",
        "heightUnit",
        "BMIUnit",
        "Protein Ontogenies"
      )
      missing_extended <- setdiff(extended_cols, names(df))
      if (length(missing_extended) > 0) {
        result$add_warning(
          "Structure",
          paste0(
            "Demographics sheet is missing optional columns used by ",
            "readPopulationCharacteristicsFromXLS: ",
            paste(missing_extended, collapse = ", ")
          )
        )
      }

      # Check for duplicate PopulationName
      if (any(duplicated(df$PopulationName))) {
        duplicates <- df$PopulationName[duplicated(df$PopulationName)]
        stop(paste(
          "Duplicate PopulationName values:",
          paste(duplicates, collapse = ", ")
        ))
      }

      # Validate proportionOfFemales is between 0 and 1
      if (
        any(
          df$proportionOfFemales < 0 | df$proportionOfFemales > 1,
          na.rm = TRUE
        )
      ) {
        result$add_warning(
          "Data Range",
          "proportionOfFemales should be between 0 and 1"
        )
      }

      df
    }),
    result
  )

  # Validate additional population parameter sheets if they exist
  # These are used by extendPopulationFromXLS
  population_param_sheets <- setdiff(sheets, "Demographics")
  for (sheet in population_param_sheets) {
    .safe_validate(
      quote({
        df <- readExcel(filePath, sheet = sheet)

        if (nrow(df) > 0) {
          # Check for columns expected by extendPopulationFromXLS
          extend_cols <- c(
            "Container Path",
            "Parameter Name",
            "Mean",
            "SD",
            "Distribution"
          )
          missing_extend <- setdiff(extend_cols, names(df))
          if (length(missing_extend) > 0) {
            result$add_warning(
              "Structure",
              paste0(
                "Sheet '", sheet, "' is missing columns expected by ",
                "extendPopulationFromXLS: ",
                paste(missing_extend, collapse = ", ")
              )
            )
          }
        }

        df
      }),
      result
    )
  }

  return(result)
}
