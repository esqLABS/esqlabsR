#' Validate project configuration file
#' @param projectConfigPath Path to ProjectConfiguration.xlsx
#' @return validationResult object
#' @keywords internal
.validateProjectConfiguration <- function(projectConfigPath) {
  result <- validationResult$new()

  # Check file exists
  if (!file.exists(projectConfigPath)) {
    result$add_critical_error("File", messages$validationFileNotFound(projectConfigPath))
    return(result)
  }

  # Try to load project configuration directly (not using .safe_validate due to scoping issues)
  tryCatch({
    withCallingHandlers({
      config <- createDefaultProjectConfiguration(path = projectConfigPath)

      # Check all referenced files exist
      files_to_check <- list(
        scenarios = config$scenariosFile,
        individuals = config$individualsFile,
        populations = config$populationsFile,
        models = config$modelParamsFile,
        applications = config$applicationsFile,
        plots = config$plotsFile
      )

      for (name in names(files_to_check)) {
        if (!is.na(files_to_check[[name]]) && !file.exists(files_to_check[[name]])) {
          result$add_warning(
            "File Reference",
            paste0("Referenced ", name, " file not found: ", files_to_check[[name]])
          )
        }
      }

      result$set_data(config)
    },
    warning = function(w) {
      category <- .categorize_message(conditionMessage(w))
      result$add_warning(category, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  },
  error = function(e) {
    category <- .categorize_message(conditionMessage(e))
    result$add_critical_error(category, conditionMessage(e))
  })

  return(result)
}
