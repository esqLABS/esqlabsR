# Project sync and validation ----

#' Check synchronization status of a Project
#' @param project A `Project` object.
#' @param silent Logical. If `TRUE`, suppresses messages.
#' @returns A list with sync status details.
#' @keywords internal
#' @noRd
.projectSync <- function(project, silent = FALSE) {
  result <- list(
    in_sync = TRUE,
    unsaved_changes = FALSE,
    json_modified = FALSE,
    excel_modified = FALSE,
    details = list()
  )

  jsonPath <- project$jsonPath
  if (is.null(jsonPath) || !file.exists(jsonPath)) {
    result$in_sync <- project$modified == FALSE
    result$unsaved_changes <- project$modified

    # Even without a JSON file, sibling Excel files may exist; flag those as
    # excel_modified relative to the absent JSON so callers don't get a false
    # in_sync = TRUE.
    if (!is.null(jsonPath)) {
      excelPath <- sub("\\.json$", ".xlsx", jsonPath)
      if (file.exists(excelPath)) {
        result$excel_modified <- TRUE
        result$in_sync <- FALSE
      }
    }

    if (!silent && result$unsaved_changes) {
      message("Project has unsaved changes (no JSON file to compare).")
    }
    return(invisible(result))
  }

  if (project$modified) {
    result$unsaved_changes <- TRUE
    result$in_sync <- FALSE
  } else {
    fileProject <- loadProject(jsonPath)
    currentJson <- jsonlite::toJSON(
      .projectToJson(project),
      auto_unbox = TRUE,
      null = "null"
    )
    fileJson <- jsonlite::toJSON(
      .projectToJson(fileProject),
      auto_unbox = TRUE,
      null = "null"
    )

    if (!identical(currentJson, fileJson)) {
      result$json_modified <- TRUE
      result$in_sync <- FALSE
    }
  }

  excelPath <- sub("\\.json$", ".xlsx", jsonPath)
  if (file.exists(excelPath)) {
    excelStatus <- tryCatch(
      projectStatus(
        projectConfigPath = excelPath,
        jsonPath = jsonPath,
        silent = TRUE
      ),
      error = function(e) list(in_sync = TRUE)
    )
    if (!isTRUE(excelStatus$in_sync)) {
      result$excel_modified <- TRUE
      result$in_sync <- FALSE
      result$details$excel <- excelStatus$details
    }
  }

  if (!silent) {
    if (result$in_sync) {
      message("Project is in sync with all source files.")
    } else {
      if (result$unsaved_changes) {
        cli::cli_alert_warning("In-memory changes not saved to JSON.")
      }
      if (result$json_modified) {
        cli::cli_alert_warning("JSON file has been modified externally.")
      }
      if (result$excel_modified) {
        cli::cli_alert_warning("Excel files differ from JSON.")
      }
    }
  }

  invisible(result)
}

#' Validate project file
#' @param projectConfigPath Path to Project.xlsx
#' @return validationResult object
#' @keywords internal
.validateProject <- function(projectConfigPath) {
  result <- validationResult$new()

  # Check file exists
  if (!file.exists(projectConfigPath)) {
    result$add_critical_error(
      "File",
      messages$validationFileNotFound(projectConfigPath)
    )
    return(result)
  }

  # Try to load project configuration directly (not using .safe_validate due to scoping issues)
  tryCatch(
    {
      withCallingHandlers(
        {
          config <- loadProject(path = projectConfigPath)

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
            if (
              !is.na(files_to_check[[name]]) &&
                !file.exists(files_to_check[[name]])
            ) {
              result$add_warning(
                "File Reference",
                paste0(
                  "Referenced ",
                  name,
                  " file not found: ",
                  files_to_check[[name]]
                )
              )
            }
          }

          result$set_data(config)
        },
        warning = function(w) {
          category <- .categorize_message(conditionMessage(w))
          result$add_warning(category, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      category <- .categorize_message(conditionMessage(e))
      result$add_critical_error(category, conditionMessage(e))
    }
  )

  return(result)
}
