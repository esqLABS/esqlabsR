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

  # Read DataCombined sheet upfront so cross-reference checks can use it
  dc_df <- tryCatch(
    readExcel(filePath, sheet = "DataCombined"),
    error = function(e) NULL
  )

  # Validate DataCombined sheet
  if (is.null(dc_df)) {
    result$add_critical_error(
      "Structure",
      "Failed to read DataCombined sheet"
    )
  } else {
    # Check required columns
    required_cols <- c("DataCombinedName", "dataType")
    missing_cols <- setdiff(required_cols, names(dc_df))

    if (length(missing_cols) > 0) {
      result$add_critical_error(
        "Structure",
        messages$validationMissingColumns("DataCombined", missing_cols)
      )
    } else if (nrow(dc_df) == 0) {
      result$add_warning(
        "Data",
        messages$validationEmptySheet("DataCombined")
      )
    } else {
      # Check mandatory 'label' column exists and has no missing values
      if (!("label" %in% names(dc_df))) {
        result$add_critical_error(
          "Structure",
          messages$validationMissingColumns("DataCombined", "label")
        )
      } else if (any(is.na(dc_df$label))) {
        result$add_critical_error(
          "Missing Fields",
          messages$missingLabel()
        )
      }

      # Check mandatory 'dataType' column has no missing values
      if (any(is.na(dc_df$dataType))) {
        result$add_critical_error(
          "Missing Fields",
          messages$missingDataType()
        )
      }

      # Check simulated rows have scenario and path defined
      simulated_rows <- dc_df[
        dc_df$dataType == "simulated" & !is.na(dc_df$dataType),
      ]
      if (nrow(simulated_rows) > 0) {
        if (!("scenario" %in% names(dc_df))) {
          result$add_critical_error(
            "Structure",
            messages$validationMissingColumns("DataCombined", "scenario")
          )
        } else if (any(is.na(simulated_rows$scenario))) {
          result$add_critical_error(
            "Missing Fields",
            messages$missingScenarioName()
          )
        }

        if (!("path" %in% names(dc_df))) {
          result$add_critical_error(
            "Structure",
            messages$validationMissingColumns("DataCombined", "path")
          )
        } else if (any(is.na(simulated_rows$path))) {
          missing_dc <- simulated_rows$DataCombinedName[is.na(
            simulated_rows$path
          )]
          result$add_critical_error(
            "Missing Fields",
            messages$stopNoPathProvided(missing_dc)
          )
        }
      }

      # Check observed rows have dataSet defined
      observed_rows <- dc_df[
        dc_df$dataType == "observed" & !is.na(dc_df$dataType),
      ]
      if (nrow(observed_rows) > 0) {
        if (!("dataSet" %in% names(dc_df))) {
          result$add_critical_error(
            "Structure",
            messages$validationMissingColumns("DataCombined", "dataSet")
          )
        } else if (any(is.na(observed_rows$dataSet))) {
          missing_dc <- observed_rows$DataCombinedName[is.na(
            observed_rows$dataSet
          )]
          result$add_critical_error(
            "Missing Fields",
            messages$stopNoDataSetProvided(missing_dc)
          )
        }
      }
    }
  }

  # Read plotConfiguration sheet upfront
  plot_df <- tryCatch(
    readExcel(filePath, sheet = "plotConfiguration"),
    error = function(e) NULL
  )

  # Validate plotConfiguration sheet
  if (is.null(plot_df)) {
    result$add_critical_error(
      "Structure",
      "Failed to read plotConfiguration sheet"
    )
  } else {
    # Check required columns
    required_cols <- c("DataCombinedName", "plotID", "plotType")
    missing_cols <- setdiff(required_cols, names(plot_df))

    if (length(missing_cols) > 0) {
      result$add_critical_error(
        "Structure",
        messages$validationMissingColumns("plotConfiguration", missing_cols)
      )
    } else if (nrow(plot_df) == 0) {
      result$add_warning(
        "Data",
        messages$validationEmptySheet("plotConfiguration")
      )
    } else {
      # Check mandatory 'DataCombinedName' has no missing values
      if (any(is.na(plot_df$DataCombinedName))) {
        result$add_critical_error(
          "Missing Fields",
          messages$missingDataCombinedName()
        )
      }

      # Check mandatory 'plotType' has no missing values
      if (any(is.na(plot_df$plotType))) {
        result$add_critical_error(
          "Missing Fields",
          messages$missingPlotType()
        )
      }

      # Check plotID uniqueness
      duplicated_plotIDs <- plot_df$plotID[duplicated(plot_df$plotID)]
      if (length(duplicated_plotIDs) > 0) {
        result$add_critical_error(
          "Uniqueness",
          messages$PlotIDsMustBeUnique(duplicated_plotIDs)
        )
      }

      # Check DataCombinedName references exist in DataCombined sheet
      if (!is.null(dc_df) && nrow(dc_df) > 0) {
        dc_names <- unique(dc_df$DataCombinedName)
        missing_dc <- setdiff(
          setdiff(plot_df$DataCombinedName, dc_names),
          NA
        )
        if (length(missing_dc) > 0) {
          result$add_critical_error(
            "Invalid Reference",
            messages$stopInvalidDataCombinedName(missing_dc)
          )
        }
      }
    }
  }

  # Validate plotGrids sheet if present
  if ("plotGrids" %in% sheets) {
    grids_df <- tryCatch(
      readExcel(filePath, sheet = "plotGrids"),
      error = function(e) NULL
    )

    if (!is.null(grids_df) && nrow(grids_df) > 0) {
      # Check required columns for plotGrids
      grids_required_cols <- c("name", "plotIDs")
      grids_missing_cols <- setdiff(grids_required_cols, names(grids_df))
      if (length(grids_missing_cols) > 0) {
        result$add_critical_error(
          "Structure",
          messages$validationMissingColumns("plotGrids", grids_missing_cols)
        )
      } else {
        # Check mandatory 'plotIDs' has no missing values
        if (any(is.na(grids_df$plotIDs))) {
          result$add_critical_error(
            "Missing Fields",
            messages$missingPlotIDs()
          )
        }

        # Check plotGrids name uniqueness
        duplicated_names <- grids_df$name[duplicated(grids_df$name)]
        if (length(duplicated_names) > 0) {
          result$add_critical_error(
            "Uniqueness",
            messages$PlotGridsNamesMustBeUnique(duplicated_names)
          )
        }

        # Check plotIDs reference valid plots
        if (!is.null(plot_df) && "plotID" %in% names(plot_df)) {
          all_grid_plotIDs <- unlist(lapply(grids_df$plotIDs, \(plotId) {
            unlist(trimws(scan(
              text = as.character(plotId),
              what = "character",
              sep = ",",
              quiet = TRUE
            )))
          }))
          missing_plots <- setdiff(
            setdiff(unique(all_grid_plotIDs), plot_df$plotID),
            NA
          )
          if (length(missing_plots) > 0) {
            result$add_critical_error(
              "Invalid Reference",
              messages$errorInvalidPlotID(missing_plots)
            )
          }
        }
      }
    }
  } else {
    result$add_warning("Structure", "Optional sheet 'plotGrids' not found")
  }

  # Validate exportConfiguration sheet if present
  if ("exportConfiguration" %in% sheets) {
    export_df <- tryCatch(
      readExcel(filePath, sheet = "exportConfiguration"),
      error = function(e) NULL
    )

    if (!is.null(export_df) && nrow(export_df) > 0) {
      # Check required columns for exportConfiguration
      export_required_cols <- c("name")
      export_missing_cols <- setdiff(export_required_cols, names(export_df))
      if (length(export_missing_cols) > 0) {
        result$add_critical_error(
          "Structure",
          messages$validationMissingColumns(
            "exportConfiguration", export_missing_cols
          )
        )
      } else if (any(is.na(export_df$name))) {
        result$add_warning(
          "Missing Fields",
          messages$missingOutputFileName()
        )
      }
    }
  } else {
    result$add_warning(
      "Structure",
      "Optional sheet 'exportConfiguration' not found"
    )
  }

  return(result)
}
