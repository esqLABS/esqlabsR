#' Export project configuration Excel files to JSON
#'
#' @description Exports all Excel configuration files in the Configurations
#' folder of an esqlabsR project to a single JSON file. This allows for easier
#' version control and programmatic manipulation.
#'
#' @family project configuration snapshots
#' @param projectConfig A ProjectConfiguration object or path to
#'   ProjectConfiguration excel file. Defaults to "ProjectConfiguration.xlsx".
#' @param outputDir Directory where the JSON file will be saved. If NULL
#'   (default), the JSON file will be created in the same directory as the
#'   source Excel file.
#'
#' @return Invisibly returns the exported configuration data structure
#' @export
snapshotProjectConfiguration <- function(
  projectConfig = "ProjectConfiguration.xlsx",
  outputDir = NULL,
  ...
) {
  extraArguments <- list(...)
  # Convert to ProjectConfiguration object if path is provided
  if (is.character(projectConfig)) {
    projectConfig <- createProjectConfiguration(projectConfig)
  }

  # Validate projectConfig
  if (!inherits(projectConfig, "ProjectConfiguration")) {
    stop(
      "projectConfig must be a ProjectConfiguration object or valid path to ProjectConfiguration.xlsx"
    )
  }

  # Determine output filename and path based on source excel filename
  if (!is.null(projectConfig$projectConfigurationFilePath)) {
    sourceFileName <- basename(projectConfig$projectConfigurationFilePath)
    # If outputDir is NULL, use the same directory as the source file
    if (is.null(outputDir)) {
      outputDir <- dirname(projectConfig$projectConfigurationFilePath)
    }
    outputFileName <- sub("\\.xlsx$", ".json", sourceFileName)
  } else {
    outputFileName <- "ProjectConfiguration.json"
    # If outputDir is NULL and no projectConfigurationFilePath, use current directory
    if (is.null(outputDir)) {
      outputDir <- "."
    }
  }

  outputPath <- file.path(outputDir, outputFileName)

  # Ensure the directory exists, create it if it doesn't
  outputDir <- dirname(outputPath)
  if (!dir.exists(outputDir) && outputDir != "") {
    dir.create(outputDir, recursive = TRUE)
  }

  # Create the main config structure
  configData <- list()

  # Add ProjectConfiguration.xlsx data
  if (
    !is.null(projectConfig$projectConfigurationFilePath) &&
      file.exists(projectConfig$projectConfigurationFilePath)
  ) {
    configData$projectConfiguration <- .excelToListStructure(
      projectConfig$projectConfigurationFilePath
    )
  }

  # Define files to export
  excelFiles <- list(
    modelParameterSets = projectConfig$modelParamsFile,
    Individuals = projectConfig$individualsFile,
    Populations = projectConfig$populationsFile,
    Scenarios = projectConfig$scenariosFile,
    Applications = projectConfig$applicationsFile,
    Plots = projectConfig$plotsFile
  )

  # Read each Excel file and store its sheets
  for (name in names(excelFiles)) {
    filePath <- excelFiles[[name]]
    if (!is.na(filePath) && file.exists(filePath)) {
      # Get sheets
      sheets <- readxl::excel_sheets(filePath)

      # Create a container for this file's sheets
      configData[[name]] <- list()

      # Process each sheet
      for (sheet in sheets) {
        # Read the data and convert to list structure
        configData[[name]][[sheet]] <- .excelToListStructure(filePath, sheet)
      }
    }
  }

  # Handle CSV files in populations folder if it exists
  if (
    !is.na(projectConfig$populationsFolder) &&
      dir.exists(projectConfig$populationsFolder)
  ) {
    # Find CSV files
    csvFiles <- list.files(
      projectConfig$populationsFolder,
      pattern = "\\.csv$",
      full.names = TRUE
    )

    if (length(csvFiles) > 0) {
      configData$populationsCSV <- list()

      for (csvFile in csvFiles) {
        fileName <- basename(csvFile)
        # Read CSV and convert to list structure
        configData$populationsCSV[[fileName]] <- .csvToListStructure(csvFile)
      }
    }
  }

  # Convert to JSON
  jsonData <- jsonlite::toJSON(
    configData,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA
  )

  # Ensure output directory exists
  dirPath <- dirname(outputPath)
  if (!dir.exists(dirPath)) {
    dir.create(dirPath, recursive = TRUE)
  }

  # Write to file
  writeLines(jsonData, outputPath)

  # Display message with relative path
  inputFile <- fs::path_rel(
    projectConfig$projectConfigurationFilePath,
    start = getwd()
  )

  # Get relative path from working directory
  outputFile <- fs::path_rel(outputPath, start = getwd())

  if (interactive() && !isTRUE(extraArguments$silent)) {
    cli::cli_alert_success(
      "Snapshot of {.file {inputFile}} created at {.file {outputFile}}"
    )
  }

  invisible(configData)
}

#' Import project configuration from JSON to Excel files
#'
#' @description Creates Excel configuration files from a JSON configuration
#' file. This allows for recreating the project configuration from
#' version-controlled JSON.
#'
#' @family project configuration snapshots
#'
#' @param jsonPath Path to the JSON configuration file. Defaults to
#'   "ProjectConfiguration.json".
#' @param outputDir Directory where the Excel files will be created. If NULL
#'   (default), the Excel files will be created in the same directory as the
#'   source JSON file.
#'
#' @return A ProjectConfiguration object initialized with the regenerated
#'   ProjectConfiguration.xlsx
#' @export
restoreProjectConfiguration <- function(
  jsonPath = "ProjectConfiguration.json",
  outputDir = NULL
) {
  # Check if JSON file exists
  if (!file.exists(jsonPath)) {
    stop("JSON file does not exist: ", jsonPath)
  }

  # If outputDir is NULL, use the same directory as the source JSON file
  if (is.null(outputDir)) {
    outputDir <- dirname(jsonPath)
  }

  # Determine Excel filename based on source JSON filename
  jsonFilename <- basename(jsonPath)
  excelFilename <- sub("\\.json$", ".xlsx", jsonFilename)
  projConfigPath <- file.path(outputDir, excelFilename)

  # Only check sync if Excel file exists
  if (file.exists(projConfigPath)) {
    status <- projectConfigurationStatus(
      projConfigPath,
      jsonPath,
      silent = TRUE
    )
    if (!isTRUE(status$in_sync)) {
      cli::cli_alert_warning(
        "The Excel configuration files are NOT in sync with the JSON snapshot.
      Restoring will OVERWRITE the Excel files and you may lose unsaved work."
      )

      if (interactive()) {
        qs <- sample(c("Absolutely not", "Yes", "No way"))
        out <- utils::menu(
          title = "Are you sure you want to continue?",
          choices = qs
        )
        if (out == 0L || qs[[out]] != "Yes") {
          cli::cli_abort("The function was aborted by the user.")
        }
      }
    }
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = TRUE)
  }

  # Create Configurations directory
  configDir <- file.path(outputDir, "Configurations")
  if (!dir.exists(configDir)) {
    dir.create(configDir, recursive = TRUE, showWarnings = FALSE)
  }

  # Load JSON data
  configData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)

  # Determine Excel filename based on source JSON filename
  jsonFilename <- basename(jsonPath)
  excelFilename <- sub("\\.json$", ".xlsx", jsonFilename)

  # Path to save ProjectConfiguration.xlsx
  projConfigPath <- file.path(outputDir, excelFilename)

  # Handle ProjectConfiguration.xlsx if present
  if ("projectConfiguration" %in% names(configData)) {
    projConfigData <- configData$projectConfiguration

    # Get column names
    columnNames <- projConfigData$column_names

    # Create an empty data frame with the correct structure
    projConfigDf <- data.frame(matrix(
      ncol = length(columnNames),
      nrow = length(projConfigData$rows)
    ))
    colnames(projConfigDf) <- columnNames

    # Fill in data if we have rows
    if (length(projConfigData$rows) > 0) {
      for (i in seq_along(projConfigData$rows)) {
        rowData <- projConfigData$rows[[i]]

        # Fill in each column
        for (j in seq_along(columnNames)) {
          if (j <= length(rowData)) {
            projConfigDf[i, j] <- rowData[[j]] %||% ""
          }
        }
      }

      # Convert data types to match the originals
      projConfigDf <- .convertDataTypes(projConfigDf)
    }

    # Write ProjectConfiguration.xlsx file to the root directory, not inside Configurations
    .writeExcel(projConfigDf, projConfigPath)
  } else {
    # If no ProjectConfiguration in JSON, create a basic one
    warning("No ProjectConfiguration found in JSON file. Creating a basic one.")

    # Create a basic ProjectConfiguration dataframe
    projConfigDf <- data.frame(
      Property = c(
        "modelFolder",
        "configurationsFolder",
        "outputFolder"
      ),
      Value = c(
        file.path(outputDir, "Models"),
        file.path(outputDir, "Configurations"),
        file.path(outputDir, "Results")
      ),
      Description = c(
        "Path to folder containing model files",
        "Path to folder containing configuration files",
        "Path to folder for simulation results"
      )
    )

    # Write the basic ProjectConfiguration
    .writeExcel(projConfigDf, projConfigPath)
  }

  # Map file names to their Excel filenames
  fileMapping <- list(
    modelParameterSets = "ModelParameters.xlsx",
    Individuals = "Individuals.xlsx",
    Populations = "Populations.xlsx",
    Scenarios = "Scenarios.xlsx",
    Applications = "Applications.xlsx",
    Plots = "Plots.xlsx"
  )

  # Process each Excel file
  for (name in names(fileMapping)) {
    if (name %in% names(configData)) {
      # Get the sheets for this file
      sheetsData <- configData[[name]]
      excelSheets <- list()

      # Process each sheet
      for (sheetName in names(sheetsData)) {
        sheetInfo <- sheetsData[[sheetName]]

        # Get column names
        columnNames <- sheetInfo$column_names

        # Create an empty data frame with the correct structure
        df <- data.frame(matrix(
          ncol = length(columnNames),
          nrow = length(sheetInfo$rows)
        ))
        colnames(df) <- columnNames

        # Fill in data if we have rows
        if (length(sheetInfo$rows) > 0) {
          for (i in seq_along(sheetInfo$rows)) {
            rowData <- sheetInfo$rows[[i]]

            # Fill in each column
            for (j in seq_along(columnNames)) {
              if (j <= length(rowData)) {
                df[i, j] <- rowData[[j]] %||% ""
              }
            }
          }

          # Convert data types to match the originals
          df <- .convertDataTypes(df)
        }

        # Add to the list of sheets
        excelSheets[[sheetName]] <- df
      }

      # Write the Excel file if we have data
      if (length(excelSheets) > 0) {
        excelPath <- file.path(configDir, fileMapping[[name]])
        .writeExcel(excelSheets, excelPath)
      }
    }
  }

  # Handle populations CSV folder
  if ("populationsCSV" %in% names(configData)) {
    csvDir <- file.path(configDir, "PopulationsCSV")
    if (!dir.exists(csvDir)) {
      dir.create(csvDir, recursive = TRUE, showWarnings = FALSE)
    }

    # Get CSV files
    csvFiles <- configData$populationsCSV

    # Process each CSV file
    for (fileName in names(csvFiles)) {
      csvInfo <- csvFiles[[fileName]]

      # Get column names
      columnNames <- csvInfo$column_names

      # Create empty data frame
      csvDf <- data.frame(matrix(
        ncol = length(columnNames),
        nrow = length(csvInfo$rows)
      ))
      colnames(csvDf) <- columnNames

      # Fill in data if we have rows
      if (length(csvInfo$rows) > 0) {
        for (i in seq_along(csvInfo$rows)) {
          rowData <- csvInfo$rows[[i]]

          # Fill in each column
          for (j in seq_along(columnNames)) {
            if (j <= length(rowData)) {
              csvDf[i, j] <- rowData[[j]]
            }
          }
        }
      }

      # Write CSV file
      csvPath <- file.path(csvDir, fileName)
      utils::write.csv(csvDf, csvPath, row.names = FALSE)
    }
  }

  # Get relative path from working directory for the output message
  relPath <- fs::path_rel(projConfigPath, start = getwd())

  # Display message with relative path
  if (interactive()) {
    cli::cli_alert_success(
      "Project configuration from {.file {jsonPath}} restored at {.file {relPath}}"
    )
  }

  # Create and return a ProjectConfiguration object
  invisible(createProjectConfiguration(projConfigPath))
}

#' Check if Excel configuration files are in sync with JSON snapshot
#'
#' @description Compares Excel configuration files against their JSON snapshot
#' to determine if they are synchronized. The JSON snapshot is considered the
#' source of truth.
#'
#' @param projectConfig A ProjectConfiguration object or path to
#'   ProjectConfiguration excel file. Defaults to "ProjectConfiguration.xlsx".
#' @param jsonPath Path to the JSON configuration file. If NULL (default), the
#'   function will look for a JSON file with the same name as the
#'   ProjectConfiguration file but with .json extension.
#'
#' @family project configuration snapshots
#' @return A list with components: \item{in_sync}{Logical indicating whether all
#'   files are synchronized} \item{details}{A list with detailed comparison
#'   results for each file} \item{unsaved_changes}{Logical indicating whether
#'   the ProjectConfiguration object has unsaved modifications}
#'
#' @import cli
#' @export
projectConfigurationStatus <- function(
  projectConfig = "ProjectConfiguration.xlsx",
  jsonPath = NULL,
  silent = FALSE
) {
  # Convert to ProjectConfiguration object if path is provided
  if (is.character(projectConfig)) {
    projectConfig <- createProjectConfiguration(projectConfig)
  }

  # Validate projectConfig
  if (!inherits(projectConfig, "ProjectConfiguration")) {
    stop(
      "projectConfig must be a ProjectConfiguration object or valid path to ProjectConfiguration.xlsx"
    )
  }

  # Check if the project configuration has been modified
  hasUnsavedChanges <- projectConfig$modified
  if (hasUnsavedChanges && !silent) {
    cli::cli_warn(
      c(
        "!" = "The ProjectConfiguration object has been modified since loading from file.",
        "i" = "The object properties don't match the original Excel file.",
        ">" = "Consider running {.run projectConfig$save()} to save changes to the Excel file."
      )
    )
  }

  # Determine JSON path if not provided
  if (is.null(jsonPath)) {
    if (!is.null(projectConfig$projectConfigurationFilePath)) {
      jsonPath <- sub(
        "\\.xlsx$",
        ".json",
        projectConfig$projectConfigurationFilePath
      )
    } else {
      jsonPath <- "ProjectConfiguration.json"
    }
  }

  # Check if JSON file exists
  if (!file.exists(jsonPath)) {
    stop("JSON file does not exist: ", jsonPath)
  }

  # Create temporary directory for snapshot
  tempDir <- tempfile("config_snapshot")
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  # Create current snapshot from Excel files
  tempJsonPath <- file.path(tempDir, basename(jsonPath))
  snapshotProjectConfiguration(
    projectConfig,
    outputDir = tempDir,
    silent = TRUE
  )

  # Load both JSON files as strings first for a quick initial check
  originalJson <- readLines(jsonPath, warn = FALSE)
  currentJson <- readLines(tempJsonPath, warn = FALSE)

  # Simple string comparison to check if files are identical
  if (identical(originalJson, currentJson)) {
    # Files are identical
    result <- list(
      in_sync = TRUE,
      details = list(),
      unsaved_changes = hasUnsavedChanges
    )

    # Display message if interactive
    if (hasUnsavedChanges && !silent) {
      cli::cli_alert_success(
        "Excel configuration files are in sync with JSON snapshot."
      )
      cli::cli_alert_info(
        "However, the ProjectConfiguration object has {.strong unsaved changes} that differ from the Excel file."
      )
    } else {
      cli::cli_alert_success(
        "Excel configuration files are in sync with JSON snapshot."
      )
    }
  } else {
    # Files are different, now do a detailed comparison
    originalJsonObj <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
    currentJsonObj <- jsonlite::fromJSON(tempJsonPath, simplifyVector = FALSE)

    # Initialize detailed difference tracking
    fileChanges <- list()
    sheetChanges <- list()
    dataChanges <- list()

    # Keep track of file status for summary display
    fileStatus <- list()

    # 1. Check for differences in Excel files (added/removed files)
    originalFiles <- names(originalJsonObj)
    currentFiles <- names(currentJsonObj)

    # Files missing in current Excel compared to snapshot
    missingFiles <- setdiff(originalFiles, currentFiles)
    if (length(missingFiles) > 0) {
      for (file in missingFiles) {
        fileChanges[[file]] <- "Excel file is missing but exists in snapshot"
        fileStatus[[file]] <- "out-of-sync"
      }
    }

    # Files added in current Excel compared to snapshot
    addedFiles <- setdiff(currentFiles, originalFiles)
    if (length(addedFiles) > 0) {
      for (file in addedFiles) {
        fileChanges[[file]] <- "New Excel file not present in snapshot"
        fileStatus[[file]] <- "out-of-sync"
      }
    }

    # Common files to check for sheet and data differences
    commonFiles <- intersect(originalFiles, currentFiles)

    # 2. Check for sheet differences in each file
    for (file in commonFiles) {
      # Skip some keys that are not Excel files
      if (file %in% c("populationsCSV")) {
        next
      }

      # Initialize file status as in-sync, will be updated if differences found
      if (!(file %in% names(fileStatus))) {
        fileStatus[[file]] <- "in-sync"
      }

      # For files with sheets
      if (is.list(originalJsonObj[[file]]) && is.list(currentJsonObj[[file]])) {
        originalSheets <- names(originalJsonObj[[file]])
        currentSheets <- names(currentJsonObj[[file]])

        # Sheets missing in current Excel compared to snapshot
        missingSheets <- setdiff(originalSheets, currentSheets)
        if (length(missingSheets) > 0) {
          sheetChanges[[file]] <- list(
            missing = missingSheets
          )
          fileStatus[[file]] <- "out-of-sync"
        }

        # Sheets added in current Excel compared to snapshot
        addedSheets <- setdiff(currentSheets, originalSheets)
        if (length(addedSheets) > 0) {
          if (file %in% names(sheetChanges)) {
            sheetChanges[[file]][["added"]] <- addedSheets
          } else {
            sheetChanges[[file]] <- list(
              added = addedSheets
            )
          }
          fileStatus[[file]] <- "out-of-sync"
        }

        # 3. Check for data differences in common sheets
        commonSheets <- intersect(originalSheets, currentSheets)
        changedSheets <- c()

        for (sheet in commonSheets) {
          if (
            !identical(
              originalJsonObj[[file]][[sheet]],
              currentJsonObj[[file]][[sheet]]
            )
          ) {
            changedSheets <- c(changedSheets, sheet)
            fileStatus[[file]] <- "out-of-sync"
          }
        }

        if (length(changedSheets) > 0) {
          dataChanges[[file]] <- changedSheets
        }
      }
    }

    # Format all changes for report
    differences <- list(
      file_status = fileStatus,
      file_changes = if (length(fileChanges) > 0) fileChanges else NULL,
      sheet_changes = if (length(sheetChanges) > 0) sheetChanges else NULL,
      data_changes = if (length(dataChanges) > 0) dataChanges else NULL
    )

    # Return result
    result <- list(
      in_sync = FALSE,
      details = differences,
      unsaved_changes = hasUnsavedChanges
    )

    # Display message
    if (!silent) {
      cli::cli_alert_warning(
        "Excel configuration files are NOT in sync with JSON snapshot."
      )
      if (hasUnsavedChanges & !silent) {
        cli::cli_alert_info(
          "Additionally, the ProjectConfiguration object has {.strong unsaved changes} that differ from the Excel file."
        )
      }
    }

    if (!silent) {
      # Display the summary of file statuses
      cli::cli_h2("File Sync Status:")

      for (file in names(fileStatus)) {
        status_text <- fileStatus[[file]]
        if (status_text == "in-sync") {
          cli::cli_text(
            "{.green {cli::symbol$tick}} {file}.xlsx:  {status_text}"
          )
        } else {
          cli::cli_text(
            "{.red {cli::symbol$cross}} {file}.xlsx: {status_text}"
          )
        }
      }

      # Display detailed differences
      cli::cli_h2("Details:")

      for (file in names(fileStatus)) {
        if (fileStatus[[file]] == "out-of-sync") {
          cli::cli_li("{file}.xlsx")

          # Sheet changes
          if (
            !is.null(differences$sheet_changes) &&
              file %in% names(differences$sheet_changes)
          ) {
            sheet_info <- differences$sheet_changes[[file]]

            if (
              !is.null(sheet_info$missing) && length(sheet_info$missing) > 0
            ) {
              missing_sheets <- paste(sheet_info$missing, collapse = ", ")
              sublist <- cli::cli_ul()
              cli::cli_li("Missing sheets: {missing_sheets}")
              cli::cli_end(sublist)
            }

            if (!is.null(sheet_info$added) && length(sheet_info$added) > 0) {
              added_sheets <- paste(sheet_info$added, collapse = ", ")
              sublist <- cli::cli_ul()
              cli::cli_li("New sheets: {added_sheets}")
              cli::cli_end(sublist)
            }
          }

          # Data changes
          if (
            !is.null(differences$data_changes) &&
              file %in% names(differences$data_changes)
          ) {
            changed_sheets <- paste(
              differences$data_changes[[file]],
              collapse = ", "
            )
            sublist <- cli::cli_ul()
            cli::cli_li("Different data in sheets: {changed_sheets}")
            cli::cli_end(sublist)
          }
        }
      }

      # Add suggestions for resolving differences
      cli::cli_h2("Suggested Actions:")
      cli::cli_text("To resolve these differences, you can:")
      cli::cli_ul()

      # If there are unsaved changes, suggest saving first
      if (hasUnsavedChanges) {
        cli::cli_li(
          "{.run projectConfig$save()} - Save the unsaved changes in the ProjectConfiguration object to the Excel file."
        )
      }

      # Suggest snapshotProjectConfiguration to update the JSON snapshot
      cli::cli_li(
        "{.run snapshotProjectConfiguration()} - Save the changes from Excel files to the project snapshot."
      )

      # Suggest restoreProjectConfiguration to update the Excel files
      cli::cli_li(
        "{.run restoreProjectConfiguration()} - Recreate the Excel files according to the configuration snapshot."
      )

      cli::cli_end()
    }
  }

  invisible(result)
}

#' Convert Excel file to list structure for JSON serialization
#'
#' @param filePath Path to the Excel file
#' @param sheet Sheet name to read. If NULL, reads the first sheet.
#' @return List structure ready for JSON serialization
#' @keywords internal
#' @noRd
.excelToListStructure <- function(filePath, sheet = NULL) {
  # Read the data
  df <- readExcel(filePath, sheet)

  # Convert to simple list format
  sheetData <- list(
    column_names = names(df),
    rows = list()
  )

  # Store each row
  if (nrow(df) > 0) {
    for (i in 1:nrow(df)) {
      # Extract row as a character vector to avoid type issues during serialization
      rowValues <- sapply(df[i, ], as.character)
      sheetData$rows[[i]] <- as.list(rowValues)
    }
  }

  return(sheetData)
}

#' Convert CSV file to list structure for JSON serialization
#'
#' @param filePath Path to the CSV file
#' @return List structure ready for JSON serialization
#' @keywords internal
#' @noRd
.csvToListStructure <- function(filePath) {
  # Read the CSV data
  csvDf <- utils::read.csv(filePath, stringsAsFactors = FALSE)

  # Convert to simple list format
  csvData <- list(
    column_names = names(csvDf),
    rows = list()
  )

  # Store each row
  if (nrow(csvDf) > 0) {
    for (i in 1:nrow(csvDf)) {
      # Extract row as a character vector
      rowValues <- sapply(csvDf[i, ], as.character)
      csvData$rows[[i]] <- as.list(rowValues)
    }
  }

  return(csvData)
}

#' Convert data frame columns to appropriate types
#'
#' @param df Data frame with columns that need type conversion
#' @return Data frame with columns converted to appropriate types
#' @keywords internal
#' @noRd
.convertDataTypes <- function(df) {
  if (ncol(df) == 0 || nrow(df) == 0) {
    return(df)
  }

  for (colName in names(df)) {
    # Get non-NA values for type checking
    values <- df[[colName]]
    nonNaValues <- values[!is.na(values) & values != ""]

    # Skip if all values are NA or empty
    if (length(nonNaValues) == 0) {
      next
    }

    # Check if column can be converted to logical (TRUE/FALSE values)
    if (all(nonNaValues %in% c("TRUE", "FALSE"))) {
      df[[colName]] <- as.logical(values)
      next
    }

    # Check if column can be converted to numeric
    # Use suppressWarnings to handle NAs gracefully
    numericConversion <- suppressWarnings(as.numeric(values))
    # If no NAs were introduced by conversion (except those that were already NA)
    if (!any(is.na(numericConversion) & !is.na(values) & values != "")) {
      df[[colName]] <- numericConversion
    }
  }

  return(df)
}
