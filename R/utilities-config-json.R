#' Export project configuration Excel files to JSON
#'
#' @description
#' Exports all Excel configuration files in the Configurations folder of an esqlabsR project
#' to a single JSON file. This allows for easier version control and programmatic manipulation.
#'
#' @param projectConfig A ProjectConfiguration object or path to ProjectConfiguration excel file. Defaults to "ProjectConfiguration.xlsx".
#' @param outputDir Directory where the JSON file will be saved. If NULL (default), the JSON file will be created in the same directory as the source Excel file.
#'
#' @return Invisibly returns the exported configuration data structure
#' @export
snapshotProjectConfiguration <- function(
  projectConfig = "ProjectConfiguration.xlsx",
  outputDir = NULL
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
    projConfigDf <- readExcel(projectConfig$projectConfigurationFilePath)

    # Convert to simple list format
    projConfigData <- list(
      column_names = names(projConfigDf),
      rows = list()
    )

    # Store each row
    if (nrow(projConfigDf) > 0) {
      for (i in 1:nrow(projConfigDf)) {
        # Extract row as a character vector
        rowValues <- sapply(projConfigDf[i, ], as.character)
        projConfigData$rows[[i]] <- as.list(rowValues)
      }
    }

    # Store in config data
    configData$projectConfiguration <- projConfigData
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
        # Read the data
        df <- readExcel(filePath, sheet)

        # Convert to simple list format to avoid serialization issues
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

        # Store in config data
        configData[[name]][[sheet]] <- sheetData
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
        csvDf <- utils::read.csv(csvFile, stringsAsFactors = FALSE)

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

        # Store in config data
        configData$populationsCSV[[fileName]] <- csvData
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

  if (interactive()) {
    cli::cli_alert_success(
      "Snapshot of {.file {inputFile}} created at {.file {outputFile}}"
    )
  }

  invisible(configData)
}

#' Import project configuration from JSON to Excel files
#'
#' @description
#' Creates Excel configuration files from a JSON configuration file.
#' This allows for recreating the project configuration from version-controlled JSON.
#'
#' @param jsonPath Path to the JSON configuration file. Defaults to "ProjectConfiguration.json".
#' @param outputDir Directory where the Excel files will be created. If NULL (default), the Excel files
#'   will be created in the same directory as the source JSON file.
#'
#' @return A ProjectConfiguration object initialized with the regenerated ProjectConfiguration.xlsx
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
