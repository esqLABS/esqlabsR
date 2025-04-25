#' Export project configuration Excel files to JSON
#'
#' @description
#' Exports all Excel configuration files in the Configurations folder of an esqlabsR project
#' to a single JSON file. This allows for easier version control and programmatic manipulation.
#'
#' @param project_config A ProjectConfiguration object or path to ProjectConfiguration excel file. Defaults to "ProjectConfiguration.xlsx".
#' @param output_dir where the JSON file will be saved. Defaults to the current working directory.
#'
#' @return Invisibly returns the exported configuration data structure
#' @export
snapshotProjectConfiguration <- function(
  project_config = "ProjectConfiguration.xlsx",
  output_dir = "."
) {
  # Convert to ProjectConfiguration object if path is provided
  if (is.character(project_config)) {
    project_config <- createProjectConfiguration(project_config)
  }

  # Validate project_config
  if (!inherits(project_config, "ProjectConfiguration")) {
    stop(
      "project_config must be a ProjectConfiguration object or valid path to ProjectConfiguration.xlsx"
    )
  }

  # Determine output filename based on source excel filename
  if (!is.null(project_config$projectConfigurationFilePath)) {
    source_filename <- basename(project_config$projectConfigurationFilePath)
    output_filename <- sub("\\.xlsx$", ".json", source_filename)
  } else {
    output_filename <- "ProjectConfiguration.json"
  }

  output_path <- file.path(output_dir, output_filename)

  # Validate output_path
  if (!endsWith(tolower(output_path), ".json")) {
    stop("output_path must end with .json extension")
  }

  # Ensure the directory exists, create it if it doesn't
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir) && output_dir != "") {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create the main config structure
  config_data <- list(
    files = list()
  )

  # Add ProjectConfiguration.xlsx data
  if (
    !is.null(project_config$projectConfigurationFilePath) &&
      file.exists(project_config$projectConfigurationFilePath)
  ) {
    proj_config_df <- readExcel(project_config$projectConfigurationFilePath)

    # Convert to simple list format
    proj_config_data <- list(
      column_names = names(proj_config_df),
      rows = list()
    )

    # Store each row
    if (nrow(proj_config_df) > 0) {
      for (i in 1:nrow(proj_config_df)) {
        # Extract row as a character vector
        row_values <- sapply(proj_config_df[i, ], as.character)
        proj_config_data$rows[[i]] <- as.list(row_values)
      }
    }

    # Store in config data
    config_data$files$projectConfiguration <- list(
      ProjectConfiguration = proj_config_data
    )
  }

  # Define files to export
  excel_files <- list(
    modelParameters = project_config$modelParamsFile,
    individuals = project_config$individualsFile,
    populations = project_config$populationsFile,
    scenarios = project_config$scenariosFile,
    applications = project_config$applicationsFile,
    plots = project_config$plotsFile
  )

  # Read each Excel file and store its sheets
  for (name in names(excel_files)) {
    file_path <- excel_files[[name]]
    if (!is.na(file_path) && file.exists(file_path)) {
      # Get sheets
      sheets <- readxl::excel_sheets(file_path)

      # Create a container for this file's sheets
      config_data$files[[name]] <- list()

      # Process each sheet
      for (sheet in sheets) {
        # Read the data
        df <- readExcel(file_path, sheet)

        # Convert to simple list format to avoid serialization issues
        sheet_data <- list(
          column_names = names(df),
          rows = list()
        )

        # Store each row
        if (nrow(df) > 0) {
          for (i in 1:nrow(df)) {
            # Extract row as a character vector to avoid type issues during serialization
            row_values <- sapply(df[i, ], as.character)
            sheet_data$rows[[i]] <- as.list(row_values)
          }
        }

        # Store in config data
        config_data$files[[name]][[sheet]] <- sheet_data
      }
    }
  }

  # Handle CSV files in populations folder if it exists
  if (
    !is.na(project_config$populationsFolder) &&
      dir.exists(project_config$populationsFolder)
  ) {
    # Find CSV files
    csv_files <- list.files(
      project_config$populationsFolder,
      pattern = "\\.csv$",
      full.names = TRUE
    )

    if (length(csv_files) > 0) {
      config_data$files$populationsCSV <- list()

      for (csv_file in csv_files) {
        file_name <- basename(csv_file)
        csv_df <- utils::read.csv(csv_file, stringsAsFactors = FALSE)

        # Convert to simple list format
        csv_data <- list(
          column_names = names(csv_df),
          rows = list()
        )

        # Store each row
        if (nrow(csv_df) > 0) {
          for (i in 1:nrow(csv_df)) {
            # Extract row as a character vector
            row_values <- sapply(csv_df[i, ], as.character)
            csv_data$rows[[i]] <- as.list(row_values)
          }
        }

        # Store in config data
        config_data$files$populationsCSV[[file_name]] <- csv_data
      }
    }
  }

  # Convert to JSON
  json_data <- jsonlite::toJSON(
    config_data,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA
  )

  # Ensure output directory exists
  dir_path <- dirname(output_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Write to file
  writeLines(json_data, output_path)

  # Display message with relative path
  input_file <- fs::path_rel(
    project_config$projectConfigurationFilePath,
    start = getwd()
  )

  # Get relative path from working directory
  output_file <- fs::path_rel(output_path, start = getwd())

  cli::cli_alert_success(
    "Snapshot of {.file {input_file}} created at {.file {output_file}}"
  )

  invisible(config_data)
}

#' Import project configuration from JSON to Excel files
#'
#' @description
#' Creates Excel configuration files from a JSON configuration file.
#' This allows for recreating the project configuration from version-controlled JSON.
#'
#' @param json_path Path to the JSON configuration file. Defaults to "ProjectConfiguration.json".
#' @param output_dir Directory where the Excel files will be created. Defaults to the current working directory.
#'
#' @return A ProjectConfiguration object initialized with the regenerated ProjectConfiguration.xlsx
#' @export
restoreProjectConfiguration <- function(
  json_path = "ProjectConfiguration.json",
  output_dir = "."
) {
  # Check if JSON file exists
  if (!file.exists(json_path)) {
    stop("JSON file does not exist: ", json_path)
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create Configurations directory
  config_dir <- file.path(output_dir, "Configurations")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Load JSON data
  config_data <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

  # Determine Excel filename based on source JSON filename
  json_filename <- basename(json_path)
  excel_filename <- sub("\\.json$", ".xlsx", json_filename)

  # Path to save ProjectConfiguration.xlsx
  proj_config_path <- file.path(output_dir, excel_filename)

  # Handle ProjectConfiguration.xlsx if present
  if ("projectConfiguration" %in% names(config_data$files)) {
    proj_config_data <- config_data$files$projectConfiguration$ProjectConfiguration

    # Get column names
    column_names <- proj_config_data$column_names

    # Create an empty data frame with the correct structure
    proj_config_df <- data.frame(matrix(
      ncol = length(column_names),
      nrow = length(proj_config_data$rows)
    ))
    colnames(proj_config_df) <- column_names

    # Fill in data if we have rows
    if (length(proj_config_data$rows) > 0) {
      for (i in seq_along(proj_config_data$rows)) {
        row_data <- proj_config_data$rows[[i]]

        # Fill in each column
        for (j in seq_along(column_names)) {
          if (j <= length(row_data)) {
            proj_config_df[i, j] <- row_data[[j]] %||% ""
          }
        }
      }

      # Convert data types to match the originals
      proj_config_df <- .convert_data_types(proj_config_df)
    }

    # Write ProjectConfiguration.xlsx file to the root directory, not inside Configurations
    .writeExcel(proj_config_df, proj_config_path)
  } else {
    # If no ProjectConfiguration in JSON, create a basic one
    warning("No ProjectConfiguration found in JSON file. Creating a basic one.")

    # Create a basic ProjectConfiguration dataframe
    proj_config_df <- data.frame(
      Property = c(
        "modelFolder",
        "configurationsFolder",
        "outputFolder"
      ),
      Value = c(
        file.path(output_dir, "Models"),
        file.path(output_dir, "Configurations"),
        file.path(output_dir, "Results")
      ),
      Description = c(
        "Path to folder containing model files",
        "Path to folder containing configuration files",
        "Path to folder for simulation results"
      )
    )

    # Write the basic ProjectConfiguration
    .writeExcel(proj_config_df, proj_config_path)
  }

  # Map file names to their Excel filenames
  file_mapping <- list(
    modelParameters = "ModelParameters.xlsx",
    individuals = "Individuals.xlsx",
    populations = "Populations.xlsx",
    scenarios = "Scenarios.xlsx",
    applications = "Applications.xlsx",
    plots = "Plots.xlsx"
  )

  # Process each Excel file
  for (name in names(file_mapping)) {
    if (name %in% names(config_data$files)) {
      # Get the sheets for this file
      sheets_data <- config_data$files[[name]]
      excel_sheets <- list()

      # Process each sheet
      for (sheet_name in names(sheets_data)) {
        sheet_info <- sheets_data[[sheet_name]]

        # Get column names
        column_names <- sheet_info$column_names

        # Create an empty data frame with the correct structure
        df <- data.frame(matrix(
          ncol = length(column_names),
          nrow = length(sheet_info$rows)
        ))
        colnames(df) <- column_names

        # Fill in data if we have rows
        if (length(sheet_info$rows) > 0) {
          for (i in seq_along(sheet_info$rows)) {
            row_data <- sheet_info$rows[[i]]

            # Fill in each column
            for (j in seq_along(column_names)) {
              if (j <= length(row_data)) {
                df[i, j] <- row_data[[j]] %||% ""
              }
            }
          }

          # Convert data types to match the originals
          df <- .convert_data_types(df)
        }

        # Add to the list of sheets
        excel_sheets[[sheet_name]] <- df
      }

      # Write the Excel file if we have data
      if (length(excel_sheets) > 0) {
        excel_path <- file.path(config_dir, file_mapping[[name]])
        .writeExcel(excel_sheets, excel_path)
      }
    }
  }

  # Handle populations CSV folder
  if ("populationsCSV" %in% names(config_data$files)) {
    csv_dir <- file.path(config_dir, "PopulationsCSV")
    if (!dir.exists(csv_dir)) {
      dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Get CSV files
    csv_files <- config_data$files$populationsCSV

    # Process each CSV file
    for (file_name in names(csv_files)) {
      csv_info <- csv_files[[file_name]]

      # Get column names
      column_names <- csv_info$column_names

      # Create empty data frame
      csv_df <- data.frame(matrix(
        ncol = length(column_names),
        nrow = length(csv_info$rows)
      ))
      colnames(csv_df) <- column_names

      # Fill in data if we have rows
      if (length(csv_info$rows) > 0) {
        for (i in seq_along(csv_info$rows)) {
          row_data <- csv_info$rows[[i]]

          # Fill in each column
          for (j in seq_along(column_names)) {
            if (j <= length(row_data)) {
              csv_df[i, j] <- row_data[[j]]
            }
          }
        }
      }

      # Write CSV file
      csv_path <- file.path(csv_dir, file_name)
      utils::write.csv(csv_df, csv_path, row.names = FALSE)
    }
  }

  # Get relative path from working directory for the output message
  rel_path <- fs::path_rel(proj_config_path, start = getwd())

  # Display message with relative path
  cli::cli_alert_success(
    "Project configuration from {.file {json_path}} restored at {.file {rel_path}}"
  )

  # Create and return a ProjectConfiguration object
  invisible(createProjectConfiguration(proj_config_path))
}


#' Convert data frame columns to appropriate types
#'
#' @param df Data frame with columns that need type conversion
#' @return Data frame with columns converted to appropriate types
#' @keywords internal
#' @noRd
.convert_data_types <- function(df) {
  if (ncol(df) == 0 || nrow(df) == 0) {
    return(df)
  }

  for (col_name in names(df)) {
    # Get non-NA values for type checking
    values <- df[[col_name]]
    non_na_values <- values[!is.na(values) & values != ""]

    # Skip if all values are NA or empty
    if (length(non_na_values) == 0) {
      next
    }

    # Check if column can be converted to logical (TRUE/FALSE values)
    if (all(non_na_values %in% c("TRUE", "FALSE"))) {
      df[[col_name]] <- as.logical(values)
      next
    }

    # Check if column can be converted to numeric
    # Use suppressWarnings to handle NAs gracefully
    numeric_conversion <- suppressWarnings(as.numeric(values))
    # If no NAs were introduced by conversion (except those that were already NA)
    if (!any(is.na(numeric_conversion) & !is.na(values) & values != "")) {
      df[[col_name]] <- numeric_conversion
    }
  }

  return(df)
}
