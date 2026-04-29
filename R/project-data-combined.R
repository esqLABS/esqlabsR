# DataCombined JSON <-> flat data.frame ----

#' Parse nested dataCombined JSON to flat data.frame
#' @param nestedData List of dataCombined objects with simulated/observed arrays
#' @returns data.frame with DataCombinedName, dataType, and all entry fields
#' @keywords internal
#' @noRd
.parseNestedDataCombined <- function(nestedData) {
  if (is.null(nestedData) || length(nestedData) == 0) {
    return(data.frame())
  }

  transformCols <- c(
    "xOffsets",
    "xOffsetsUnits",
    "yOffsets",
    "yOffsetsUnits",
    "xScaleFactors",
    "yScaleFactors"
  )

  rows <- list()
  for (dataCombined in nestedData) {
    dataCombinedName <- dataCombined$name

    # Process simulated entries
    if (
      !is.null(dataCombined$simulated) && length(dataCombined$simulated) > 0
    ) {
      for (entry in dataCombined$simulated) {
        row <- list(
          DataCombinedName = dataCombinedName,
          dataType = "simulated",
          label = entry$label,
          scenario = entry$scenario,
          path = entry$path,
          dataSet = NA,
          group = entry$group %||% NA
        )
        for (col in transformCols) {
          row[[col]] <- entry[[col]] %||% NA
        }
        rows[[length(rows) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
      }
    }

    # Process observed entries
    if (!is.null(dataCombined$observed) && length(dataCombined$observed) > 0) {
      for (entry in dataCombined$observed) {
        row <- list(
          DataCombinedName = dataCombinedName,
          dataType = "observed",
          label = entry$label,
          scenario = NA,
          path = NA,
          dataSet = entry$dataSet,
          group = entry$group %||% NA
        )
        for (col in transformCols) {
          row[[col]] <- entry[[col]] %||% NA
        }
        rows[[length(rows) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame())
  }

  do.call(rbind, rows)
}

#' Convert flat dataCombined data.frame to nested JSON structure
#' @param df data.frame with DataCombinedName, dataType, and entry fields
#' @returns List of dataCombined objects with simulated/observed arrays
#' @keywords internal
#' @noRd
.dataCombinedToNestedJson <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  transformCols <- c(
    "xOffsets",
    "xOffsetsUnits",
    "yOffsets",
    "yOffsetsUnits",
    "xScaleFactors",
    "yScaleFactors"
  )

  dataCombinedNames <- unique(df$DataCombinedName)

  lapply(dataCombinedNames, function(dataCombinedName) {
    dataCombinedRows <- df[
      df$DataCombinedName == dataCombinedName,
      ,
      drop = FALSE
    ]

    simRows <- dataCombinedRows[
      dataCombinedRows$dataType == "simulated",
      ,
      drop = FALSE
    ]
    obsRows <- dataCombinedRows[
      dataCombinedRows$dataType == "observed",
      ,
      drop = FALSE
    ]

    simulated <- lapply(seq_len(nrow(simRows)), function(i) {
      row <- simRows[i, ]
      entry <- list(
        label = row$label,
        scenario = row$scenario,
        path = row$path,
        group = if (is.na(row$group)) NULL else row$group
      )
      for (col in transformCols) {
        entry[[col]] <- if (is.na(row[[col]])) NULL else row[[col]]
      }
      entry
    })

    observed <- lapply(seq_len(nrow(obsRows)), function(i) {
      row <- obsRows[i, ]
      entry <- list(
        label = row$label,
        dataSet = row$dataSet,
        group = if (is.na(row$group)) NULL else row$group
      )
      for (col in transformCols) {
        entry[[col]] <- if (is.na(row[[col]])) NULL else row[[col]]
      }
      entry
    })

    list(
      name = dataCombinedName,
      simulated = simulated,
      observed = observed
    )
  })
}
