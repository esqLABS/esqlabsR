#' Source all .R files located in a specific folder
#'
#' @param folderPath Path to the folder where .R files are located
#' @param recursive If TRUE, the contents of the sub-folders are also sourced,
#' otherwise only the files located directly in the directory are considered.
#' Default is FALSE.
#' @export
sourceAll <- function(folderPath, recursive = FALSE) {
  filesPaths <- list.files(folderPath, recursive = recursive)

  sourceFile <- function(filePath) {
    if (toupper(tools::file_ext(filePath)) == "R") {
      source(filePath, encoding = "UTF-8")
    }
    invisible()
  }

  invisible(lapply(file.path(folderPath, filesPaths), sourceFile))
}

#' pathFromClipboard
#'
#' Converts the windows-like path (using `\`) from the clipboard to the form
#' readable by R (using` /`).
#'
#' @param path Path that will be converted. If "clipboard" (default), path is
#'   queried from clipboard.
#'
#' @return String representation of a file path with `/` as separator
#' @export
pathFromClipboard <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}

#' Read XLSX files using `readxl::read_excel` with suppressed warnings
#'
#' @param path Full path of an XLS/XLSX file
#' @param sheet Name or number of the sheet. If `NULL` (default), the first sheet of the
#'   file is used.
#' @param ... Any other parameters that can be passed to readxl::read_excel
#'
#' @return A tibble with the contents of the excel sheet
#' @export
readExcel <- function(path, sheet = NULL, ...) {
  return(readxl::read_excel(
    path,
    sheet,
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE),
    ...
  ))
}
