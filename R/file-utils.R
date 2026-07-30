#' Source all .R files located in a specific folder
#'
#' @param folderPath Path to the folder where .R files are located
#' @param recursive If `TRUE`, the contents of the sub-folders are also sourced,
#'   otherwise only the files located directly in the directory are considered.
#'   Default is `FALSE`.
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

#' Convert Windows filepaths for R
#'
#' Converts the Windows-like path (using `\`) from the clipboard to the form
#' readable by R (using` /`).
#'
#' @param path Path that will be converted. If `"clipboard"` (default), path is
#'   queried from clipboard.
#'
#' @returns String representation of a file path with `/` as separator.
#' @export
pathFromClipboard <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    clipr::read_clip()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  clipr::write_clip(x, allow_non_interactive = TRUE)
  return(x)
}

#' Read XLSX files using `readxl::read_excel` with suppressed warnings
#'
#' @details Rows that are blank in every column are dropped. Stray cell
#'   formatting extends a sheet's used range past its last real row, so a
#'   workbook edited over time routinely reports trailing rows that hold
#'   nothing; `readxl` returns them as all-`NA` records. They carry no
#'   information a project could use, and a parser that takes each row for a
#'   definition would abort on the first of them for having no id. Dropping them
#'   at the one place every sheet is read keeps every parser out of the
#'   business of recognizing them.
#'
#' @param path Full path of an XLS/XLSX file
#' @param sheet Name or number of the sheet. If `NULL` (default), the first
#'   sheet of the file is used.
#' @param ... Any other parameters that can be passed to `readxl::read_excel`
#'
#' @returns A tibble with the contents of the excel sheet
#' @export
readExcel <- function(path, sheet = NULL, ...) {
  .dropBlankRows(readxl::read_excel(
    path,
    sheet,
    .name_repair = "unique_quiet",
    ...
  ))
}

# TRUE for each element of one parsed sheet column that holds no value. A
# `col_types = "list"` column comes back as a list, whose elements are tested
# one at a time.
# @keywords internal
# @noRd
.blankColumnCells <- function(x) {
  if (is.list(x)) {
    return(vapply(x, .isBlankCell, logical(1)))
  }
  is.na(x) | trimws(as.character(x)) == ""
}

# Drop the rows of a parsed sheet that are blank in every column.
#
# The kept rows' original positions are recorded in a `sheetRow` attribute (1 for
# the first row below the header), because dropping rows here is what makes a
# parsed-frame index untrue as a workbook coordinate: a parser that reports a row
# to the user reads the attribute rather than guessing at a header offset. It is
# an attribute rather than a column so no parser that loops over `names(data)`
# sees it, and it therefore does not survive subsetting the frame: read it
# straight off the value `readExcel()` returned.
# @keywords internal
# @noRd
.dropBlankRows <- function(data) {
  if (nrow(data) == 0L || ncol(data) == 0L) {
    return(data)
  }
  blank <- Reduce(`&`, lapply(data, .blankColumnCells))
  kept <- data[!blank, , drop = FALSE]
  attr(kept, "sheetRow") <- which(!blank)
  kept
}

#' Resolve a project-controlled path and require it to stay under its root
#'
#' @details A project file (`Project.json`, a scenario, an observed-data entry)
#'   carries author-controlled path strings that are joined with a project
#'   folder and handed to a file loader. Without a check, a value such as
#'   `"../../../../etc/passwd"` resolves outside the project and the loader
#'   reads (or writes) a file the author never intended to expose. This helper
#'   resolves `path` relative to `root` and aborts unless the result stays
#'   inside `root`, so a traversal attempt is rejected with a clear error
#'   instead of the misleading "file not found" it would otherwise produce.
#'
#'   The containment check runs before any existence check, so callers can pass
#'   the returned path straight on to the loader and keep raising their own
#'   not-found error where a legitimate path simply does not exist yet.
#'
#'   Both the root and the candidate are made absolute with `fs::path_abs()`,
#'   which cleans `..` / `.` lexically without touching the filesystem. Using
#'   the same purely-lexical resolver on both sides is what makes the prefix
#'   comparison sound: mixing it with `normalizePath()` (which resolves
#'   symlinks, e.g. macOS `/var` -> `/private/var`) would leave the two sides
#'   on different prefixes and misread a legitimate path as an escape. It also
#'   means the root need not exist yet. Symlink resolution is intentionally not
#'   done here; the downstream loader resolves the returned path itself.
#'
#' @param path Author-controlled path, relative to `root`.
#' @param root Project folder the path must stay under. Need not exist yet;
#'   containment is checked lexically so a declared-but-not-created folder is
#'   fine.
#' @param fieldName Name of the project field `path` came from, used in the
#'   error message.
#' @returns The resolved absolute path (a string), on success.
#' @keywords internal
#' @noRd
# TRUE when `path`, resolved relative to `root`, lands outside `root`. The
# non-aborting containment predicate shared by `.resolveProjectPath()` (which
# aborts on TRUE) and the validators (which record a finding on TRUE). Purely
# lexical: both sides are made absolute with `fs::path_abs()` (no symlink
# resolution) so `..` climbing above the root is detected without touching the
# filesystem and the root need not exist. The contained-child prefix is the
# root plus a separator, except when the root already ends in one (a filesystem
# root such as `/` or `D:/`), where appending another would double the
# separator and wrongly reject every legitimate child.
# @keywords internal
# @noRd
# Resolve `path` to an absolute location for containment testing. An absolute
# `path` resolves to itself; a relative one is joined onto `root`. Lexical
# only (`fs::path_abs()` cleans `..`/`.` without touching the filesystem).
# @keywords internal
# @noRd
.absoluteAgainstRoot <- function(path, absRoot) {
  if (fs::is_absolute_path(path)) {
    as.character(fs::path_abs(path))
  } else {
    as.character(fs::path_abs(fs::path(absRoot, path)))
  }
}

.pathEscapesRoot <- function(path, root) {
  # No root to contain against (an unset / not-yet-known folder): nothing can
  # "escape" it, so report not-escaping and let the caller's own not-declared
  # handling fire where a root is actually required.
  if (is.null(root) || length(root) != 1L || is.na(root) || !nzchar(root)) {
    return(FALSE)
  }
  absRoot <- as.character(fs::path_abs(root))
  absPath <- .absoluteAgainstRoot(path, absRoot)
  sep <- .Platform$file.sep
  rootPrefix <- if (endsWith(absRoot, sep)) absRoot else paste0(absRoot, sep)
  absPath != absRoot && !startsWith(absPath, rootPrefix)
}

.resolveProjectPath <- function(path, root, fieldName = "path") {
  absRoot <- as.character(fs::path_abs(root))
  if (.pathEscapesRoot(path, root)) {
    cli::cli_abort(messages$projectPathEscapesRoot(fieldName, path, root))
  }
  .absoluteAgainstRoot(path, absRoot)
}

# The one pattern for a `${VAR}` / `$VAR` reference in a path, shared by the
# expander and by the predicate that grants the containment exemption, so a
# reference one of them recognizes is always one the other acts on.
# @keywords internal
# @noRd
.envVarPathPattern <- "\\$\\{?([A-Za-z_][A-Za-z0-9_]*)\\}?"

# The variable names `path` references, in order (empty when it references
# none).
# @keywords internal
# @noRd
.envVarNamesInPath <- function(path) {
  matches <- regmatches(
    path,
    gregexpr(.envVarPathPattern, path, perl = TRUE)
  )[[1]]
  sub(.envVarPathPattern, "\\1", matches)
}

# TRUE when `path` embeds a `${VAR}` / `$VAR` reference that `.replaceEnvVarPath()`
# will actually expand. A path that does is the sanctioned way to name a
# location outside the project (shared-drive data, a models folder several
# projects share), so every containment check exempts it and judges the raw,
# pre-expansion value. One predicate rather than the regex repeated at each
# check, so the exemption cannot come to mean different things in different
# places.
#
# `$PATH` is deliberately never expanded, so it does not earn the exemption:
# granting it would let `$PATH/../../etc` stay literal and then resolve outside
# the root with no containment check having run on the result.
# @keywords internal
# @noRd
.declaresEnvVarPath <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    return(FALSE)
  }
  any(.envVarNamesInPath(path) != "PATH")
}

# Expand every `${VAR}` / `$VAR` reference in `path` against the environment,
# leaving an unset variable's reference in place and never touching `$PATH`.
# The one place the package's env-var-in-path contract lives; the `Project`
# working-folder resolver (`.replaceEnvVar`) and the Excel-import folder
# resolution both call it so they cannot drift.
# @keywords internal
# @noRd
.replaceEnvVarPath <- function(path) {
  if (length(path) == 0L) {
    return(path)
  }
  m <- gregexpr(.envVarPathPattern, path, perl = TRUE)
  regmatches(path, m) <- lapply(regmatches(path, m), function(matches) {
    vapply(
      matches,
      function(match) {
        name <- sub(.envVarPathPattern, "\\1", match)
        if (identical(name, "PATH")) {
          return(match)
        }
        val <- Sys.getenv(name, unset = NA)
        if (is.na(val)) match else val
      },
      character(1)
    )
  })
  path
}

#' Write data to excel
#'
#' @details Uses `writexl::write_xlsx` to write data to excel. If the folder
#'   does not exist, creates folder(s) recursively. If the file exists, it is
#'   overwritten.
#'
#' @param data Data frame or named list of data frames that will be sheets in
#'   the xlsx
#' @param path Path to the xlsx file
#' @inheritParams writexl::write_xlsx
#' @keywords internal
#' @noRd
.writeExcel <- function(data, path, col_names = TRUE) {
  # If the provided path to the output file targets a non-existent directory,
  # try to create the directory
  parentDir <- dirname(path)
  if (!file.exists(parentDir)) {
    dir.create(parentDir, recursive = TRUE)
  }

  writexl::write_xlsx(data, path = path, col_names = col_names)
}

#' Guard a name that will be joined onto a directory
#'
#' A name that becomes a path via `file.path(dir, name)` must be a single plain
#' filename. One carrying a path separator, or `.` / `..`, would resolve outside
#' `dir` and write over whatever sits there.
#'
#' @param name The name to check.
#' @param message A `messages` entry taking `name` and returning the abort text,
#'   so each caller names its own argument.
#' @param call The frame the abort is attributed to; defaults to the caller, so
#'   the error reads as the public function the user actually called.
#' @returns `NULL`, invisibly; called for the abort.
#' @keywords internal
#' @noRd
.validateFilenameSegment <- function(name, message, call = rlang::caller_env()) {
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name) ||
      grepl("[/\\]", name) ||
      name %in% c(".", "..")
  ) {
    cli::cli_abort(message(name), call = call)
  }
  invisible(NULL)
}
