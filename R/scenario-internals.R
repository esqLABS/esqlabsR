# Internal scenario helpers ----

#' Parse protein ontogenies from a string
#'
#' @param ontogenyString A string in the format
#'   "Molecule1:Ontogeny1,Molecule2:Ontogeny2" or NULL.
#'
#' @returns A list of `MoleculeOntogeny` objects, or NULL.
#' @keywords internal
#' @noRd
.readOntongeniesFromList <- function(ontogenyString) {
  if (
    is.null(ontogenyString) || is.na(ontogenyString) || ontogenyString == ""
  ) {
    return(NULL)
  }
  # The string format is "Molecule1:Ontogeny1,Molecule2:Ontogeny2"
  ontogeniesSplit <- unlist(strsplit(ontogenyString, split = ",", fixed = TRUE))
  ontogeniesSplit <- trimws(ontogeniesSplit)
  moleculeOntogenies <- vector("list", length(ontogeniesSplit))
  for (i in seq_along(ontogeniesSplit)) {
    parts <- unlist(strsplit(ontogeniesSplit[[i]], split = ":", fixed = TRUE))
    if (length(parts) != 2) {
      stop(messages$errorWrongOntogenyStructure(ontogeniesSplit[[i]]))
    }
    protein <- parts[[1]]
    ontogeny <- parts[[2]]
    validateEnumValue(value = ontogeny, enum = ospsuite::StandardOntogeny)
    moleculeOntogenies[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = protein,
      ontogeny = ospsuite::StandardOntogeny[[ontogeny]]
    )
  }
  moleculeOntogenies
}

#' Read parameter values from a structured Excel file
#'
#' Each excel sheet must consist of columns 'Container Path', 'Parameter Name',
#' 'Value', and 'Units'.
#'
#' @param paramsXLSpath Path to the excel file
#' @param sheets Names of the excel sheets containing the information about the
#'   parameters. Multiple sheets can be processed. If no sheets are provided,
#'   the first one in the Excel file is used.
#'
#' @returns A list containing vectors `paths` with the full paths to the
#'   parameters, `values` the values of the parameters, and `units` with the
#'   units the values are in.
#' @keywords internal
#' @noRd
.readParametersFromXLS <- function(paramsXLSpath, sheets = NULL) {
  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")
  validateIsString(paramsXLSpath)
  validateIsString(sheets, nullAllowed = TRUE)

  if (is.null(sheets)) {
    sheets <- c(1)
  }

  pathsValuesVector <- vector(mode = "numeric")
  pathsUnitsVector <- vector(mode = "character")

  for (sheet in sheets) {
    data <- readExcel(path = paramsXLSpath, sheet = sheet)

    if (!all(columnNames %in% names(data))) {
      stop(messages$errorWrongXLSStructure(
        filePath = paramsXLSpath,
        expectedColNames = columnNames
      ))
    }

    fullPaths <- paste(
      data[["Container Path"]],
      data[["Parameter Name"]],
      sep = "|"
    )
    pathsValuesVector[fullPaths] <- as.numeric(data[["Value"]])

    pathsUnitsVector[fullPaths] <- tidyr::replace_na(
      data = as.character(data[["Units"]]),
      replace = ""
    )
  }

  return(.parametersVectorToList(pathsValuesVector, pathsUnitsVector))
}

#' Read species-specific parameters from the bundled SpeciesParameters.xlsx
#'
#' @param species Character scalar — the species name (e.g. "Rat", "Mouse").
#' @returns A parameter structure list (`paths`, `values`, `units`) or `NULL`
#'   if no sheet matches the species.
#' @keywords internal
#' @noRd
.getSpeciesParameters <- function(species) {
  filePath <- system.file(
    "extdata",
    "SpeciesParameters.xlsx",
    package = "esqlabsR"
  )
  if (!file.exists(filePath)) {
    return(NULL)
  }

  sheets <- readxl::excel_sheets(filePath)
  if (!species %in% sheets) {
    return(NULL)
  }

  .readParametersFromXLS(paramsXLSpath = filePath, sheets = species)
}
