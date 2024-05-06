#' Read ontogeny mapping from excel
#'
#' @param data Data from from excel file containing columns 'Protein' and
#' 'Ontogeny'
#'
#' @keywords internal
#' @return A list of `MoleculeOntogeny` objects
createOntogenies <- function(data) {
  # Read columns 'Ontogeny' and 'Protein'
  ontogenies <- data$Ontogeny
  # calling 'as.character' as sometimes empty cells in Excel are not recognized as
  # chr NA, but some other NA, and strsplit fails.
  ontogenies <- as.character(ontogenies)
  # Proteins/ontogenies are separated by a ','
  ontogenies <- unlist(strsplit(x = ontogenies, split = ",", fixed = TRUE))
  # Remove whitespaces
  ontogenies <- trimws(ontogenies)
  proteins <- data$Protein
  proteins <- as.character(proteins)
  proteins <- unlist(strsplit(x = proteins, split = ",", fixed = TRUE))
  proteins <- trimws(proteins)

  # For each protein, an ontogeny must be specified
  validateIsSameLength(proteins, ontogenies)
  # Return 'NULL' if no ontogenies are specified. Not returning earlier to catch
  # a case where e.g. protein names are specified but not the ontogenies (lenghts
  # are not equal)
  if (anyNA(proteins)) {
    return(NULL)
  }
  moleculeOntogenies <- vector("list", length(proteins))
  for (i in seq_along(proteins)) {
    ontogeny <- ontogenies[[i]]
    validateEnumValue(value = ontogeny, enum = ospsuite::StandardOntogeny)
    moleculeOntogenies[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = proteins[[i]],
      ontogeny = ospsuite::StandardOntogeny[[ontogeny]]
    )
  }

  return(moleculeOntogenies)
}
