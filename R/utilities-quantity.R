#' Get the name of the molecule from a quantity
#'
#' @description Returns the name of the molecule to which the quantity object
#' is associated. The quantity could be the amount of the molecule in a container
#' ('Organism|VenousBlood|Plasma|Aciclovir'), a parameter of the molecule
#' ('Aciclovir|Lipophilicity' or 'Organism|VenousBlood|Plasma|Aciclovir|Concentration'),
#' or an observer ("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)").
#'
#' If the quantity is not associated with a molecule (e.g. 'Organism|Weight'),
#' an error is thrown.
#'
#' @param quantity A `Quantity` object
#'
#' @return Name of the molecule the quantity is associated with.
#' @export
#'
#' @examples
#' simulation <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
#' quantity <- getQuantity(
#'   path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
#'   container = simulation
#' )
#' getMoleculeNameFromQuantity(quantity = quantity)
getMoleculeNameFromQuantity <- function(quantity) {
  validateIsOfType(quantity, "Quantity")

  quantityType <- quantity$quantityType

  # If the passed quantitiy is a molecule, return its name
  if (any(c("Drug", "Molecule") == quantityType)) {
    return(quantity$name)
  }

  # Otherwise try to get its parent container
  parentContainer <- quantity$parentContainer
  parentContainerType <- parentContainer$containerType

  # If parent container is not a molecule, stop with an error
  if (!(any(c("Drug", "Molecule") == parentContainerType))) {
    stop(messages$cannotGetMoleculeFromQuantity(quantity$path))
  }

  return(parentContainer$name)
}
