simulation <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
simTree <- getSimulationTree(simulation)

test_that("It throws an error if the quantity does not come from a molecule", {
  path <- simTree$Organism$Weight
  quantity <- getQuantity(path, simulation)

  expect_error(getMoleculeNameFromQuantity(quantity), messages$cannotGetMoleculeFromQuantity(path))
})

test_that("It returns the correct name of the molecule for molecules global parameter", {
  path <- simTree$Aciclovir$`Fraction unbound (plasma)`
  quantity <- getQuantity(path, simulation)

  expect_equal(getMoleculeNameFromQuantity(quantity), "Aciclovir")
})

test_that("It returns the correct name of the molecule for molecules local parameter", {
  path <- simTree$Organism$VenousBlood$Plasma$Aciclovir$Concentration
  quantity <- getQuantity(path, simulation)

  expect_equal(getMoleculeNameFromQuantity(quantity), "Aciclovir")
})

test_that("It returns the correct name of the molecule in a container", {
  path <- simTree$Organism$VenousBlood$Plasma$Aciclovir$path
  quantity <- getQuantity(path, simulation)

  expect_equal(getMoleculeNameFromQuantity(quantity), "Aciclovir")
})

test_that("It returns the correct name for an observer", {
  path <- simTree$Organism$PeripheralVenousBlood$Aciclovir$`Plasma (Peripheral Venous Blood)`
  quantity <- getQuantity(path, simulation)

  expect_equal(getMoleculeNameFromQuantity(quantity), "Aciclovir")
})
