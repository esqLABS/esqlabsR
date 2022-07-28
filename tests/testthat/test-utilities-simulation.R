# getAllApplicationParameters

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)

test_that("It returns application parameters when no molecules are defined", {
  applicationParams <- getAllApplicationParameters(simulation = simulation)

  expect_length(applicationParams, 5)
})

test_that("It returns application parameters when a molecule are defined", {
  molecule <- "Aciclovir"
  applicationParams <- getAllApplicationParameters(
    simulation = simulation,
    moleculeNames = molecule
  )

  expect_length(applicationParams, 5)
})

test_that("It returns an empty list when a molecule is defined that is not in the model", {
  molecule <- "Foo"
  applicationParams <- getAllApplicationParameters(
    simulation = simulation,
    moleculeNames = molecule
  )

  expect_equal(applicationParams, list())
})
