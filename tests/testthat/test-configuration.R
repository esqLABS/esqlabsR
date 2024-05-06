test_that("Configuration object can be initialized", {
  project <- testProject()

  expect_no_condition(Configuration$new(project = project))
})


test_that("Individual Configuration can be initialized", {
  project <- testProject()

  individualConfigurations <- createIndividualsConfigurations(project)
  individualConfiguration <- IndividualConfiguration$new(configuration)

  expect_no_condition(individualConfiguration)
})
