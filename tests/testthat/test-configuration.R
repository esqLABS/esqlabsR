test_that("Configuration object can be initialized", {
  project <- testProject()

  expect_no_condition(Configuration$new(project = project))
})

