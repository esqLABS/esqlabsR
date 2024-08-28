test_that("Individual configuration can be initialized", {
  project <- testProject()

  expect_snapshot(project$configurations$individuals$Indiv1)
})

test_that("Individual configuration can be modified", {
  project <- testProject()

  project$configurations$individuals$Indiv1$characteristics$age <- 35

  expect_equal(project$configurations$individuals$Indiv1$characteristics$age, 35)

  project$configurations$individuals$Indiv1$parameters$GFR$value <- 80

  expect_equal(project$configurations$individuals$Indiv1$parameters$GFR$value, 80)
})

test_that("Individual can be exported as dataFrame", {
  project <- testProject()

  expect_snapshot(project$configurations$individuals$Indiv1$toDataFrame())
})


test_that("Individual object can be created", {
  project <- testProject()

  expect_no_condition(project$configurations$individuals$Indiv1$individualObject)
})


test_that("Individual object is recreated if individual configuration is changed", {
  project <- testProject()

  original_seed <- project$configurations$individuals$Indiv1$individualObject$seed

  project$configurations$individuals$Indiv1$characteristics$age <- 35
  new_seed <- project$configurations$individuals$Indiv1$individualObject$seed
  expect_true(original_seed != new_seed)

  project$configurations$individuals$Indiv1$parameters$GFR$value <- 80
  new_seed2 <- project$configurations$individuals$Indiv1$individualObject$seed
  expect_true(new_seed != new_seed2)

})
