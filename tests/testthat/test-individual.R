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

  expect_silent(
    (project$configurations$individuals$Indiv1$individualObject)
  )
})


test_that("Individual object is recreated if individual configuration is changed", {
  project <- testProject()

  original_seed <- project$configurations$individuals$Indiv1$individualObject$characteristics$seed

  project$configurations$individuals$Indiv1$characteristics$age <- 35
  new_seed <- project$configurations$individuals$Indiv1$individualObject$characteristics$seed
  expect_true(original_seed != new_seed)

  project$configurations$individuals$Indiv1$parameters$GFR$value <- 80
  new_seed2 <- project$configurations$individuals$Indiv1$individualObject$characteristics$seed
  expect_true(new_seed != new_seed2)

})

test_that("It returns NULL if the specified individual Id cannot be found in
          the file and nullIfNotFound is TRUE", {
            individualId <- "notPresent"

          })

test_that("It throws an error if the specified individual Id cannot be found in
          the file and nullIfNotFound is FALSE", {
            individualId <- "notPresent"


          })

test_that("It create IndividualCharacteristics with the correct values", {

})

test_that("It create IndividualCharacteristics when numerical values are empty", {

})


