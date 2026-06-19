test_that("addIndividual + removeIndividual round-trip leaves the section unchanged", {
  project <- testProject()
  before <- project$individuals

  addIndividual(project, "NewI", species = "Human", gender = "MALE")
  expect_true("NewI" %in% names(project$individuals))

  removeIndividual(project, "NewI")
  expect_identical(project$individuals, before)
})

test_that("addIndividual aborts when individualId already exists", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addIndividual(project, "Indiv1", species = "Human", gender = "MALE")
  )
})

test_that("removeIndividual warns when referenced by a scenario", {
  project <- testProject()
  referenced <- "Indiv1"
  expect_warning(removeIndividual(project, referenced), "referenced")
  expect_false(referenced %in% names(project$individuals))
})
