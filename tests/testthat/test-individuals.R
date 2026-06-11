test_that(".parseIndividuals passes through unknown fields", {
  raw <- list(
    list(
      individualId = "I1",
      species = "Human",
      weight = 70,
      futureField = "kept"
    )
  )
  result <- esqlabsR:::.parseIndividuals(raw)

  expect_identical(result[["I1"]]$species, "Human")
  expect_identical(result[["I1"]]$weight, 70)
  expect_identical(result[["I1"]]$futureField, "kept")
})

test_that("an individual carrying an unknown field round-trips through serialization", {
  raw <- list(
    list(
      individualId = "I1",
      species = "Human",
      gender = "MALE",
      futureField = "kept"
    )
  )
  individuals <- esqlabsR:::.parseIndividuals(raw)
  project <- .fakeProject(individuals = individuals)

  out <- esqlabsR:::.individualsToJson(project)[[1L]]
  expect_identical(out$individualId, "I1")
  expect_identical(out$futureField, "kept")
  # individualId leads; the remaining fields pass through in record order.
  expect_identical(
    names(out),
    c("individualId", "species", "gender", "futureField")
  )
})

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
