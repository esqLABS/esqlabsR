test_that("addOutputPath() sets modified and clears validatedSinceMutation", {
  project <- testProject()
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  addOutputPath(project, "X", "Organism|A|Concentration in container")

  expect_true(project$modified)
  expect_false(project$validatedSinceMutation)
})

test_that("removeOutputPath() warns on missing key and is a no-op", {
  project <- testProject()
  expect_warning(removeOutputPath(project, "Ghost"), "not found")
  expect_false(project$modified)
})

test_that("addOutputPath aborts on a duplicate id", {
  project <- testProject()
  existing <- names(project$outputPaths)[[1]]
  expect_snapshot(
    error = TRUE,
    addOutputPath(
      project,
      existing,
      "Organism|other|Concentration in container"
    )
  )
})

test_that("removeOutputPath warns when the id is referenced by a scenario, removes anyway", {
  project <- testProject()
  referenced <- intersect(
    names(project$outputPaths),
    unlist(lapply(project$scenarios, \(sc) names(sc$outputPaths)))
  )[[1]]

  expect_warning(removeOutputPath(project, referenced), "referenced")
  expect_false(referenced %in% names(project$outputPaths))
})
