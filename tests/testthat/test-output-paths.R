# Round-trip ----

test_that("saveProject produces round-trip fidelity for outputPaths", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(project$outputPaths, pc2$outputPaths)
})

test_that("addOutputPath survives round-trip", {
  project <- testProject()
  addOutputPath(project, "RTPath", "Organism|Liver|C")
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  expect_equal(reloaded$outputPaths[["RTPath"]], "Organism|Liver|C")
})

# Public CRUD: outputPaths ----

test_that("addOutputPath adds single entry", {
  project <- testProject()
  initial <- length(project$outputPaths)
  addOutputPath(
    project,
    id = "MyOut",
    path = "Organism|PeripheralVenousBlood|C"
  )
  expect_equal(length(project$outputPaths), initial + 1)
  expect_equal(
    project$outputPaths[["MyOut"]],
    "Organism|PeripheralVenousBlood|C"
  )
  expect_true(project$modified)
})

test_that("addOutputPath accepts vectors of equal length", {
  project <- testProject()
  addOutputPath(
    project,
    id = c("O1", "O2"),
    path = c("Organism|Liver|C", "Organism|Kidney|C")
  )
  expect_equal(project$outputPaths[["O1"]], "Organism|Liver|C")
  expect_equal(project$outputPaths[["O2"]], "Organism|Kidney|C")
})

test_that("addOutputPath errors on length mismatch", {
  project <- testProject()
  expect_error(
    addOutputPath(project, c("A", "B"), c("Organism|Liver|C")),
    regexp = "same length"
  )
})

test_that("addOutputPath errors on duplicate id within call", {
  project <- testProject()
  expect_error(
    addOutputPath(project, c("Dup", "Dup"), c("a", "b")),
    regexp = "duplicate"
  )
})

test_that("addOutputPath errors on existing id", {
  project <- testProject()
  existing <- names(project$outputPaths)[[1]]
  expect_error(
    addOutputPath(project, existing, "Organism|Liver|C"),
    regexp = "already exists"
  )
})

test_that("removeOutputPath removes entry", {
  project <- testProject()
  addOutputPath(project, "GoingAway", "Organism|Liver|C")
  project$.markSaved()
  removeOutputPath(project, "GoingAway")
  expect_false("GoingAway" %in% names(project$outputPaths))
  expect_true(project$modified)
})

test_that("removeOutputPath warns on missing id", {
  project <- testProject()
  expect_warning(
    removeOutputPath(project, "NoSuchPath_ZZ"),
    regexp = "not found"
  )
})

test_that("project$addOutputPath / removeOutputPath / removeScenario delegate", {
  pc1 <- testProject()
  pc2 <- testProject()
  addOutputPath(pc1, "M1", "Organism|Liver|C")
  pc2$addOutputPath("M1", "Organism|Liver|C")
  expect_equal(pc1$outputPaths[["M1"]], pc2$outputPaths[["M1"]])

  removeOutputPath(pc1, "M1")
  pc2$removeOutputPath("M1")
  expect_false("M1" %in% names(pc1$outputPaths))
  expect_false("M1" %in% names(pc2$outputPaths))
})

test_that("removeOutputPath warns when referenced by a scenario", {
  project <- testProject()
  # Find any outputPath referenced by at least one scenario (fixture-dependent)
  refId <- NULL
  for (id in names(project$outputPaths)) {
    hit <- vapply(
      project$scenarios,
      function(s) project$outputPaths[[id]] %in% s$outputPaths,
      logical(1)
    )
    if (any(hit)) {
      refId <- id
      break
    }
  }
  skip_if(is.null(refId), "no referenced outputPath in test fixture")

  expect_warning(
    removeOutputPath(project, refId),
    regexp = "referenced"
  )
  expect_false(refId %in% names(project$outputPaths))
})
