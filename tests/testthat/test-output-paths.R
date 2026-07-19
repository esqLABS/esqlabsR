test_that("addOutputPath() clears the validation flag", {
  project <- testProject()
  .markValidated(project)
  expect_true(.isValidated(project))

  addOutputPath(project, "X", "Organism|A|Concentration in container")

  expect_false(.isValidated(project))
})

test_that("removeOutputPath() warns on missing key and is a no-op", {
  project <- testProject()
  .markValidated(project)
  expect_warning(removeOutputPath(project, "Ghost"), "not found")
  # A no-op must not invalidate the validation cache.
  expect_true(.isValidated(project))
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

test_that("addOutputPath aborts on a duplicate id in the batch", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addOutputPath(
      project,
      c("a", "a"),
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

test_that("removeOutputPath does not over-report when an unreferenced id shares a path", {
  project <- testProject()
  # An id that IS referenced by a scenario, plus a second id resolving to the
  # SAME literal path but referenced by no scenario.
  referenced <- intersect(
    names(project$outputPaths),
    unlist(lapply(project$scenarios, \(sc) names(sc$outputPaths)))
  )[[1]]
  addOutputPath(project, "shared_copy", project$outputPaths[[referenced]])

  # The reference check keys on the output-path id, not the resolved path, so
  # removing `shared_copy` (which no scenario references by id) must not warn,
  # even though its path value also appears under `referenced`.
  expect_no_warning(removeOutputPath(project, "shared_copy"))
  expect_false("shared_copy" %in% names(project$outputPaths))
})

# setOutputPath ----

test_that("setOutputPath changes the literal path in memory and persists on save", {
  project <- testProject()
  id <- names(project$outputPaths)[[1]]
  setOutputPath(project, id, "Organism|Lung|Concentration in container")

  expect_equal(
    project$outputPaths[[id]],
    "Organism|Lung|Concentration in container"
  )

  # The edit reaches disk on save: a throwaway reload sees the new path.
  saveProject(project)
  reloaded <- loadProject(project$jsonPath)
  expect_equal(
    reloaded$outputPaths[[id]],
    "Organism|Lung|Concentration in container"
  )
})

test_that("setOutputPath clears the validation flag", {
  project <- testProject()
  .markValidated(project)
  id <- names(project$outputPaths)[[1]]
  setOutputPath(project, id, "Organism|Lung|Concentration in container")
  expect_false(.isValidated(project))
})

test_that("setOutputPath leaves the other ids untouched", {
  project <- testProject()
  ids <- names(project$outputPaths)
  other <- ids[[2]]
  beforeOther <- project$outputPaths[[other]]

  setOutputPath(project, ids[[1]], "Organism|Lung|Concentration in container")
  expect_equal(project$outputPaths[[other]], beforeOther)
})

test_that("setOutputPath aborts on a non-existent id", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    setOutputPath(project, "Ghost", "Organism|A|Concentration in container")
  )
})

test_that("setOutputPath rejects an empty path", {
  project <- testProject()
  id <- names(project$outputPaths)[[1]]
  before <- project$outputPaths[[id]]
  expect_snapshot(
    error = TRUE,
    setOutputPath(project, id, "")
  )
  expect_equal(project$outputPaths[[id]], before)
})

test_that("setOutputPath stays in memory until saveProject()", {
  source <- testProject()
  id <- names(source$outputPaths)[[1]]
  before <- source$outputPaths[[id]]
  setOutputPath(source, id, "Organism|Lung|Concentration in container")

  expect_equal(
    source$outputPaths[[id]],
    "Organism|Lung|Concentration in container"
  )
  # The edit must not reach the on-disk tree before a save.
  reloaded <- loadProject(source$jsonPath)
  expect_equal(reloaded$outputPaths[[id]], before)
})

# Vectorized authoring ----

test_that("addOutputPath recycles a single path to every id", {
  project <- testProject()
  addOutputPath(project, c("a", "b"), "Organism|Liver|X")
  expect_identical(project$outputPaths$a, "Organism|Liver|X")
  expect_identical(project$outputPaths$b, "Organism|Liver|X")
})

test_that("addOutputPath aligns a length-N path vector by position", {
  project <- testProject()
  addOutputPath(project, c("a", "b"), c("Organism|A", "Organism|B"))
  expect_identical(project$outputPaths$a, "Organism|A")
  expect_identical(project$outputPaths$b, "Organism|B")
  saveProject(project)
  reloaded <- loadProject(project$jsonPath)
  expect_identical(reloaded$outputPaths$a, "Organism|A")
  expect_identical(reloaded$outputPaths$b, "Organism|B")
})

test_that("addOutputPath aborts on a path length that is neither 1 nor N", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addOutputPath(project, c("a", "b", "c"), c("X", "Y"))
  )
})

test_that("setOutputPath vectorizes across N ids", {
  project <- testProject()
  addOutputPath(project, c("a", "b"), c("Organism|A", "Organism|B"))
  setOutputPath(project, c("a", "b"), c("Organism|A2", "Organism|B2"))
  expect_identical(project$outputPaths$a, "Organism|A2")
  expect_identical(project$outputPaths$b, "Organism|B2")
})

test_that("removeOutputPath removes a vector of ids in one write-through", {
  project <- testProject()
  addOutputPath(project, c("a", "b"), "Organism|X")
  removeOutputPath(project, c("a", "b"))
  expect_false(any(c("a", "b") %in% names(project$outputPaths)))
  reloaded <- loadProject(project$jsonPath)
  expect_false(any(c("a", "b") %in% names(reloaded$outputPaths)))
})

test_that("addOutputPath aborts the whole batch and writes nothing on one bad id", {
  project <- testProject()
  before <- names(project$outputPaths)
  existing <- before[[1]]
  expect_error(
    # "a" is new but the second id already exists, so the whole batch aborts.
    addOutputPath(project, c("a", existing), "Organism|X")
  )
  # Neither memory nor disk gained the new id.
  expect_identical(names(project$outputPaths), before)
  reloaded <- loadProject(project$jsonPath)
  expect_identical(names(reloaded$outputPaths), before)
})

test_that("addOutputPath rejects an empty or NA path", {
  project <- testProject()
  before <- names(project$outputPaths)
  expect_error(addOutputPath(project, "empty", ""), "non-empty")
  expect_error(addOutputPath(project, "missing", NA_character_), "non-empty")
  # A bad path in a batch aborts the whole call and writes nothing.
  expect_error(addOutputPath(project, c("a", "b"), c("Organism|X", "")))
  expect_identical(names(project$outputPaths), before)
})
