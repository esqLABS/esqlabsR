# Tests for the v2.0 Project.json serializer. The headline contract is
# round-trip equivalence: parse → serialize → parse again must produce a
# structurally identical project. Specific shape concerns (object vs array,
# scalar vs length-1 array, NULL preservation) get their own focused tests.

example_project_json_path <- function() {
  system.file(
    "extdata",
    "projects",
    "Example",
    "Project.json",
    package = "esqlabsR",
    mustWork = TRUE
  )
}

test_that(".projectToJson() returns a JSON-shaped list with the canonical top-level keys", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  tree <- esqlabsR:::.projectToJson(project)

  expect_type(tree, "list")
  expect_named(
    tree,
    c(
      "schemaVersion",
      "esqlabsRVersion",
      "filePaths",
      "observedData",
      "outputPaths",
      "scenarios",
      "modelParameters",
      "individuals",
      "populations",
      "applications",
      "plots"
    ),
    ignore.order = TRUE
  )
  expect_identical(tree$schemaVersion, "2.0")
})

test_that(".projectToJson() rejects non-Project input", {
  expect_error(esqlabsR:::.projectToJson(list()), "must be a Project")
  expect_error(esqlabsR:::.projectToJson(NULL), "must be a Project")
})

test_that(".saveProjectJson() writes a valid JSON file", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  out <- withr::local_tempfile(fileext = ".json")

  result <- esqlabsR:::.saveProjectJson(project, out)

  expect_identical(result, out)
  expect_true(file.exists(out))
  expect_true(jsonlite::validate(readLines(out, warn = FALSE)))
})

test_that(".saveProjectJson() rejects non-string paths", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  expect_error(
    esqlabsR:::.saveProjectJson(project, NULL),
    "must be a single non-NA string"
  )
  expect_error(
    esqlabsR:::.saveProjectJson(project, c("a", "b")),
    "must be a single non-NA string"
  )
})

test_that(".saveProjectJson() refuses to write to a missing directory", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  out <- file.path(tempfile(), "nested", "Project.json")

  expect_error(
    esqlabsR:::.saveProjectJson(project, out),
    "Parent directory does not exist"
  )
})

test_that("round-trip is structurally identical for the bundled example", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  reloaded <- esqlabsR:::.loadProjectJson(out)

  # jsonPath / projectDirPath legitimately differ; everything else must match.
  expect_identical(reloaded$schemaVersion, project$schemaVersion)
  expect_identical(reloaded$esqlabsRVersion, project$esqlabsRVersion)
  expect_identical(reloaded$filePaths, project$filePaths)
  expect_identical(reloaded$outputPaths, project$outputPaths)
  expect_identical(reloaded$modelParameters, project$modelParameters)
  expect_identical(reloaded$individuals, project$individuals)
  expect_identical(reloaded$populations, project$populations)
  expect_identical(reloaded$applications, project$applications)
  expect_identical(reloaded$observedData, project$observedData)
  expect_identical(reloaded$plots, project$plots)

  # Scenarios are R6 objects; compare via the JSON projection so that
  # parse(serialize(parse(x))) == parse(x) at the wire-shape level.
  expect_identical(
    esqlabsR:::.projectToJson(reloaded)$scenarios,
    esqlabsR:::.projectToJson(project)$scenarios
  )
})

test_that("round-trip preserves length-1 arrays as arrays, not scalars", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  # outputPathIds for Aciclovir_iv has one entry; auto_unbox must
  # not collapse it to a scalar string.
  ids <- raw$scenarios[[1L]]$outputPathIds
  expect_type(ids, "list")
  expect_length(ids, 1L)
  expect_identical(ids[[1L]], "Aciclovir_PVB")
})

test_that("round-trip preserves NULL fields", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  # The first scenario has populationId: null and steadyStateTime: null.
  # Without `null = "null"`, jsonlite would drop them; the field would be
  # absent on reload, breaking equality.
  expect_null(raw$scenarios[[1L]]$populationId)
  expect_null(raw$scenarios[[1L]]$steadyStateTime)
})

test_that("empty map sections serialize as JSON objects, not arrays", {
  empty <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "2.0", esqlabsRVersion = "6.0.0"),
    empty,
    auto_unbox = TRUE
  )
  project <- esqlabsR:::.loadProjectJson(empty)

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  text <- paste(readLines(out, warn = FALSE), collapse = "\n")

  # Map-shaped sections must be `{}` even when empty; array-shaped sections
  # are `[]`. The schema is asymmetric on purpose.
  expect_match(text, '"filePaths":\\s*\\{\\s*\\}')
  expect_match(text, '"outputPaths":\\s*\\{\\s*\\}')
  expect_match(text, '"applications":\\s*\\{\\s*\\}')
  expect_match(text, '"modelParameters":\\s*\\{\\s*\\}')
  expect_match(text, '"scenarios":\\s*\\[\\s*\\]')
  expect_match(text, '"individuals":\\s*\\[\\s*\\]')
  expect_match(text, '"populations":\\s*\\[\\s*\\]')
  expect_match(text, '"observedData":\\s*\\[\\s*\\]')
})

test_that("empty map sections survive a round-trip as empty named lists", {
  empty <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "2.0", esqlabsRVersion = "6.0.0"),
    empty,
    auto_unbox = TRUE
  )
  project <- esqlabsR:::.loadProjectJson(empty)

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  reloaded <- esqlabsR:::.loadProjectJson(out)

  # On the second hop the JSON is `{}` (because the serializer emits map
  # sections that way), so jsonlite returns a *named* empty list. The
  # contract is that this representation is stable: a second save/reload
  # produces an identical structure.
  empty_named <- structure(list(), names = character(0L))
  expect_identical(reloaded$filePaths, empty_named)
  expect_identical(reloaded$outputPaths, empty_named)
  expect_identical(reloaded$applications, empty_named)
  expect_identical(reloaded$modelParameters, empty_named)

  # And the round-trip is stable from there: re-saving and re-loading does
  # not drift further.
  out2 <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(reloaded, out2)
  reloaded2 <- esqlabsR:::.loadProjectJson(out2)
  expect_identical(reloaded2$filePaths, reloaded$filePaths)
  expect_identical(reloaded2$outputPaths, reloaded$outputPaths)
  expect_identical(reloaded2$applications, reloaded$applications)
  expect_identical(reloaded2$modelParameters, reloaded$modelParameters)
})

test_that("round-trip preserves a steady-state scenario including unit conversion", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  ss <- Filter(
    function(s) s$name == "Aciclovir_iv_steadystate",
    raw$scenarios
  )[[1L]]

  expect_true(ss$steadyState)
  # The numeric value must be in the *declared* unit (h), not the base
  # unit (min); 1 h survives the round-trip exactly.
  # jsonlite::fromJSON reads whole-number JSON numerics as integer.
  expect_identical(ss$steadyStateTime, 1L)
  expect_identical(ss$steadyStateTimeUnit, "h")
})

test_that("round-trip preserves outputPathIds order", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  ss <- Filter(
    function(s) s$name == "Aciclovir_iv_steadystate",
    raw$scenarios
  )[[1L]]

  # JSON declared fat_cell, PVB (non-alphabetical). The order must be
  # preserved through parse -> serialize.
  expect_identical(
    ss$outputPathIds,
    list("Aciclovir_fat_cell", "Aciclovir_PVB")
  )
})
