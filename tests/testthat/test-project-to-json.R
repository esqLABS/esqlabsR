# Tests for the v2.0 Project.json serializer. The headline contract is
# round-trip equivalence: parse → serialize → parse again must produce a
# structurally identical project. Specific shape concerns (object vs array,
# scalar vs length-1 array, NULL preservation) get their own focused tests.

test_that(".projectToJson() returns a JSON-shaped list with the canonical top-level keys", {
  project <- exampleProject()
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
      "modelParameterSets",
      "individuals",
      "individualParameterSets",
      "populations",
      "applications",
      "applicationParameterSets",
      "plots",
      "parameterIdentification"
    ),
    ignore.order = TRUE
  )
  expect_identical(tree$schemaVersion, "2.0")
})

test_that(".projectToJson() rejects non-Project input", {
  expect_error(esqlabsR:::.projectToJson(list()), "must be a")
  expect_error(esqlabsR:::.projectToJson(NULL), "must be a")
})

test_that(".saveProjectJson() writes a valid JSON file", {
  project <- exampleProject()
  out <- withr::local_tempfile(fileext = ".json")

  result <- esqlabsR:::.saveProjectJson(project, out)

  expect_identical(result, out)
  expect_true(file.exists(out))
  expect_true(jsonlite::validate(readLines(
    out,
    encoding = "UTF-8",
    warn = FALSE
  )))
})

test_that(".saveProjectJson() rejects non-string paths", {
  project <- exampleProject()
  expect_error(
    esqlabsR:::.saveProjectJson(project, NULL),
    "must be a single non-empty, non-NA string"
  )
  expect_error(
    esqlabsR:::.saveProjectJson(project, c("a", "b")),
    "must be a single non-empty, non-NA string"
  )
  expect_error(
    esqlabsR:::.saveProjectJson(project, ""),
    "must be a single non-empty, non-NA string"
  )
})

test_that(".saveProjectJson() refuses to write to a missing directory", {
  project <- exampleProject()
  out <- file.path(tempfile(), "nested", "Project.json")

  expect_error(
    esqlabsR:::.saveProjectJson(project, out),
    "Parent directory does not exist"
  )
})

test_that("round-trip is structurally identical for the bundled example", {
  project <- exampleProject()
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  reloaded <- loadProject(out)

  # jsonPath / projectDirPath legitimately differ; everything else must match.
  expect_identical(reloaded$schemaVersion, project$schemaVersion)
  expect_identical(reloaded$esqlabsRVersion, project$esqlabsRVersion)
  expect_identical(reloaded$filePaths, project$filePaths)
  expect_identical(reloaded$outputPaths, project$outputPaths)
  expect_identical(
    reloaded$modelParameterSets,
    project$modelParameterSets
  )
  expect_identical(
    reloaded$individualParameterSets,
    project$individualParameterSets
  )
  expect_identical(
    reloaded$applicationParameterSets,
    project$applicationParameterSets
  )
  expect_identical(reloaded$individuals, project$individuals)
  expect_identical(reloaded$populations, project$populations)
  expect_identical(reloaded$applications, project$applications)
  expect_identical(reloaded$observedData, project$observedData)

  # Scenarios are R6 objects; plots has a non-trivial in-memory shape
  # (named list + data.frames with NA padding) where unset fields drop
  # to absent in JSON and re-read as missing columns. Compare via the
  # JSON projection so wire-level round-trip is stable.
  expect_identical(
    esqlabsR:::.projectToJson(reloaded)$scenarios,
    esqlabsR:::.projectToJson(project)$scenarios
  )
  expect_identical(
    esqlabsR:::.projectToJson(reloaded)$plots,
    esqlabsR:::.projectToJson(project)$plots
  )
})

test_that(".dataCombinedToNestedJson re-adds the name field and drops empty sublists", {
  parsed <- list(
    DC1 = list(simulated = list(list(label = "a")), observed = list()),
    DC2 = list(simulated = list(), observed = list(list(label = "b")))
  )
  json <- esqlabsR:::.dataCombinedToNestedJson(parsed)
  expect_length(json, 2)
  expect_equal(json[[1]]$name, "DC1")
  expect_equal(json[[1]]$simulated[[1]]$label, "a")
  expect_null(json[[1]]$observed) # empty observed dropped
  expect_equal(json[[2]]$name, "DC2")
  expect_null(json[[2]]$simulated)
  expect_equal(json[[2]]$observed[[1]]$label, "b")
})

test_that(".dataCombinedToNestedJson handles NULL and empty input", {
  expect_identical(esqlabsR:::.dataCombinedToNestedJson(list()), list())
  expect_identical(esqlabsR:::.dataCombinedToNestedJson(NULL), list())
})

test_that(".dataFrameToListOfLists drops NA cells per row", {
  df <- data.frame(
    plotId = c("P1", "P2"),
    plotType = c("individual", "population"),
    title = c("T1", NA),
    stringsAsFactors = FALSE
  )
  result <- esqlabsR:::.dataFrameToListOfLists(df)
  expect_length(result, 2)
  expect_equal(result[[1]]$title, "T1")
  expect_null(result[[2]]$title) # NA dropped
})

test_that(".plotsToJson returns NULL when project has no plots section", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "2.0", esqlabsRVersion = "6.0.0"),
    tmp,
    auto_unbox = TRUE
  )
  project <- loadProject(tmp)
  expect_null(esqlabsR:::.plotsToJson(project))
})

test_that("round-trip preserves length-1 arrays as arrays, not scalars", {
  project <- exampleProject()
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
  project <- exampleProject()
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  # The first scenario has populationId: null and steadyStateTime: null.
  # Without `null = "null"`, jsonlite would drop them; the field would be
  # absent on reload, breaking equality.
  expect_null(raw$scenarios[[1L]]$populationId)
  expect_null(raw$scenarios[[1L]]$steadyStateTime)
})

test_that("an empty Project saves a file that loadProject can reload", {
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(Project$new(), out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_identical(raw$schemaVersion, "2.0")

  # Reload must succeed (the schemaVersion guard would reject a null).
  reloaded <- loadProject(out)
  expect_identical(reloaded$schemaVersion, "2.0")
  expect_length(reloaded$scenarios, 0L)
})

test_that("outputPaths supplied as a named character vector serialize as a JSON object", {
  project <- .fakeProject(
    outputPaths = c(PVB = "Organism|PVB|Drug", Fat = "Organism|Fat|Drug")
  )
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_type(raw$outputPaths, "list")
  expect_named(raw$outputPaths, c("PVB", "Fat"))
  expect_identical(raw$outputPaths$PVB, "Organism|PVB|Drug")
})

test_that(".outputPathsToJson errors on a non-empty unnamed value", {
  project <- .fakeProject(outputPaths = c("Organism|PVB|Drug"))
  expect_snapshot(error = TRUE, esqlabsR:::.outputPathsToJson(project))
})

test_that("empty map sections serialize as JSON objects, not arrays", {
  empty <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "2.0", esqlabsRVersion = "6.0.0"),
    empty,
    auto_unbox = TRUE
  )
  project <- loadProject(empty)

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  text <- paste(
    readLines(out, encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )

  # Map-shaped sections must be `{}` even when empty; array-shaped sections
  # are `[]`. The schema is asymmetric on purpose.
  expect_match(text, '"filePaths":\\s*\\{\\s*\\}')
  expect_match(text, '"outputPaths":\\s*\\{\\s*\\}')
  expect_match(text, '"applications":\\s*\\{\\s*\\}')
  expect_match(text, '"modelParameterSets":\\s*\\{\\s*\\}')
  expect_match(text, '"individualParameterSets":\\s*\\{\\s*\\}')
  expect_match(text, '"applicationParameterSets":\\s*\\{\\s*\\}')
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
  project <- loadProject(empty)

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  reloaded <- loadProject(out)

  # On the second hop the JSON is `{}` (because the serializer emits map
  # sections that way), so jsonlite returns a *named* empty list. The
  # contract is that this representation is stable: a second save/reload
  # produces an identical structure.
  empty_named <- structure(list(), names = character(0L))
  expect_identical(reloaded$filePaths, empty_named)
  expect_identical(reloaded$outputPaths, empty_named)
  expect_identical(reloaded$applications, empty_named)
  expect_identical(reloaded$modelParameterSets, empty_named)
  expect_identical(reloaded$individualParameterSets, empty_named)
  expect_identical(reloaded$applicationParameterSets, empty_named)

  # And the round-trip is stable from there: re-saving and re-loading does
  # not drift further.
  out2 <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(reloaded, out2)
  reloaded2 <- loadProject(out2)
  expect_identical(reloaded2$filePaths, reloaded$filePaths)
  expect_identical(reloaded2$outputPaths, reloaded$outputPaths)
  expect_identical(reloaded2$applications, reloaded$applications)
  expect_identical(
    reloaded2$modelParameterSets,
    reloaded$modelParameterSets
  )
  expect_identical(
    reloaded2$individualParameterSets,
    reloaded$individualParameterSets
  )
  expect_identical(
    reloaded2$applicationParameterSets,
    reloaded$applicationParameterSets
  )
})

test_that("round-trip preserves a steady-state scenario including unit conversion", {
  project <- exampleProject()
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
  project <- exampleProject()
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

test_that("steadyState=false with a declared time and unit round-trips", {
  raw <- list(
    list(
      name = "S",
      individualId = "I",
      modelFile = "m.pkml",
      steadyState = FALSE,
      steadyStateTime = 2,
      steadyStateTimeUnit = "h"
    )
  )
  scenarios <- esqlabsR:::.parseScenarios(raw, list())
  sc <- scenarios[["S"]]

  # Parser stored the time in base units (minutes) but kept the flag off.
  expect_false(sc$simulateSteadyState)
  expect_equal(sc$steadyStateTime, 120)
  expect_identical(sc$steadyStateTimeUnit, "h")

  project <- .fakeProject(scenarios = scenarios)
  out <- esqlabsR:::.scenariosToJson(project)[[1L]]

  # The flag stays false, and the declared time/unit survive instead of
  # being dropped to null.
  expect_false(out$steadyState)
  expect_equal(out$steadyStateTime, 2)
  expect_identical(out$steadyStateTimeUnit, "h")
})

test_that("a standalone simulationTimeUnit round-trips when simulationTime is null", {
  raw <- list(
    list(
      name = "S",
      individualId = "I",
      modelFile = "m.pkml",
      simulationTimeUnit = "h"
    )
  )
  scenarios <- esqlabsR:::.parseScenarios(raw, list())

  expect_null(scenarios[["S"]]$simulationTime)
  expect_identical(scenarios[["S"]]$simulationTimeUnit, "h")

  project <- .fakeProject(scenarios = scenarios)
  out <- esqlabsR:::.scenariosToJson(project)[[1L]]
  expect_null(out$simulationTime)
  expect_identical(out$simulationTimeUnit, "h")
})

test_that("populationId is emitted even when simulationType has drifted", {
  sc <- Scenario(
    scenarioName = "S",
    modelFile = "m.pkml",
    individualId = "I"
  )
  # Drift: populationId set but type left at the Individual default.
  sc$populationId <- "Pop"
  expect_identical(sc$simulationType, "Individual")

  project <- .fakeProject(scenarios = list(S = sc))
  out <- esqlabsR:::.scenariosToJson(project)[[1L]]
  expect_identical(out$populationId, "Pop")
})

test_that(".validateScenarios warns on populationId / simulationType drift", {
  sc <- Scenario(scenarioName = "S", modelFile = "m.pkml")
  sc$populationId <- "Pop"

  result <- esqlabsR:::.validateScenarios(list(S = sc))
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_true(any(grepl("populationId but simulationType", msgs)))
})

test_that(".scenariosToJson errors when scenario outputPaths reference unknown ids", {
  project <- exampleProject()
  # Mutate the first scenario to add a named path whose id is not in
  # project$outputPaths. (Parser would have rejected this, but a Chapter
  # 7+ programmatic mutation could land us here.)
  project$scenarios[[1L]]$outputPaths <- c(
    project$scenarios[[1L]]$outputPaths,
    c(UnknownId = "Organism|NotDeclared|Path")
  )

  expect_error(
    esqlabsR:::.projectToJson(project),
    "unknown outputPathIds.*UnknownId"
  )
})

test_that(".scenariosToJson errors when outputPaths has unnamed elements", {
  project <- exampleProject()
  # Strip names to simulate a programmatic mutation that violates the invariant.
  project$scenarios[[1L]]$outputPaths <- unname(
    project$scenarios[[1L]]$outputPaths
  )

  expect_error(
    esqlabsR:::.projectToJson(project),
    "outputPaths.*without ids"
  )
})

test_that(".scenariosToJson errors when simulateSteadyState is TRUE without a unit", {
  project <- exampleProject()
  # Aciclovir_iv has simulateSteadyState=FALSE and no unit.
  # Flip the flag without setting the unit — the round-trip cannot
  # carry the steady-state time, so the serializer must reject it.
  project$scenarios[["Aciclovir_iv"]]$simulateSteadyState <- TRUE

  expect_error(
    esqlabsR:::.projectToJson(project),
    "Aciclovir_iv.*simulateSteadyState=TRUE.*steadyStateTimeUnit"
  )
})

test_that("round-trip preserves empty modelParameterSets as a JSON array", {
  project <- exampleProject()
  project$scenarios[["Aciclovir_iv"]]$modelParameterSets <- character(0)

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)

  # Empty modelParameterSets must serialise as `[]`, not `null`, so the
  # JSON shape stays an array.
  mp <- raw$scenarios[[1L]]$modelParameterSets
  expect_type(mp, "list")
  expect_length(mp, 0L)
})

test_that("round-trip preserves empty outputPathIds as a JSON array", {
  project <- exampleProject()
  project$scenarios[["Aciclovir_iv"]]$outputPaths <- NULL

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)

  # Absent / empty outputPaths must serialise as `[]`, not `null`.
  ids <- raw$scenarios[[1L]]$outputPathIds
  expect_type(ids, "list")
  expect_length(ids, 0L)
})

test_that(".scenariosToJson preserves both ids when two ids map to the same literal path", {
  jsonString <- '{
    "schemaVersion": "2.0",
    "esqlabsRVersion": "6.0.0",
    "filePaths": {},
    "observedData": [],
    "outputPaths": {
      "primary": "Organism|Brain|Drug",
      "alias":   "Organism|Brain|Drug"
    },
    "scenarios": [{
      "name": "S",
      "individualId": "I",
      "populationId": null,
      "readPopulationFromCSV": false,
      "modelParameterSets": [],
      "applicationProtocol": null,
      "simulationTime": null,
      "simulationTimeUnit": null,
      "steadyState": false,
      "steadyStateTime": null,
      "steadyStateTimeUnit": null,
      "overwriteFormulasInSS": false,
      "modelFile": "M.pkml",
      "outputPathIds": ["primary", "alias"]
    }],
    "modelParameterSets": {},
    "individuals": [],
    "populations": [],
    "applications": {},
    "plots": null
  }'
  jsonPath <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonString, jsonPath)

  project <- suppressWarnings(loadProject(jsonPath))
  rebuilt <- esqlabsR:::.scenariosToJson(project)

  expect_equal(rebuilt[[1]]$outputPathIds, list("primary", "alias"))
})
