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
      "name",
      "description",
      "definitionsFolder",
      "filePaths",
      "defaultSimulationRunOptions",
      "observedData",
      "outputPaths",
      "scenarios",
      "parameterSets",
      "initialConditions",
      "individuals",
      "populations",
      "applications",
      "dataCombined",
      "plots",
      "plotGrids",
      "parameterIdentification",
      # The Excel-bridge block is emitted only when the project carries
      # Excel-bridge fields (the bundled example does).
      "excel"
    ),
    ignore.order = TRUE
  )
  expect_identical(tree$schemaVersion, "2.0")
})

test_that(".projectToJson() splits the container path fields into filePaths and excel", {
  project <- exampleProject()
  tree <- esqlabsR:::.projectToJson(project)

  # The four live working folders stay in `filePaths`.
  expect_named(
    tree$filePaths,
    c("modelFolder", "populationsFolder", "dataFolder", "outputFolder"),
    ignore.order = TRUE
  )
  # The seven Excel-bridge sheet names move to the `excel` block.
  expect_named(
    tree$excel,
    c(
      "configurationsFolder",
      "modelParamsFile",
      "individualsFile",
      "populationsFile",
      "scenariosFile",
      "applicationsFile",
      "plotsFile"
    ),
    ignore.order = TRUE
  )
})

test_that(".projectToJson() omits the excel block for a from-scratch project", {
  project <- Project$new()
  tree <- esqlabsR:::.projectToJson(project)

  expect_false("excel" %in% names(tree))
  expect_null(tree$name)
  expect_null(tree$description)
  expect_null(tree$defaultSimulationRunOptions)
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
  expect_identical(reloaded$info$schemaVersion, project$info$schemaVersion)
  expect_identical(reloaded$info$esqlabsRVersion, project$info$esqlabsRVersion)
  expect_identical(reloaded$rawFilePaths(), project$rawFilePaths())
  expect_identical(
    reloaded$definitions$outputPaths,
    project$definitions$outputPaths
  )
  expect_identical(
    reloaded$definitions$parameterSets,
    project$definitions$parameterSets
  )
  expect_identical(
    reloaded$definitions$individuals,
    project$definitions$individuals
  )
  expect_identical(
    reloaded$definitions$populations,
    project$definitions$populations
  )
  expect_identical(
    reloaded$definitions$applications,
    project$definitions$applications
  )
  expect_identical(
    reloaded$definitions$observedData,
    project$definitions$observedData
  )

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

test_that(".dataCombinedToNestedJson re-adds the id field and drops empty sublists", {
  parsed <- list(
    DC1 = list(simulated = list(list(label = "a")), observed = list()),
    DC2 = list(simulated = list(), observed = list(list(label = "b")))
  )
  json <- esqlabsR:::.dataCombinedToNestedJson(parsed)
  expect_length(json, 2)
  expect_equal(json[[1]]$dataCombinedId, "DC1")
  expect_equal(json[[1]]$simulated[[1]]$label, "a")
  expect_null(json[[1]]$observed) # empty observed dropped
  expect_equal(json[[2]]$dataCombinedId, "DC2")
  expect_null(json[[2]]$simulated)
  expect_equal(json[[2]]$observed[[1]]$label, "b")
})

test_that(".dataCombinedToNestedJson handles NULL and empty input", {
  expect_identical(esqlabsR:::.dataCombinedToNestedJson(list()), list())
  expect_identical(esqlabsR:::.dataCombinedToNestedJson(NULL), list())
})

test_that(".plotEntriesToJson strips the entry class and drops the list name", {
  entries <- list(
    P1 = structure(
      list(plotId = "P1", plotType = "individual", title = "T1"),
      class = c("Plot", "list")
    ),
    P2 = structure(
      list(plotId = "P2", plotType = "population"),
      class = c("Plot", "list")
    )
  )
  result <- esqlabsR:::.plotEntriesToJson(entries)
  # A plain unnamed array of records, with the Plot class stripped so it never
  # leaks into JSON.
  expect_null(names(result))
  expect_length(result, 2)
  expect_identical(class(result[[1]]), "list")
  expect_equal(result[[1]]$title, "T1")
  expect_false("title" %in% names(result[[2]]))
})

test_that(".plotEntriesToJson returns an empty list for NULL or empty", {
  expect_identical(esqlabsR:::.plotEntriesToJson(NULL), list())
  expect_identical(esqlabsR:::.plotEntriesToJson(list()), list())
})

test_that("the plots-section serializers return NULL when empty", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "2.0", esqlabsRVersion = "6.0.0"),
    tmp,
    auto_unbox = TRUE
  )
  project <- loadProject(tmp)
  expect_null(esqlabsR:::.dataCombinedSectionToJson(project))
  expect_null(esqlabsR:::.plotsSectionToJson(project))
  expect_null(esqlabsR:::.plotGridsSectionToJson(project))
})

test_that("round-trip preserves length-1 arrays as arrays, not scalars", {
  project <- exampleProject()
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  # outputPaths for Aciclovir_iv has one entry; auto_unbox must
  # not collapse it to a scalar string.
  ids <- raw$scenarios[[1L]]$outputPaths
  expect_type(ids, "list")
  expect_length(ids, 1L)
  expect_identical(ids[[1L]], "aciclovir_pvb")
})

test_that("round-trip preserves NULL fields", {
  project <- exampleProject()
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  # The first scenario has population: null and steadyStateTime: null.
  # Without `null = "null"`, jsonlite would drop them; the field would be
  # absent on reload, breaking equality.
  expect_null(raw$scenarios[[1L]]$population)
  expect_null(raw$scenarios[[1L]]$steadyStateTime)
})

test_that("an empty Project saves a file that loadProject can reload", {
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(Project$new(), out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_identical(raw$schemaVersion, "2.0")

  # Reload must succeed (the schemaVersion guard would reject a null).
  reloaded <- loadProject(out)
  expect_identical(reloaded$info$schemaVersion, "2.0")
  expect_length(reloaded$definitions$scenarios, 0L)
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
  expect_match(text, '"parameterSets":\\s*\\{\\s*\\}')
  expect_match(text, '"initialConditions":\\s*\\{\\s*\\}')
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
  # Section accessors wrap the stored list in a printable DefinitionList; unwrap
  # to assert the underlying named-empty shape.
  empty_named <- structure(list(), names = character(0L))
  # A project with no `filePaths` block stores an unnamed empty list.
  expect_identical(reloaded$rawFilePaths(), list())
  expect_identical(
    .unwrapDefinitionList(reloaded$definitions$outputPaths),
    empty_named
  )
  expect_identical(
    .unwrapDefinitionList(reloaded$definitions$applications),
    empty_named
  )
  expect_identical(
    .unwrapDefinitionList(reloaded$definitions$parameterSets),
    empty_named
  )
  expect_identical(
    .unwrapDefinitionList(reloaded$definitions$initialConditions),
    empty_named
  )

  # And the round-trip is stable from there: re-saving and re-loading does
  # not drift further.
  out2 <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(reloaded, out2)
  reloaded2 <- loadProject(out2)
  expect_identical(reloaded2$rawFilePaths(), reloaded$rawFilePaths())
  expect_identical(
    reloaded2$definitions$outputPaths,
    reloaded$definitions$outputPaths
  )
  expect_identical(
    reloaded2$definitions$applications,
    reloaded$definitions$applications
  )
  expect_identical(
    reloaded2$definitions$parameterSets,
    reloaded$definitions$parameterSets
  )
})

test_that("a populated initialConditions section round-trips through a snapshot", {
  # An inline snapshot with one initial-condition set. Loading it, saving a
  # snapshot, and reloading yields an identical section (a fixed point).
  src <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      esqlabsRVersion = "6.0.0",
      initialConditions = list(
        testinitialset = list(
          list(path = "Organism|A|Concentration", value = 1.5, unit = "mg/l"),
          list(path = "Organism|B|Concentration", value = 0.5, unit = "µmol/l")
        )
      )
    ),
    src,
    auto_unbox = TRUE
  )
  project <- loadProject(src)
  expect_named(project$definitions$initialConditions, "testinitialset")
  expect_length(project$definitions$initialConditions$testinitialset, 2L)

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  reloaded <- loadProject(out)
  expect_identical(
    reloaded$definitions$initialConditions,
    project$definitions$initialConditions
  )
})

test_that("round-trip preserves a steady-state scenario including unit conversion", {
  project <- exampleProject()
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  ss <- Filter(
    function(s) s$name == "aciclovir_iv_steadystate",
    raw$scenarios
  )[[1L]]

  expect_true(ss$steadyState)
  # The numeric value must be in the *declared* unit (h), not the base
  # unit (min); 1 h survives the round-trip exactly.
  # jsonlite::fromJSON reads whole-number JSON numerics as integer.
  expect_identical(ss$steadyStateTime, 1L)
  expect_identical(ss$steadyStateTimeUnit, "h")
})

test_that("round-trip preserves outputPaths order", {
  project <- exampleProject()
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)

  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  ss <- Filter(
    function(s) s$name == "aciclovir_iv_steadystate",
    raw$scenarios
  )[[1L]]

  # JSON declared fat_cell, PVB (non-alphabetical). The order must be
  # preserved through parse -> serialize.
  expect_identical(
    ss$outputPaths,
    list("aciclovir_fat_cell", "aciclovir_pvb")
  )
})

test_that("steadyState=false with a declared time and unit round-trips", {
  raw <- list(
    list(
      name = "S",
      individual = "I",
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
      individual = "I",
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

test_that("population is emitted even when simulationType has drifted", {
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
  expect_identical(out$population, "Pop")
})

test_that(".validateScenarios warns on populationId / simulationType drift", {
  sc <- Scenario(scenarioName = "S", modelFile = "m.pkml")
  sc$populationId <- "Pop"

  result <- esqlabsR:::.validateScenarios(list(S = sc))
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_true(any(grepl("populationId but simulationType", msgs)))
})

test_that(".scenariosToJson keeps unknown outputPaths (referential, lazy)", {
  # An unknown outputPathId is a referential issue caught lazily by the
  # cross-reference validator, not a serialization error: the id round-trips
  # verbatim so a transiently-dangling reference is not lost on save.
  sc <- Scenario(scenarioName = "S", modelFile = "m.pkml")
  sc$outputPaths <- c(UnknownId = "Organism|NotDeclared|Path")
  project <- .fakeProject(scenarios = list(S = sc))

  out <- esqlabsR:::.scenariosToJson(project)[[1L]]
  expect_identical(out$outputPaths, list("UnknownId"))
})

test_that(".scenariosToJson errors when outputPaths has unnamed elements", {
  # Build the bad state on an in-memory project so the serializer guard is
  # exercised directly (a tree-backed project would fail-fast at write).
  sc <- Scenario(scenarioName = "S", modelFile = "m.pkml")
  sc$outputPaths <- unname(c(PVB = "Organism|PVB|Drug"))
  project <- .fakeProject(scenarios = list(S = sc))

  expect_error(
    esqlabsR:::.projectToJson(project),
    "outputPaths.*without ids"
  )
})

test_that(".scenariosToJson errors when simulateSteadyState is TRUE without a unit", {
  # The round-trip cannot carry the steady-state time without a unit, so the
  # serializer must reject it. Built on an in-memory project to exercise the
  # serializer guard directly.
  sc <- Scenario(scenarioName = "S", modelFile = "m.pkml")
  sc$simulateSteadyState <- TRUE
  project <- .fakeProject(scenarios = list(S = sc))

  expect_error(
    esqlabsR:::.projectToJson(project),
    "S.*simulateSteadyState=TRUE.*steadyStateTimeUnit"
  )
})

test_that("round-trip preserves empty modelParameterSets as a JSON array", {
  project <- exampleProject()
  scenarios <- .getSection(project, "scenarios")
  scenarios[["aciclovir_iv"]]$modelParameterSets <- character(0)
  .setSection(project, "scenarios", scenarios)

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)

  # Empty modelParameterSets must serialise as `[]`, not `null`, so the
  # JSON shape stays an array.
  mp <- raw$scenarios[[1L]]$parameterSets
  expect_type(mp, "list")
  expect_length(mp, 0L)
})

test_that("round-trip preserves empty outputPaths as a JSON array", {
  project <- exampleProject()
  scenarios <- .getSection(project, "scenarios")
  scenarios[["aciclovir_iv"]]$outputPaths <- NULL
  .setSection(project, "scenarios", scenarios)

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)

  # Absent / empty outputPaths must serialise as `[]`, not `null`.
  ids <- raw$scenarios[[1L]]$outputPaths
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
      "individual": "I",
      "population": null,
      "readPopulationFromCSV": false,
      "parameterSets": [],
      "application": null,
      "simulationTime": null,
      "simulationTimeUnit": null,
      "steadyState": false,
      "steadyStateTime": null,
      "steadyStateTimeUnit": null,
      "overwriteFormulasInSS": false,
      "modelFile": "M.pkml",
      "outputPaths": ["primary", "alias"]
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

  expect_equal(rebuilt[[1]]$outputPaths, list("primary", "alias"))
})
