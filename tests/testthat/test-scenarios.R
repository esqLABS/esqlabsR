# Tests for the Scenario class and the .parseScenarios helper.

test_that("Scenario has the documented field defaults", {
  sc <- Scenario()

  expect_s3_class(sc, "Scenario")
  expect_type(sc, "list")

  # Fields default to NULL except where the spec calls for a typed default.
  expect_null(sc$scenarioName)
  expect_null(sc$modelFile)
  expect_null(sc$applicationProtocol)
  expect_null(sc$individualId)
  expect_null(sc$populationId)
  expect_null(sc$outputPaths)
  expect_identical(sc$simulationType, "Individual")
  expect_false(sc$readPopulationFromCSV)
  expect_false(sc$simulateSteadyState)
  expect_null(sc$simulationTime)
  expect_null(sc$simulationTimeUnit)
  expect_identical(sc$steadyStateTime, 1000)
  expect_null(sc$steadyStateTimeUnit)
  expect_false(sc$overwriteFormulasInSS)
  expect_null(sc$modelParameterSets)
})

test_that("as.list(Scenario()) exposes exactly the v2.0 schema fields", {
  sc <- Scenario()

  expect_named(
    as.list(sc),
    c(
      "scenarioName",
      "modelFile",
      "applicationProtocol",
      "individualId",
      "populationId",
      "outputPaths",
      "simulationType",
      "readPopulationFromCSV",
      "simulateSteadyState",
      "simulationTime",
      "simulationTimeUnit",
      "steadyStateTime",
      "steadyStateTimeUnit",
      "overwriteFormulasInSS",
      "modelParameterSets",
      "initialConditions"
    )
  )
})

test_that(".scenarioFieldNames stays in sync with the Scenario() formals", {
  # Guards against silent drift between the two sources of the field set:
  # a formal added without updating the constant would be silently dropped,
  # and a constant entry without a matching formal errors at construction.
  expect_identical(.scenarioFieldNames, names(formals(Scenario)))
})

test_that("Scenario records have copy semantics", {
  sc <- Scenario(scenarioName = "A", modelFile = "m.pkml")
  copy <- sc
  copy$modelFile <- "other.pkml"

  expect_identical(sc$modelFile, "m.pkml")
})

test_that("Scenario derives simulationType from populationId", {
  expect_identical(Scenario()$simulationType, "Individual")
  expect_identical(
    Scenario(populationId = "Pop")$simulationType,
    "Population"
  )
})

test_that(".parseScenarios returns list() for NULL input", {
  expect_identical(
    esqlabsR:::.parseScenarios(NULL, list()),
    list()
  )
})

test_that(".parseScenarios copies basic fields for an individual scenario", {
  project <- exampleProject()
  sc <- project$scenarios[["aciclovir_iv"]]

  expect_s3_class(sc, "Scenario")
  expect_identical(sc$scenarioName, "aciclovir_iv")
  expect_identical(sc$modelFile, "Aciclovir.pkml")
  expect_identical(sc$individualId, "adult_male")
  expect_identical(sc$applicationProtocol, "aciclovir_iv_250mg")
  expect_identical(sc$modelParameterSets, c("global", "aciclovir"))
  expect_null(sc$populationId)
  expect_identical(sc$simulationType, "Individual")
  expect_false(sc$readPopulationFromCSV)
})

test_that(".parseScenarios reads a scenario's initialConditions references", {
  raw <- list(list(
    name = "WithIC",
    modelFile = "m.pkml",
    parameterSets = list("ps1"),
    initialConditions = list("ic1", "ic2")
  ))
  sc <- esqlabsR:::.parseScenarios(raw, list())[["WithIC"]]
  expect_identical(sc$initialConditions, c("ic1", "ic2"))
})

test_that(".parseScenarios leaves initialConditions NULL when JSON omits it", {
  raw <- list(list(name = "NoIC", modelFile = "m.pkml"))
  sc <- esqlabsR:::.parseScenarios(raw, list())[["NoIC"]]
  expect_null(sc$initialConditions)
})

test_that("a scenario's initialConditions round-trips through serialize/parse", {
  sc <- Scenario(
    scenarioName = "RT",
    modelFile = "m.pkml",
    initialConditions = c("ic1", "ic2")
  )
  json <- esqlabsR:::.scenarioToJson(sc, outputPaths = list())
  expect_identical(json$initialConditions, list("ic1", "ic2"))

  reparsed <- esqlabsR:::.parseScenarios(
    list(stats::setNames(
      list(json$name, json$modelFile, json$initialConditions),
      c("name", "modelFile", "initialConditions")
    )),
    list()
  )[["RT"]]
  expect_identical(reparsed$initialConditions, c("ic1", "ic2"))
})

test_that(".parseScenarios sets simulationType=Population when populationId present", {
  project <- exampleProject()
  sc <- project$scenarios[["aciclovir_iv_population"]]

  expect_identical(sc$populationId, "european_adults")
  expect_identical(sc$simulationType, "Population")
})

test_that(".parseScenarios defaults applicationProtocol to NA when JSON has null", {
  raw <- list(
    list(
      name = "X",
      individual = "i",
      modelFile = "m.pkml",
      application = NULL
    )
  )
  result <- esqlabsR:::.parseScenarios(raw, list())

  expect_length(result, 1L)
  expect_true(is.na(result[["X"]]$applicationProtocol))
})

test_that(".parseScenarios converts steadyStateTime to base units (minutes)", {
  project <- exampleProject()
  sc <- project$scenarios[["aciclovir_iv_steadystate"]]

  expect_true(sc$simulateSteadyState)
  # 1 hour -> 60 minutes
  expect_equal(sc$steadyStateTime, 60)
  expect_identical(sc$steadyStateTimeUnit, "h")
})

test_that(".parseScenarios coerces a whole-number steadyStateTime to double", {
  # `jsonlite::fromJSON` reads a whole number as integer; a same-unit
  # conversion preserves that type, so without an explicit coercion the parsed
  # value would be integer while a freshly built scenario's default is double,
  # breaking a byte-equivalent round trip.
  raw <- list(
    list(
      name = "WholeSS",
      individual = "i",
      modelFile = "m.pkml",
      steadyStateTime = 1000L,
      steadyStateTimeUnit = "min"
    )
  )
  sc <- esqlabsR:::.parseScenarios(raw, list())[["WholeSS"]]

  expect_type(sc$steadyStateTime, "double")
  expect_identical(sc$steadyStateTime, 1000)
})

test_that(".parseScenarios leaves simulateSteadyState=FALSE when JSON omits/sets false", {
  project <- exampleProject()
  sc <- project$scenarios[["aciclovir_iv"]]

  expect_false(sc$simulateSteadyState)
  expect_null(sc$steadyStateTimeUnit)
  # The class default of 1000 stays put when JSON's steadyStateTime is null.
  expect_identical(sc$steadyStateTime, 1000)
})

test_that(".parseScenarios errors when steadyStateTime set without unit", {
  raw <- list(
    list(
      name = "BadSS",
      individual = "i",
      modelFile = "m.pkml",
      steadyStateTime = 5,
      steadyStateTimeUnit = NULL
    )
  )
  expect_error(
    esqlabsR:::.parseScenarios(raw, list()),
    "BadSS.*steadyStateTime.*steadyStateTimeUnit"
  )
})

test_that(".parseScenarios parses simulationTime to a list of length-3 numerics", {
  project <- exampleProject()
  sc <- project$scenarios[["aciclovir_iv"]]

  expect_type(sc$simulationTime, "list")
  expect_length(sc$simulationTime, 1L)
  expect_identical(sc$simulationTime[[1L]], c(0, 24, 60))
  expect_identical(sc$simulationTimeUnit, "h")
})

test_that(".parseScenarios resolves outputPaths ids to literal outputPaths in declared order", {
  project <- exampleProject()
  sc <- project$scenarios[["aciclovir_iv_steadystate"]]

  expect_type(sc$outputPaths, "character")
  expect_length(sc$outputPaths, 2L)
  # Names are the ids, values are the literal paths; order follows JSON declaration.
  expect_named(sc$outputPaths, c("aciclovir_fat_cell", "aciclovir_pvb"))
  expect_identical(
    unname(sc$outputPaths),
    c(
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container",
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
    )
  )
})

test_that(".parseScenarios single outputPaths id resolves to a length-1 named character vector", {
  project <- exampleProject()
  sc <- project$scenarios[["aciclovir_iv"]]

  expect_type(sc$outputPaths, "character")
  expect_length(sc$outputPaths, 1L)
  expect_named(sc$outputPaths, "aciclovir_pvb")
  expect_identical(
    unname(sc$outputPaths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
})

test_that(".parseScenarios keeps unknown outputPaths ids as dangling refs (lazy)", {
  # Referential integrity is lazy: an unknown output-path id does not abort
  # the parse; it survives as a name with an NA literal path so the
  # cross-reference validator can flag it later.
  raw <- list(
    list(
      name = "BadRefs",
      individual = "i",
      modelFile = "m.pkml",
      outputPaths = list("Aciclovir_PVB", "Nope", "AlsoNope")
    )
  )
  outputPaths <- list(Aciclovir_PVB = "Organism|PVB|...")

  sc <- esqlabsR:::.parseScenarios(raw, outputPaths)[["BadRefs"]]
  expect_named(sc$outputPaths, c("Aciclovir_PVB", "Nope", "AlsoNope"))
  expect_identical(
    unname(sc$outputPaths[["Aciclovir_PVB"]]),
    "Organism|PVB|..."
  )
  expect_true(is.na(sc$outputPaths[["Nope"]]))
  expect_true(is.na(sc$outputPaths[["AlsoNope"]]))
})

test_that(".parseScenarios collapses duplicate outputPaths ids to one (first-seen order)", {
  # A repeated id is redundant, not an error: it must resolve to a single
  # entry so the path is never run or plotted twice, keeping first-seen order.
  raw <- list(
    list(
      name = "Dups",
      individual = "i",
      modelFile = "m.pkml",
      outputPaths = list("a", "a", "b", "a")
    )
  )
  outputPaths <- list(a = "PATH_A", b = "PATH_B")

  sc <- esqlabsR:::.parseScenarios(raw, outputPaths)[["Dups"]]
  expect_named(sc$outputPaths, c("a", "b"))
  expect_identical(unname(sc$outputPaths), c("PATH_A", "PATH_B"))
})

test_that(".parseScenarios leaves outputPaths NULL when JSON omits outputPaths", {
  raw <- list(
    list(
      name = "NoOutputs",
      individual = "i",
      modelFile = "m.pkml"
    )
  )
  result <- esqlabsR:::.parseScenarios(raw, list())

  expect_null(result[["NoOutputs"]]$outputPaths)
})

test_that("addScenario aborts when a referenced individual is unknown", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "bad",
      modelFile = "Aciclovir.pkml",
      individual = "Ghost"
    )
  )
})

test_that("addScenario accepts a valid initialConditions reference", {
  project <- testProject()
  addInitialConditions(project, "icset")
  addScenario(
    project,
    id = "withic",
    modelFile = "Aciclovir.pkml",
    initialConditions = "icset"
  )
  expect_identical(project$scenarios[["withic"]]$initialConditions, "icset")
})

test_that("addScenario aborts eagerly on a dangling initialConditions ref", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "bad",
      modelFile = "Aciclovir.pkml",
      initialConditions = "ghostset"
    )
  )
})

test_that("setScenario updates and clears the initialConditions reference", {
  project <- testProject()
  addInitialConditions(project, "icset")
  addScenario(project, id = "sc", modelFile = "Aciclovir.pkml")

  setScenario(project, "sc", initialConditions = "icset")
  expect_identical(project$scenarios[["sc"]]$initialConditions, "icset")

  setScenario(project, "sc", initialConditions = NULL)
  expect_null(project$scenarios[["sc"]]$initialConditions)
})

test_that("setScenario aborts eagerly on a dangling initialConditions ref", {
  project <- testProject()
  addScenario(project, id = "sc", modelFile = "Aciclovir.pkml")
  expect_snapshot(
    error = TRUE,
    setScenario(project, "sc", initialConditions = "ghostset")
  )
})

test_that("addScenario rejects NA-valued FK args", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "S",
      modelFile = "Aciclovir.pkml",
      individual = NA_character_
    )
  )
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "S",
      modelFile = "Aciclovir.pkml",
      outputPaths = c("Output1", NA_character_)
    )
  )
})

test_that("addScenario collapses duplicate outputPaths to one (first-seen order)", {
  project <- testProject()
  addScenario(
    project,
    id = "dupout",
    modelFile = "Aciclovir.pkml",
    outputPaths = c("aciclovir_pvb", "aciclovir_pvb", "aciclovir_fat_cell")
  )
  sc <- project$scenarios[["dupout"]]
  expect_named(sc$outputPaths, c("aciclovir_pvb", "aciclovir_fat_cell"))
})

test_that("setScenario collapses duplicate outputPaths to one (first-seen order)", {
  project <- testProject()
  setScenario(
    project,
    "testscenario",
    outputPaths = c(
      "aciclovir_fat_cell",
      "aciclovir_pvb",
      "aciclovir_fat_cell"
    )
  )
  sc <- project$scenarios[["testscenario"]]
  expect_named(sc$outputPaths, c("aciclovir_fat_cell", "aciclovir_pvb"))
})

test_that("removeScenario uses the id argument matching addScenario", {
  project <- testProject()
  addScenario(
    project,
    id = "toremove",
    modelFile = "Aciclovir.pkml"
  )
  expect_true("toremove" %in% names(project$scenarios))
  removeScenario(project, id = "toremove")
  expect_false("toremove" %in% names(project$scenarios))
})

test_that("addScenario and removeScenario clear the validation cache", {
  project <- testProject()
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  addScenario(project, id = "x", modelFile = "Aciclovir.pkml")
  expect_false(project$validatedSinceMutation)

  project$.markValidated()
  removeScenario(project, id = "x")
  expect_false(project$validatedSinceMutation)
})

test_that("addScenario stores steadyStateTime in base units and round-trips the declared unit", {
  project <- testProject()
  addScenario(
    project,
    id = "ss",
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  )

  # Stored value is the base unit (minutes): 10 h -> 600 min.
  expect_equal(project$scenarios[["ss"]]$steadyStateTime, 600)
  expect_equal(project$scenarios[["ss"]]$steadyStateTimeUnit, "h")

  # Saved JSON carries the declared 10 / "h" (the serializer converts the
  # base-unit value back to the declared unit).
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  savedSS <- Filter(\(s) identical(s[["name"]], "ss"), raw$scenarios)[[1]]
  expect_equal(savedSS$steadyStateTime, 10)
  expect_equal(savedSS$steadyStateTimeUnit, "h")

  # Reload round-trips back to the base-unit value.
  reloaded <- loadProject(out)
  expect_equal(reloaded$scenarios[["ss"]]$steadyStateTime, 600)
  expect_equal(reloaded$scenarios[["ss"]]$steadyStateTimeUnit, "h")
})

# setScenario ----

test_that("setScenario changes a field and persists to file and memory", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")

  setScenario(project, "testscenario", simulationTime = "0, 48, 120")

  expect_equal(
    project$scenarios[["testscenario"]]$simulationTime,
    list(c(0, 48, 120))
  )
  reloaded <- loadProject(project$jsonPath)
  expect_equal(
    reloaded$scenarios[["testscenario"]]$simulationTime,
    list(c(0, 48, 120))
  )
  expect_true(file.exists(file.path(dir, "testscenario.json")))
})

test_that("setScenario partial update leaves other fields untouched", {
  project <- testProject()
  before <- project$scenarios[["testscenario"]]

  setScenario(project, "testscenario", simulationTimeUnit = "min")
  after <- project$scenarios[["testscenario"]]

  expect_equal(after$simulationTimeUnit, "min")
  # Every other field is unchanged.
  for (f in setdiff(names(before), "simulationTimeUnit")) {
    expect_equal(after[[f]], before[[f]])
  }
})

test_that("setScenario invalidates the validation cache", {
  project <- testProject()
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  setScenario(project, "testscenario", simulationTimeUnit = "min")

  expect_false(project$validatedSinceMutation)
})

test_that("setScenario can clear an optional field with NULL", {
  project <- testProject()
  expect_false(is.null(project$scenarios[["populationscenario"]]$individualId))

  setScenario(project, "populationscenario", individual = NULL)

  expect_null(project$scenarios[["populationscenario"]]$individualId)
  reloaded <- loadProject(project$jsonPath)
  expect_null(reloaded$scenarios[["populationscenario"]]$individualId)
})

test_that("setScenario aborts on a non-existent scenario, no file written", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  before <- list.files(dir)

  expect_snapshot(
    error = TRUE,
    setScenario(project, "Ghost", simulationTimeUnit = "min")
  )
  expect_setequal(list.files(dir), before)
})

test_that("setScenario fails fast on a structural violation, disk and memory untouched", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  before <- project$scenarios[["testscenario"]]
  beforeFile <- readLines(file.path(dir, "testscenario.json"))

  # Clearing modelFile is a structural violation; the write-through must abort.
  expect_error(
    setScenario(project, "testscenario", modelFile = NULL),
    "modelFile"
  )
  # Neither memory nor disk changed.
  expect_equal(project$scenarios[["testscenario"]], before)
  expect_identical(readLines(file.path(dir, "testscenario.json")), beforeFile)
})

test_that("setScenario rejects an unknown foreign key like addScenario", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    setScenario(project, "testscenario", individual = "Ghost")
  )
})

test_that("the write path allows a dangling outputPathId as a lazy referential finding", {
  project <- testProject()
  # The authoring functions check references eagerly, but the lower-level
  # write-through entry point (`.setSection()`) is structural-only: an unknown
  # output-path id is allowed at write time (referential checks are lazy); it
  # is the cross-reference validator that flags it later.
  scenarios <- project$.getSection("scenarios")
  sc <- scenarios[["testscenario"]]
  sc$outputPaths <- c(sc$outputPaths, Ghost = NA_character_)
  scenarios[["testscenario"]] <- sc
  expect_no_error(project$.setSection("scenarios", scenarios))
})

test_that("setScenario on a clone leaves the source's on-disk tree untouched", {
  source <- testProject()
  dir <- file.path(source$projectDirPath, "definitions", "scenarios")
  sourceFile <- readLines(file.path(dir, "testscenario.json"))

  clone <- source$clone()
  setScenario(clone, "testscenario", simulationTimeUnit = "min")

  # The clone changed in memory only; the source's file is untouched.
  expect_equal(clone$scenarios[["testscenario"]]$simulationTimeUnit, "min")
  expect_identical(readLines(file.path(dir, "testscenario.json")), sourceFile)
})

# renameScenario ----

test_that("renameScenario moves the entity file and changes the in-memory key", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  before <- readLines(file.path(dir, "testscenario.json"))

  renameScenario(project, "testscenario", "renamed")

  # In-memory: old key gone, new key present.
  expect_false("testscenario" %in% names(project$scenarios))
  expect_true("renamed" %in% names(project$scenarios))
  # On disk: old file removed, new file written.
  expect_false(file.exists(file.path(dir, "testscenario.json")))
  expect_true(file.exists(file.path(dir, "renamed.json")))

  # Content is preserved up to the name field: the only difference between the
  # old and new file is the line carrying the scenario name (now the new id).
  after <- readLines(file.path(dir, "renamed.json"))
  changed <- which(before != after)
  expect_length(changed, 1L)
  expect_match(before[changed], "testscenario")
  expect_match(after[changed], "renamed")
})

test_that("renameScenario updates the record's stored name so a reload round-trips", {
  project <- testProject()

  renameScenario(project, "testscenario", "renamed")

  expect_equal(project$scenarios[["renamed"]]$scenarioName, "renamed")
  # A reload re-derives scenarios from the tree; the new key must validate and
  # round-trip (name == key invariant holds).
  reloaded <- loadProject(project$jsonPath)
  expect_true("renamed" %in% names(reloaded$scenarios))
  expect_equal(reloaded$scenarios[["renamed"]]$scenarioName, "renamed")
  expect_no_error(validateProject(reloaded))
})

test_that("renameScenario errors clearly on a non-existent id", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    renameScenario(project, "Ghost", "renamed")
  )
})

test_that("renameScenario errors when the target id already exists", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    renameScenario(project, "testscenario", "populationscenario")
  )
})

test_that("renameScenario canonicalizes newId, warning and landing on the canonical form", {
  project <- testProject()
  expect_snapshot(
    renameScenario(project, "testscenario", "New Name")
  )
  expect_true("new name" %in% names(project$scenarios))
  expect_false("testscenario" %in% names(project$scenarios))
})

test_that("renameScenario on a clone leaves the source's on-disk tree untouched", {
  source <- testProject()
  dir <- file.path(source$projectDirPath, "definitions", "scenarios")
  sourceFiles <- list.files(dir)

  clone <- source$clone()
  renameScenario(clone, "testscenario", "renamed")

  # The clone changed in memory only; the source's tree is untouched.
  expect_true("renamed" %in% names(clone$scenarios))
  expect_setequal(list.files(dir), sourceFiles)
})

# duplicateScenario ----

test_that("duplicateScenario creates an independent on-disk and in-memory copy", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")

  duplicateScenario(project, "testscenario", "copy")

  # Both exist in memory; the original is untouched.
  expect_true(all(c("testscenario", "copy") %in% names(project$scenarios)))
  expect_equal(project$scenarios[["copy"]]$scenarioName, "copy")
  # The copy is a new entity file alongside the original.
  expect_true(file.exists(file.path(dir, "testscenario.json")))
  expect_true(file.exists(file.path(dir, "copy.json")))
})

test_that("duplicateScenario produces an independent copy: mutating it leaves the original", {
  project <- testProject()
  originalBefore <- project$scenarios[["testscenario"]]

  duplicateScenario(project, "testscenario", "copy")
  setScenario(project, "copy", simulationTimeUnit = "min")

  expect_equal(project$scenarios[["copy"]]$simulationTimeUnit, "min")
  # The original record (and its file) is unchanged.
  expect_equal(project$scenarios[["testscenario"]], originalBefore)
  reloaded <- loadProject(project$jsonPath)
  expect_equal(
    reloaded$scenarios[["testscenario"]]$simulationTimeUnit,
    originalBefore$simulationTimeUnit
  )
})

test_that("duplicateScenario errors on a non-existent source id", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    duplicateScenario(project, "Ghost", "copy")
  )
})

test_that("duplicateScenario errors when the target id already exists", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    duplicateScenario(project, "testscenario", "populationscenario")
  )
})

test_that("duplicateScenario on a clone leaves the source's on-disk tree untouched", {
  source <- testProject()
  dir <- file.path(source$projectDirPath, "definitions", "scenarios")
  sourceFiles <- list.files(dir)

  clone <- source$clone()
  duplicateScenario(clone, "testscenario", "copy")

  expect_true("copy" %in% names(clone$scenarios))
  expect_setequal(list.files(dir), sourceFiles)
})

# Vectorized authoring ----

test_that("addScenario adds N scenarios in one call equal to N scalar adds", {
  vectorized <- testProject()
  addScenario(
    vectorized,
    c("s1", "s2"),
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    outputPaths = "aciclovir_pvb"
  )

  scalar <- testProject()
  addScenario(
    scalar,
    "s1",
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    outputPaths = "aciclovir_pvb"
  )
  addScenario(
    scalar,
    "s2",
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    outputPaths = "aciclovir_pvb"
  )

  expect_identical(
    vectorized$scenarios[c("s1", "s2")],
    scalar$scenarios[c("s1", "s2")]
  )
})

test_that("removeScenario warns when a dataCombined still references it", {
  project <- testProject()
  addDataCombined(
    project,
    "dc_ref",
    simulated = list(
      list(
        label = "ref",
        scenario = "testscenario",
        path = "Organism|A|Concentration"
      )
    )
  )
  expect_snapshot(removeScenario(project, "testscenario"))
})

test_that("addScenario recycles a scalar field and applies outputPaths whole", {
  project <- testProject()
  addScenario(
    project,
    c("s1", "s2"),
    modelFile = "Aciclovir.pkml",
    individual = c("indiv1", NULL),
    outputPaths = c("aciclovir_pvb", "aciclovir_fat_cell")
  )
  expect_identical(project$scenarios$s1$modelFile, "Aciclovir.pkml")
  expect_identical(project$scenarios$s2$modelFile, "Aciclovir.pkml")
  # outputPaths applied whole to both scenarios.
  expect_identical(
    names(project$scenarios$s1$outputPaths),
    c("aciclovir_pvb", "aciclovir_fat_cell")
  )
  expect_identical(
    names(project$scenarios$s2$outputPaths),
    c("aciclovir_pvb", "aciclovir_fat_cell")
  )
})

test_that("addScenario persists all N to disk in one write-through", {
  project <- testProject()
  addScenario(project, c("s1", "s2"), modelFile = "Aciclovir.pkml")
  reloaded <- loadProject(project$jsonPath)
  expect_true(all(c("s1", "s2") %in% names(reloaded$scenarios)))
})

test_that("addScenario aborts the whole batch and writes nothing on a bad reference", {
  project <- testProject()
  before <- names(project$scenarios)
  expect_error(
    addScenario(
      project,
      c("s1", "s2"),
      modelFile = "Aciclovir.pkml",
      individual = c("indiv1", "ghost")
    )
  )
  expect_identical(names(project$scenarios), before)
  reloaded <- loadProject(project$jsonPath)
  expect_identical(names(reloaded$scenarios), before)
})

test_that("addScenario aborts on a mismatched scalar field length", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      c("s1", "s2", "s3"),
      modelFile = c("A.pkml", "B.pkml")
    )
  )
})

test_that("setScenario vectorizes a partial update across N ids", {
  project <- testProject()
  addScenario(project, c("s1", "s2"), modelFile = "Aciclovir.pkml")
  setScenario(project, c("s1", "s2"), simulationTimeUnit = c("min", "s"))
  expect_identical(project$scenarios$s1$simulationTimeUnit, "min")
  expect_identical(project$scenarios$s2$simulationTimeUnit, "s")
  expect_identical(project$scenarios$s1$modelFile, "Aciclovir.pkml")
})

test_that("removeScenario removes a vector of ids in one write-through", {
  project <- testProject()
  addScenario(project, c("s1", "s2"), modelFile = "Aciclovir.pkml")
  removeScenario(project, c("s1", "s2"))
  expect_false(any(c("s1", "s2") %in% names(project$scenarios)))
  reloaded <- loadProject(project$jsonPath)
  expect_false(any(c("s1", "s2") %in% names(reloaded$scenarios)))
})

# Print method ----

test_that("print.Scenario renders the configured fields", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$scenarios[["testscenario"]]))
})
