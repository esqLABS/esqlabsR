test_that("Project$new() creates an empty in-memory project", {
  project <- Project$new()
  expect_s3_class(project, "Project")
  expect_null(project$projectFilePath)
  expect_null(project$projectDirPath)
  expect_false(.isValidated(project))
})

test_that("Project$new(path) loads a v2.0 JSON file", {
  project <- Project$new(
    testthat::test_path("data", "TestProject", "Project.json")
  )
  expect_s3_class(project, "Project")
  expect_equal(project$schemaVersion, "2.0")
  expect_false(.isValidated(project))
})

test_that("asList round-trips with .projectToJson", {
  project <- testProject()
  expect_identical(project$asList, esqlabsR:::.projectToJson(project))
})

test_that("ProjectConfiguration() wrapper emits lifecycle warning and returns Project", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    project <- ProjectConfiguration(
      testthat::test_path("data", "TestProject", "Project.json")
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_s3_class(project, "Project")
})
test_that("project$modelFolder resolves a relative path against projectDirPath", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$modelFolder,
    fs::path_abs(file.path(project$projectDirPath, "Models/Simulations"))
  )
})

test_that("project$configurationsFolder resolves a relative path against projectDirPath", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$configurationsFolder,
    fs::path_abs(file.path(project$projectDirPath, "Configurations"))
  )
})

test_that("project$populationsFolder resolves relative to projectDirPath", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$populationsFolder,
    fs::path_abs(file.path(project$projectDirPath, "Populations"))
  )
})

test_that("project$dataFolder resolves relative to projectDirPath", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$dataFolder,
    fs::path_abs(file.path(project$projectDirPath, "Data"))
  )
})

test_that("project$dataFolder is NULL when filePaths.dataFolder is unset", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = structure(list(), names = character(0)),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list()
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  expect_null(project$dataFolder)
})

test_that("project path fields are writable after the merger", {
  project <- testProject()

  project$modelFolder <- "AnotherModels"
  expect_match(project$modelFolder, "AnotherModels$")
})

test_that(".clean_path expands env vars (other than PATH) and resolves to absolute", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  withr::local_envvar(MY_TEST_ROOT = tempdir())
  resolved <- project$.__enclos_env__$private$.clean_path(
    "$MY_TEST_ROOT/sub",
    parent = NULL
  )
  expect_equal(
    resolved,
    fs::path_abs(file.path(tempdir(), "sub"))
  )
})

test_that(".clean_path returns NULL on NULL/NA/zero-length input", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  cp <- project$.__enclos_env__$private$.clean_path
  expect_null(cp(NULL, parent = NULL))
  expect_null(cp(NA_character_, parent = NULL))
  expect_null(cp(character(0), parent = NULL))
})


# Tests for the v2.0 Project.json parser. These tests cover only the parser
# and the internal `Project` class — the parser is not wired into runScenarios,
# validators, or plotting on this branch. Tests use `:::` because both are
# intentionally unexported.

test_that("loadProject() returns an internal Project from the bundled example", {
  project <- exampleProject()

  expect_s3_class(project, "Project")
  expect_s3_class(project, "R6")
  expect_identical(project$schemaVersion, "2.0")
  expect_identical(project$esqlabsRVersion, "6.0.0")
})

test_that("loadProject() captures jsonPath and projectDirPath", {
  path <- exampleProjectPath()
  project <- loadProject(path)

  expect_identical(
    normalizePath(project$jsonPath, winslash = "/"),
    normalizePath(path, winslash = "/")
  )
  expect_identical(project$projectDirPath, dirname(project$jsonPath))
})

test_that("loadProject() exposes filePaths verbatim", {
  project <- exampleProject()

  expect_type(project$filePaths, "list")
  expect_identical(project$filePaths$modelFolder, "Models/Simulations/")
  expect_identical(project$filePaths$dataFolder, "Data/")
  # The Excel-bridge sheet names live in the separate `excel` block now.
  expect_identical(project$excel$configurationsFolder, "Configurations/")
})

test_that("loadProject() preserves outputPaths as a named list", {
  project <- exampleProject()

  expect_type(project$outputPaths, "list")
  expect_named(
    project$outputPaths,
    c("aciclovir_pvb", "aciclovir_fat_cell"),
    ignore.order = TRUE
  )
  expect_identical(
    project$outputPaths$aciclovir_pvb,
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
})

test_that("loadProject() parses scenarios into Scenario objects keyed by name", {
  project <- exampleProject()

  expect_type(project$scenarios, "list")
  expect_length(project$scenarios, 3L)
  expect_named(
    project$scenarios,
    c("aciclovir_iv", "aciclovir_iv_population", "aciclovir_iv_steadystate")
  )

  first <- project$scenarios[["aciclovir_iv"]]
  expect_s3_class(first, "Scenario")
  expect_identical(first$scenarioName, "aciclovir_iv")
  expect_identical(first$individualId, "adult_male")
  expect_null(first$populationId)
  expect_identical(first$modelParameterSets, c("global", "aciclovir"))
})

test_that("loadProject() preserves parameterSets as a single named list of sets", {
  project <- exampleProject()

  # The three former parameter-set kinds (model / individual / application)
  # now share one `parameterSets` map keyed by set id.
  expect_named(
    project$parameterSets,
    c(
      "global",
      "aciclovir",
      "adult_male_default",
      "aciclovir_iv_250mg_default"
    ),
    ignore.order = TRUE
  )
  expect_length(project$parameterSets$global, 1L)
  expect_identical(
    project$parameterSets$global[[1L]]$parameterName,
    "EHC continuous fraction"
  )
  expect_identical(
    project$parameterSets$adult_male_default[[1L]]$parameterName,
    "GFR"
  )
  expect_identical(
    project$parameterSets$aciclovir_iv_250mg_default[[1L]]$parameterName,
    "Dose"
  )
})

test_that("loadProject() reads initialConditions and the binding is read-only", {
  project <- testProject()
  # The TestProject fixture carries one initial-condition set referenced by a
  # scenario; the binding surfaces it keyed by set id.
  expect_true("testinitialset" %in% names(project$initialConditions))
  set <- project$initialConditions$testinitialset
  expect_s3_class(set, "InitialConditionSet")
  expect_identical(set[[1L]]$path, "Organism|VenousBlood|Plasma|Aciclovir")

  expect_error(project$initialConditions <- list(), "read-only")
})

test_that("a project with no initial-conditions tree surfaces an empty section", {
  blank <- loadProject(system.file(
    "extdata",
    "projects",
    "Blank",
    "Project.json",
    package = "esqlabsR"
  ))
  expect_identical(.unwrapDefinitionList(blank$initialConditions), list())
})

test_that("loadProject() preserves individuals as a named list keyed by individualId", {
  project <- exampleProject()

  expect_named(project$individuals, "adult_male")
  ind <- project$individuals[["adult_male"]]
  expect_s3_class(ind, "Individual")
  expect_identical(ind$gender, "MALE")
  expect_identical(ind$parameterSets, "adult_male_default")
})

test_that("loadProject() preserves populations as a named list keyed by populationId", {
  project <- exampleProject()

  expect_named(project$populations, "european_adults")
  pop <- project$populations[["european_adults"]]
  expect_s3_class(pop, "Population")
  expect_identical(pop$numberOfIndividuals, 50)
})

test_that("loadProject() preserves applications as a named list keyed by protocol name", {
  project <- exampleProject()

  expect_named(project$applications, "aciclovir_iv_250mg")
  app <- project$applications[["aciclovir_iv_250mg"]]
  expect_s3_class(app, "Application")
  expect_identical(app$parameterSets, "aciclovir_iv_250mg_default")
})

test_that("loadProject() preserves the observedData section", {
  project <- exampleProject()

  expect_length(project$observedData, 1L)
  source <- project$observedData[[1L]]
  expect_identical(source$type, "excel")
  expect_identical(source$file, "Aciclovir_TimeValuesData.xlsx")
  expect_identical(source$sheets, list("Laskin 1982.Group A"))
})

test_that("loadProject() parses plots into three top-level keyed sections", {
  project <- exampleProject()

  # dataCombined: named list keyed by id (no `dataCombinedId` field on entries)
  expect_named(project$dataCombined, "aciclovir_individual")
  dc <- project$dataCombined$aciclovir_individual
  expect_named(dc, c("simulated", "observed"), ignore.order = TRUE)
  expect_length(dc$simulated, 1L)
  expect_length(dc$observed, 1L)
  # plots / plotGrids: keyed lists, each entry a classed named list of its
  # rationalized fields.
  expect_named(project$plots, "p1")
  expect_named(project$plotGrids, "individual_diagnostics")
  p1 <- project$plots$p1
  expect_s3_class(p1, "Plot")
  expect_identical(p1$plotId, "p1")
  expect_identical(p1$dataCombinedId, "aciclovir_individual")
  grid <- project$plotGrids$individual_diagnostics
  expect_s3_class(grid, "PlotGrid")
  expect_identical(grid$plotGridId, "individual_diagnostics")
})

test_that("loadProject() rejects a missing schemaVersion", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(esqlabsRVersion = "6.0.0"), tmp, auto_unbox = TRUE)

  expect_error(
    loadProject(tmp),
    "Unsupported schemaVersion"
  )
})

test_that("loadProject() rejects a non-2.0 schemaVersion", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "1.0", esqlabsRVersion = "5.0.0"),
    tmp,
    auto_unbox = TRUE
  )

  expect_error(
    loadProject(tmp),
    "Unsupported schemaVersion"
  )
})

test_that("loadProject() rejects a missing file", {
  expect_error(
    loadProject(tempfile(fileext = ".json")),
    "(does not exist|not found)"
  )
})

test_that("loadProject() wraps a non-JSON file with file context", {
  tmp <- withr::local_tempfile(fileext = ".json")
  writeLines("this is not valid json {", tmp)

  # The path is environment-specific, so match the wrapping message; the
  # underlying jsonlite lexical error is attached as the parent.
  expect_error(
    loadProject(tmp),
    "Failed to parse.*as JSON"
  )
})

test_that("loadProject() rejects a non-string path", {
  expect_error(
    loadProject(NULL),
    "must be a single non-empty, non-NA string"
  )
  expect_error(
    loadProject(c("a.json", "b.json")),
    "must be a single non-empty, non-NA string"
  )
  expect_error(
    loadProject(""),
    "must be a single non-empty, non-NA string"
  )
})

test_that("loadProject() defaults missing optional sections to empty lists", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "2.0", esqlabsRVersion = "6.0.0"),
    tmp,
    auto_unbox = TRUE
  )

  project <- loadProject(tmp)

  # Section accessors wrap the stored list in a printable DefinitionList; unwrap
  # to assert the underlying absent-vs-empty shape the parser produced.
  expect_identical(project$filePaths, structure(list(), names = character(0L)))
  expect_identical(.unwrapDefinitionList(project$outputPaths), list())
  expect_identical(.unwrapDefinitionList(project$scenarios), list())
  expect_identical(.unwrapDefinitionList(project$parameterSets), list())
  expect_identical(.unwrapDefinitionList(project$individuals), list())
  expect_identical(.unwrapDefinitionList(project$populations), list())
  expect_identical(
    .unwrapDefinitionList(project$applications),
    structure(list(), names = character(0L))
  )
  expect_identical(.unwrapDefinitionList(project$observedData), list())
  expect_identical(.unwrapDefinitionList(project$dataCombined), list())
  expect_identical(.unwrapDefinitionList(project$plots), list())
  expect_identical(.unwrapDefinitionList(project$plotGrids), list())
})

test_that("Project$print() renders the example project through ospPrint*", {
  project <- exampleProject()

  # `print()` shows file locations relative to the project directory, so the
  # output carries no machine-specific absolute path and needs no redaction or
  # width override to stay reproducible across operating systems.
  expect_snapshot(print(project))
})

# Container rework: metadata, definitionsFolder, the filePaths/excel split,
# and defaultSimulationRunOptions.

test_that("loadProject() exposes name and description metadata", {
  project <- exampleProject()
  expect_identical(project$name, "Example")
  expect_identical(project$description, "Aciclovir IV PK example project")
})

test_that("name and description are writable and persist on saveProject()", {
  project <- exampleProject()
  project$name <- "Renamed"
  project$description <- "A new description"
  expect_identical(project$name, "Renamed")
  expect_identical(project$description, "A new description")

  # The edit stays in memory until an explicit save.
  expect_true(.isModified(project))
  expect_false(identical(loadProject(project$jsonPath)$name, "Renamed"))

  saveProject(project)
  reloaded <- loadProject(project$jsonPath)
  expect_identical(reloaded$name, "Renamed")
  expect_identical(reloaded$description, "A new description")
})

test_that("filePaths holds only the four live working folders", {
  project <- exampleProject()
  expect_named(
    project$filePaths,
    c("modelFolder", "populationsFolder", "dataFolder", "outputFolder"),
    ignore.order = TRUE
  )
})

test_that("excel exposes the seven Excel-bridge sheet-name fields", {
  project <- exampleProject()
  expect_named(
    project$excel,
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
  expect_identical(project$excel$modelParamsFile, "ModelParameters.xlsx")
})

test_that("excel is read-only", {
  project <- exampleProject()
  expect_error(project$excel <- list(), "readonly")
})

test_that("a from-scratch project carries no excel block", {
  project <- Project$new()
  expect_length(project$excel, 0L)
})

test_that("definitionsFolder defaults to 'definitions' and is reported", {
  project <- exampleProject()
  expect_identical(project$definitionsFolder, "definitions")
})

test_that("a legacy flat-filePaths Project.json loads and splits the fields", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      esqlabsRVersion = "6.0.0",
      filePaths = list(
        modelFolder = "Models/",
        configurationsFolder = "Configurations/",
        modelParamsFile = "ModelParameters.xlsx",
        dataFolder = "Data/",
        outputFolder = "Results/"
      ),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list()
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)

  expect_named(
    project$filePaths,
    c("modelFolder", "dataFolder", "outputFolder"),
    ignore.order = TRUE
  )
  expect_named(
    project$excel,
    c("configurationsFolder", "modelParamsFile"),
    ignore.order = TRUE
  )
})

test_that("definitionsFolder honors a non-default tree location", {
  # Write a tree project under a custom definitions folder, then load it.
  # Re-pointing the folder is a pure in-memory change now (no clone, no
  # materialized-tree guard), so set it directly on the loaded project.
  src <- exampleProject()
  src$definitionsFolder <- "defs"
  dir <- withr::local_tempdir("custom_defs_")
  esqlabsR:::.writeProjectTree(src, dir)

  expect_true(dir.exists(file.path(dir, "defs", "scenarios")))
  expect_false(dir.exists(file.path(dir, "definitions")))

  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_identical(reloaded$definitionsFolder, "defs")
  expect_length(reloaded$scenarios, 3L)
})

test_that("changing definitionsFolder is a pure in-memory change", {
  # Under explicit-save re-pointing the folder is always allowed; it only
  # changes where the next saveProject() writes, so it sets the dirty bit and
  # touches nothing on disk.
  temp <- with_temp_project()
  project <- temp$project
  expect_true(dir.exists(file.path(temp$path, "definitions")))

  project$definitionsFolder <- "other-defs"
  expect_identical(project$definitionsFolder, "other-defs")
  expect_true(.isModified(project))
  # No tree moved on disk until a save.
  expect_true(dir.exists(file.path(temp$path, "definitions")))
  expect_false(dir.exists(file.path(temp$path, "other-defs")))

  # An in-memory project (no directory) may re-point the folder freely too.
  inMemory <- Project$new()
  inMemory$definitionsFolder <- "defs"
  expect_identical(inMemory$definitionsFolder, "defs")
})

test_that("the tree wins over a conflicting non-empty inline Project.json section", {
  # A tree project never writes an inline copy of a section, so a non-empty
  # inline section that disagrees with the tree can only arise from hand-editing
  # the Project.json. Construct that conflicting state directly and assert the
  # loader takes the tree value and ignores the stale inline copy.
  project <- testProject()
  jsonPath <- project$jsonPath
  treeDir <- file.path(dirname(jsonPath), "definitions", "scenarios")
  expect_true(dir.exists(treeDir))

  raw <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  expect_length(raw$scenarios, 0L)

  # Inject a conflicting inline scenario for an id that also lives in the tree,
  # giving it a different modelFile than the tree definition carries.
  treeModelFile <- project$scenarios[["testscenario"]]$modelFile
  raw$scenarios <- list(
    list(
      name = "testscenario",
      modelFile = "INLINE_SHOULD_NOT_WIN.pkml"
    )
  )
  writeLines(
    jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null", pretty = TRUE),
    jsonPath
  )

  reloaded <- loadProject(jsonPath)
  expect_identical(
    reloaded$scenarios[["testscenario"]]$modelFile,
    treeModelFile
  )
  expect_false(
    identical(
      reloaded$scenarios[["testscenario"]]$modelFile,
      "INLINE_SHOULD_NOT_WIN.pkml"
    )
  )
})

test_that("saveProject() writes an empty-sections container yet the tree restores them on reload", {
  treeSections <- c(
    "scenarios",
    "individuals",
    "populations",
    "parameterSets",
    "applications",
    "outputPaths",
    "observedData",
    "dataCombined",
    "plots",
    "plotGrids",
    "parameterIdentification"
  )
  project <- exampleProject()
  before <- vapply(treeSections, \(s) length(project[[s]]), integer(1))
  expect_gt(sum(before), 0L)

  # After a container-metadata edit and save, the container holds only the
  # container itself; the tree owns the sections, so they are not re-inlined.
  project$name <- "RenamedX"
  saveProject(project)
  onDisk <- jsonlite::fromJSON(project$jsonPath, simplifyVector = FALSE)
  expect_identical(onDisk$name, "RenamedX")
  expect_false(is.null(onDisk$filePaths))
  for (s in treeSections) {
    expect_length(onDisk[[s]], 0L)
  }

  # Reload restores every section from the tree, and the metadata edit stuck.
  reloaded <- loadProject(project$jsonPath)
  after <- vapply(treeSections, \(s) length(reloaded[[s]]), integer(1))
  expect_identical(after, before)
  expect_identical(reloaded$name, "RenamedX")
})

test_that("Project$print() omits zero-count definition sections", {
  project <- Project$new()
  addScenario(project, "s1", modelFile = "m.pkml")

  # Only the populated `Scenarios` section prints under `Definitions`; the
  # eleven empty sections produce no `• Label: 0` line. A from-scratch project
  # also has no working folders and no Excel side-car, so neither the `Paths`
  # nor the `Excel` header appears.
  expect_snapshot(print(project))
})

test_that("Project$print() hides the Excel section and empty sections", {
  project <- Project$new()

  # A from-scratch JSON-only project prints just the `<Project>` header: no
  # metadata bullets, no `Paths`, `Definitions`, or `Excel` headers, and no
  # stray bullets.
  expect_snapshot(print(project))
})

test_that("defaultSimulationRunOptions round-trips and defaults to NULL", {
  project <- exampleProject()
  expect_null(project$defaultSimulationRunOptions)

  project$defaultSimulationRunOptions <- list(
    numberOfCores = 2,
    checkForNegativeValues = TRUE
  )
  saveProject(project)
  reloaded <- loadProject(project$jsonPath)
  expect_equal(reloaded$defaultSimulationRunOptions$numberOfCores, 2)
  expect_true(reloaded$defaultSimulationRunOptions$checkForNegativeValues)
})

test_that("an Excel-bridge file field write targets the excel block", {
  project <- Project$new()
  project$modelParamsFile <- "X.xlsx"
  expect_match(project$modelParamsFile, "X\\.xlsx$")
  expect_identical(project$excel$modelParamsFile, "X.xlsx")
})

test_that(".markValidated leaves the validation cache set until the next mutation", {
  project <- testProject()
  .markValidated(project)
  expect_true(.isValidated(project))

  # A mutation clears the validation cache so runScenarios()/createPlots()
  # re-validate the new shape.
  .markModified(project)
  expect_false(.isValidated(project))
})

test_that("a fresh project starts unvalidated", {
  project <- testProject()
  expect_false(.isValidated(project))
})

test_that("addScenario() invalidates the validation cache", {
  project <- testProject()
  .markValidated(project)
  expect_true(.isValidated(project))

  addScenario(project, "new", modelFile = "m.pkml")

  expect_false(.isValidated(project))
})

test_that("setScenario() invalidates the validation cache", {
  project <- testProject()
  .markValidated(project)

  setScenario(project, "testscenario", modelFile = "Aciclovir.pkml")

  expect_false(.isValidated(project))
  expect_s3_class(project$scenarios[["testscenario"]], "Scenario")
})

test_that("a section-entry authoring write invalidates the validation cache", {
  project <- testProject()
  .markValidated(project)
  setIndividual(project, "indiv1", weight = 81)

  expect_false(.isValidated(project))
  expect_identical(project$individuals[["indiv1"]]$weight, 81)
})

test_that("an extracted Scenario is a copy and cannot mutate the project silently", {
  project <- testProject()
  .markValidated(project)
  sc <- project$scenarios[["testscenario"]]
  sc$modelFile <- "HIJACKED.pkml"

  # Reading and mutating a copy is not a project mutation.
  expect_true(.isValidated(project))
  expect_false(
    identical(project$scenarios[["testscenario"]]$modelFile, "HIJACKED.pkml")
  )
})

# Section accessors are read-only ----

test_that("a whole-section assignment through a section accessor is rejected", {
  project <- testProject()
  expect_snapshot(error = TRUE, project$scenarios <- list())
})

test_that("a subscript assignment through a section accessor is rejected", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    project$scenarios[["aciclovir_iv"]] <- Scenario(
      scenarioName = "aciclovir_iv",
      modelFile = "m.pkml"
    )
  )
})

test_that("a nested field assignment through a section accessor is rejected", {
  project <- testProject()
  # The most insidious form: `project$scenarios[["x"]]$field <- v` desugars to
  # the same read-modify-write the subscript form does, so it is rejected too.
  expect_snapshot(
    error = TRUE,
    project$scenarios[["testscenario"]]$individualId <- "indiv1"
  )
})

test_that("a negative-index assignment through a section accessor is rejected", {
  project <- testProject()
  expect_snapshot(error = TRUE, project$scenarios[-1] <- list())
})

test_that("the read-only block applies to every section accessor", {
  project <- testProject()
  sections <- c(
    "outputPaths",
    "scenarios",
    "parameterSets",
    "initialConditions",
    "individuals",
    "populations",
    "applications",
    "observedData",
    "dataCombined",
    "plots",
    "plotGrids",
    "parameterIdentification"
  )
  for (section in sections) {
    expect_error(
      eval(bquote(project[[.(section)]] <- list())),
      "read-only"
    )
  }
})

test_that("sub-element and negative-index assignment are rejected on every section accessor", {
  project <- testProject()
  # The scenarios section is covered by its own dedicated blocks above; the
  # DefinitionList replacement methods (`[[<-`/`[<-`/`$<-`) dispatch purely on
  # the shared wrapper class (the kind only feeds the message text), so a loop
  # over the other ten sections asserts they traverse the same read-only path.
  sections <- c(
    "outputPaths",
    "parameterSets",
    "initialConditions",
    "individuals",
    "populations",
    "applications",
    "observedData",
    "dataCombined",
    "plots",
    "plotGrids",
    "parameterIdentification"
  )
  for (section in sections) {
    expect_error(
      eval(bquote(project[[.(section)]][["new_id"]] <- list())),
      "read-only"
    )
    expect_error(
      eval(bquote(project[[.(section)]][-1] <- list())),
      "read-only"
    )
  }
})

test_that("reading a record, editing the copy, and re-submitting it is the supported edit loop", {
  project <- testProject()
  # The canonical edit loop: the accessor returns a detached copy; mutating it
  # does not touch the project, and the change lands only when re-submitted
  # through an authoring function.
  sc <- project$scenarios[["testscenario"]]
  sc$modelFile <- "Edited.pkml"
  expect_false(
    identical(project$scenarios[["testscenario"]]$modelFile, "Edited.pkml")
  )
  setScenario(project, "testscenario", modelFile = "Edited.pkml")
  expect_identical(
    project$scenarios[["testscenario"]]$modelFile,
    "Edited.pkml"
  )
})

# Under explicit-save, `.setSection()` no longer serializes on write, so a
# structurally bad record is accepted in memory but must abort the save: the
# serialize-in-memory-first guarantee in `.writeEntityTree()` (driven by
# `saveProject()`) rejects a wrong-typed reference field and an unknown field.
test_that("saveProject() rejects a wrong-typed scalar reference field", {
  project <- testProject()
  scenarios <- .getSection(project, "scenarios")
  scenarios[["testscenario"]]$individualId <- list(a = 1)
  .setSection(project, "scenarios", scenarios)
  expect_error(
    saveProject(project),
    "individualId.*single string"
  )
})

test_that("saveProject() rejects an unknown field on a record", {
  project <- testProject()
  scenarios <- .getSection(project, "scenarios")
  scenarios[["testscenario"]]$totallyBogusField <- "nonsense"
  .setSection(project, "scenarios", scenarios)
  expect_error(
    saveProject(project),
    "unknown field"
  )
})

test_that("jsonPath is read-only and aliases projectFilePath", {
  project <- testProject()
  expect_identical(project$jsonPath, project$projectFilePath)
  expect_snapshot(error = TRUE, project$jsonPath <- "elsewhere.json")
})

test_that("projectStatus() reports a clean bound project on both axes", {
  project <- testProject()

  # A freshly loaded, bound project with no Excel side-car: the tree axis is in
  # sync (no unsaved edits), the Excel axis is NA (nothing to compare).
  status <- projectStatus(project, silent = TRUE)
  expect_true(status$tree_in_sync)
  expect_identical(status$excel_in_sync, NA)
  expect_identical(status$details, list())
})

test_that("projectStatus() reports NA on both axes for an in-memory project", {
  status <- projectStatus(Project$new(), silent = TRUE)
  expect_identical(status$tree_in_sync, NA)
  expect_identical(status$excel_in_sync, NA)
})

test_that("projectStatus() reports unsaved edits on the tree axis", {
  project <- testProject()
  addOutputPath(project, "dirtypath", "Organism|A|Concentration in container")

  status <- projectStatus(project, silent = TRUE)
  expect_false(status$tree_in_sync)
})

# No clone ----

test_that("Project is not cloneable", {
  project <- testProject()
  # `cloneable = FALSE` removes the `clone` method entirely.
  expect_null(project$clone)
})

# project$status ----

test_that("project$status returns the two-axis structured shape", {
  project <- testProject()
  expect_named(project$status, c("tree_in_sync", "excel_in_sync", "details"))
})

test_that("project$status is read-only", {
  project <- testProject()
  expect_snapshot(error = TRUE, project$status <- list())
})

# Dirty bit and print marker ----

# The print output carries per-run temp-dir prefixes (the JSON path and the
# resolved working-folder paths); redact everything up to and including the
# throwaway `TestProject_<hash>` directory so the class line's marker and the
# section structure stay reviewable and portable.
.redactJsonPathLine <- function(lines) {
  gsub(".*(TestProject_[^/]+)", "<tmp>", lines)
}

test_that("print() shows the unsaved-changes marker after an edit", {
  project <- testProject()
  addOutputPath(project, "markerpath", "Organism|A|Concentration in container")
  expect_snapshot(print(project), transform = .redactJsonPathLine)
})

test_that("print() shows no marker on a freshly loaded or saved project", {
  project <- testProject()
  # Freshly loaded: clean.
  expect_snapshot(print(project), transform = .redactJsonPathLine)

  addOutputPath(project, "markerpath", "Organism|A|Concentration in container")
  saveProject(project)
  # After saving: clean again.
  expect_snapshot(print(project), transform = .redactJsonPathLine)
})

# DefinitionList section-accessor printing ----

test_that("a section accessor prints a count and the definition names", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$individuals))
  expect_snapshot(print(project$parameterSets))
})

test_that("an empty section accessor prints zero definitions", {
  project <- .fakeProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$individuals))
})

test_that("print on a section accessor returns the value invisibly", {
  project <- testProject()
  expect_invisible(print(project$individuals))
  returned <- withr::with_output_sink(
    withr::local_tempfile(),
    print(project$individuals)
  )
  expect_s3_class(returned, "DefinitionList")
})

test_that("a wrapped section accessor still behaves as a list", {
  project <- testProject()
  indivs <- project$individuals
  expect_type(indivs, "list")
  expect_length(indivs, 1L)
  expect_named(indivs, "indiv1")
  expect_s3_class(indivs[["indiv1"]], "Individual")
  # c() and named extraction are transparent.
  expect_length(c(indivs, list(extra = 1)), 2L)
})

test_that("the stored section stays a plain list (no DefinitionList class)", {
  project <- testProject()
  # Reading wraps, but the backing private store is plain: a round-trip
  # through a mutator and saveProject() persists and reloads identically.
  stored <- .unwrapDefinitionList(project$individuals)
  expect_false(inherits(stored, "DefinitionList"))

  addIndividual(project, "extra", species = "Human", gender = "MALE")
  saveProject(project)
  reloaded <- loadProject(project$jsonPath)
  expect_true("extra" %in% names(reloaded$individuals))
  expect_false(inherits(
    .unwrapDefinitionList(reloaded$individuals),
    "DefinitionList"
  ))
})

test_that("the three plots sections each print a count and ids", {
  project <- exampleProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$plots))
  expect_snapshot(print(project$plotGrids))
  expect_snapshot(print(project$dataCombined))
})
