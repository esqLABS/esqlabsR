pkmlFixture <- test_path(
  "data",
  "TestProject",
  "Models",
  "Simulations",
  "Aciclovir.pkml"
)

# createScenariosFromPKML: input handling ----

test_that("createScenariosFromPKML errors on non-Project input", {
  expect_error(
    createScenariosFromPKML(pkmlFixture, project = "not a project"),
    "Project"
  )
})

test_that("createScenariosFromPKML is a no-op returning the project for empty pkmlFilePaths", {
  project <- testProject()
  before <- names(project$scenarios)
  expect_silent(result <- createScenariosFromPKML(character(), project))
  expect_identical(result, project)
  expect_identical(names(project$scenarios), before)
})

test_that("paramSheets argument is soft-deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  project <- testProject()
  expect_warning(
    suppressMessages(createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarios = "test1",
      parameterSets = "Global",
      paramSheets = "Aciclovir"
    )),
    class = "lifecycle_warning_deprecated"
  )
})

# createScenariosFromPKML: in-place mutation ----

test_that("createScenariosFromPKML adds scenarios in place and returns the project invisibly", {
  project <- testProject()

  expect_snapshot(
    result <- createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarios = "seeded"
    )
  )

  expect_identical(result, project)
  expect_true("seeded" %in% names(project$scenarios))
  expect_s3_class(project$scenarios[["seeded"]], "Scenario")
})

# createScenariosFromPKML: output path resolution ----

test_that("PKML-extracted output paths reuse existing project ids for known literal paths", {
  project <- testProject()
  idsBefore <- names(project$outputPaths)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded"
  ))

  sc <- project$scenarios[["seeded"]]
  # The PVB path is already registered as `Aciclovir_PVB`, so its id is reused.
  expect_true("aciclovir_pvb" %in% names(sc$outputPaths))
  expect_true(all(names(sc$outputPaths) %in% names(project$outputPaths)))
})

test_that("PKML-extracted output paths register generated readable ids when unknown", {
  project <- .fakeProject(
    parameterSets = list(global = list())
  )
  project$modelFolder <- dirname(pkmlFixture)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded"
  ))

  sc <- project$scenarios[["seeded"]]
  expect_gt(length(sc$outputPaths), 0)
  # Every id used by the scenario must be registered on the project.
  expect_true(all(names(sc$outputPaths) %in% names(project$outputPaths)))
  # Generated ids are readable (built from the path's last two segments).
  expect_match(names(sc$outputPaths), "^aciclovir_", all = TRUE)
})

test_that("user-supplied named outputPaths register under the user ids", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded",
    outputPaths = c(
      plasma = "Organism|VenousBlood|Plasma|Aciclovir|Concentration"
    )
  ))

  sc <- project$scenarios[["seeded"]]
  expect_named(sc$outputPaths, "plasma")
  expect_identical(
    project$outputPaths[["plasma"]],
    "Organism|VenousBlood|Plasma|Aciclovir|Concentration"
  )
})

test_that("user-supplied outputPaths reuse the existing id when the literal path already exists", {
  project <- testProject()
  existingPath <- project$outputPaths[["aciclovir_pvb"]]
  idsBefore <- names(project$outputPaths)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded",
    # User invents a different id for an already-registered path.
    outputPaths = stats::setNames(existingPath, "myAlias")
  ))

  sc <- project$scenarios[["seeded"]]
  # The registered id wins; the user alias is dropped, no new entry added.
  expect_named(sc$outputPaths, "aciclovir_pvb")
  expect_identical(names(project$outputPaths), idsBefore)
})

test_that("user alias ignored in favour of registered id emits an inform", {
  project <- testProject()
  existingPath <- project$outputPaths[["aciclovir_pvb"]]

  expect_snapshot(
    createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarios = "seeded",
      outputPaths = stats::setNames(existingPath, "myAlias")
    )
  )
})

test_that("named outputPaths colliding with an existing id mapped to a different path abort and leave the project unchanged", {
  project <- testProject()
  idsBefore <- names(project$outputPaths)
  scenariosBefore <- names(project$scenarios)

  expect_snapshot(
    error = TRUE,
    createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarios = "seeded",
      outputPaths = c(aciclovir_pvb = "Organism|Some|Other|Path")
    )
  )

  expect_identical(names(project$outputPaths), idsBefore)
  expect_identical(names(project$scenarios), scenariosBefore)
})

test_that("comma-separated outputPaths strings are split and registered per scenario", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded",
    outputPaths = "Organism|A|Conc, Organism|B|Conc"
  ))

  sc <- project$scenarios[["seeded"]]
  expect_length(sc$outputPaths, 2)
  expect_setequal(
    unname(sc$outputPaths),
    c("Organism|A|Conc", "Organism|B|Conc")
  )
})

test_that("list-valued outputPaths assign per-scenario named vectors", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)

  suppressMessages(createScenariosFromPKML(
    c(pkmlFixture, pkmlFixture),
    project = project,
    scenarios = c("a", "b"),
    outputPaths = list(
      c(p1 = "Organism|A|Conc"),
      c(p2 = "Organism|B|Conc")
    )
  ))

  expect_named(project$scenarios[["a"]]$outputPaths, "p1")
  expect_named(project$scenarios[["b"]]$outputPaths, "p2")
})

# createScenariosFromPKML: model parameter sets ----

test_that("comma-separated parameterSets are split and FK-validated", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded",
    parameterSets = "global, aciclovir"
  ))
  expect_identical(
    project$scenarios[["seeded"]]$modelParameterSets,
    c("global", "aciclovir")
  )
})

test_that("unknown parameterSets abort and leave the project unchanged", {
  project <- testProject()
  scenariosBefore <- names(project$scenarios)
  expect_snapshot(
    error = TRUE,
    createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarios = "seeded",
      parameterSets = "DoesNotExist"
    )
  )
  expect_identical(names(project$scenarios), scenariosBefore)
})

# createScenariosFromPKML: application protocol ----

test_that("applicationProtocol defaults to NA and the seeded scenario passes validation", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded"
  ))
  expect_true(is.na(project$scenarios[["seeded"]]$applicationProtocol))
  expect_false(isAnyCriticalErrors(validateProject(project)))
})

test_that("a user application is taken verbatim without Excel sanitization", {
  project <- testProject()
  # A canonical (already-lowercase) protocol id is taken as-is: no warning,
  # no Excel-specific mangling beyond id canonicalization.
  longProtocol <- "aciclovir_iv_250mg"
  expect_no_warning(suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded",
    application = longProtocol
  )))
  expect_identical(
    project$scenarios[["seeded"]]$applicationProtocol,
    longProtocol
  )
})

test_that("an unknown application aborts", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarios = "seeded",
      application = "NoSuchProtocol"
    )
  )
})

# createScenariosFromPKML: duplicate name resolution ----

test_that("duplicate scenario names are expanded with numeric suffixes", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)
  expect_snapshot(
    createScenariosFromPKML(
      c(pkmlFixture, pkmlFixture),
      project = project,
      scenarios = c("s", "s")
    )
  )
  expect_true(all(c("s", "s_2") %in% names(project$scenarios)))
})

test_that("duplicate-name expansion respects explicit later names", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)
  suppressWarnings(suppressMessages(createScenariosFromPKML(
    rep(pkmlFixture, 3),
    project = project,
    scenarios = c("s", "s", "s_2")
  )))
  expect_true(all(c("s", "s_2", "s_2_2") %in% names(project$scenarios)))
})

test_that("scenario names colliding with pre-existing project scenarios are suffixed", {
  project <- testProject()
  suppressWarnings(suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "testscenario"
  )))
  expect_true("testscenario_2" %in% names(project$scenarios))
})

test_that("scenarios = NULL derives names from the simulation and dedupes a recycled PKML", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)
  suppressWarnings(suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    individual = NULL,
    population = NULL,
    simulationTime = c("0, 1, 1", "0, 2, 1", "0, 3, 1")
  )))
  # Three scenarios from one recycled PKML, names deduped off the sim name.
  expect_length(project$scenarios, 3)
  expect_equal(length(unique(names(project$scenarios))), 3)
})

# createScenariosFromPKML: simulation time ----

test_that("simulation time and unit are extracted from the PKML output schema", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded"
  ))
  sc <- project$scenarios[["seeded"]]
  expect_false(is.null(sc$simulationTime))
  expect_false(is.null(sc$simulationTimeUnit))
})

test_that("user simulationTime overrides PKML extraction", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded",
    simulationTime = "0, 24, 60",
    simulationTimeUnit = "h"
  ))
  sc <- project$scenarios[["seeded"]]
  expect_identical(sc$simulationTimeUnit, "h")
  expect_identical(sc$simulationTime, list(c(0, 24, 60)))
})

test_that("a user simulationTimeUnit is recorded on the extracted scenario", {
  # When simulationTime is left to PKML extraction, a user-supplied
  # simulationTimeUnit overrides the schema's own unit.
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded",
    simulationTimeUnit = "min"
  ))
  expect_identical(project$scenarios[["seeded"]]$simulationTimeUnit, "min")
})

# createScenariosFromPKML: steady state ----

test_that("steadyState = TRUE seeds a base-unit steadyStateTime and a unit", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "ss",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  ))
  sc <- project$scenarios[["ss"]]
  expect_true(sc$simulateSteadyState)
  # 10 h stored in base units (minutes).
  expect_equal(sc$steadyStateTime, 600)
  expect_identical(sc$steadyStateTimeUnit, "h")
})

test_that("steadyState defaults to 1000 min when no time is supplied", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "ss",
    steadyState = TRUE
  ))
  sc <- project$scenarios[["ss"]]
  expect_equal(sc$steadyStateTime, 1000)
  expect_identical(sc$steadyStateTimeUnit, "min")
})

test_that("seeded scenarios match addScenario-created scenarios field for field", {
  fromPkml <- testProject()
  suppressMessages(createScenariosFromPKML(
    file.path(fromPkml$modelFolder, "Aciclovir.pkml"),
    project = fromPkml,
    scenarios = "seeded",
    outputPaths = c(aciclovir_pvb = fromPkml$outputPaths[["aciclovir_pvb"]]),
    simulationTime = "0, 24, 60",
    simulationTimeUnit = "h"
  ))

  viaAdd <- testProject()
  addScenario(
    viaAdd,
    id = "seeded",
    modelFile = "Aciclovir.pkml",
    outputPaths = "aciclovir_pvb",
    simulationTime = "0, 24, 60",
    simulationTimeUnit = "h"
  )

  expect_equal(
    fromPkml$scenarios[["seeded"]],
    viaAdd$scenarios[["seeded"]]
  )
})

# createScenariosFromPKML: model file resolution ----

test_that("modelFile is stored relative to project$modelFolder as a plain character", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    file.path(project$modelFolder, "Aciclovir.pkml"),
    project = project,
    scenarios = "seeded"
  ))
  sc <- project$scenarios[["seeded"]]
  expect_identical(sc$modelFile, "Aciclovir.pkml")
  expect_type(sc$modelFile, "character")
})

test_that("NULL modelFolder falls back to the absolute pkml path with a warning", {
  project <- .fakeProject()
  expect_null(project$modelFolder)
  expect_snapshot(
    suppressMessages(createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarios = "seeded"
    ))
  )
  sc <- project$scenarios[["seeded"]]
  expect_true(fs::is_absolute_path(sc$modelFile))
})

# createScenariosFromPKML: recycling errors and rollback ----

test_that("inconsistent vector argument lengths abort", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    createScenariosFromPKML(
      rep(pkmlFixture, 2),
      project = project,
      scenarios = c("A", "B", "C")
    )
  )
})

test_that("a failing addScenario rolls back scenarios and outputPaths", {
  project <- .fakeProject(
    parameterSets = list(global = list())
  )
  project$modelFolder <- dirname(pkmlFixture)
  scenariosBefore <- names(project$scenarios)
  outputsBefore <- names(project$outputPaths)

  expect_error(
    suppressMessages(createScenariosFromPKML(
      rep(pkmlFixture, 2),
      project = project,
      scenarios = c("Ok", "Bad"),
      parameterSets = c("Global", "DoesNotExist")
    ))
  )

  expect_identical(names(project$scenarios), scenariosBefore)
  expect_identical(names(project$outputPaths), outputsBefore)
})

test_that("a failing addScenario rollback preserves validatedSinceMutation", {
  project <- .fakeProject(
    parameterSets = list(global = list())
  )
  project$modelFolder <- dirname(pkmlFixture)
  project$.markValidated()

  expect_error(
    suppressMessages(createScenariosFromPKML(
      rep(pkmlFixture, 2),
      project = project,
      scenarios = c("Ok", "Bad"),
      parameterSets = c("Global", "DoesNotExist")
    ))
  )

  expect_true(project$validatedSinceMutation)
})

test_that("a failing addScenario rolls back the on-disk scenario tree", {
  # The in-memory rollback tests use `.fakeProject()` (no directory), so the
  # branch that writes the restored section back to disk is never hit. Use a
  # real on-disk project so the rollback materializes to `definitions/scenarios`.
  project <- testProject()
  project$modelFolder <- dirname(pkmlFixture)
  scenariosDir <- file.path(
    project$projectDirPath,
    project$definitionsFolder,
    "scenarios"
  )
  filesBefore <- sort(list.files(scenariosDir))

  expect_error(
    suppressMessages(createScenariosFromPKML(
      rep(pkmlFixture, 2),
      project = project,
      scenarios = c("Ok", "Bad"),
      parameterSets = c("global", "doesnotexist")
    ))
  )

  # The successful first scenario must not survive on disk.
  expect_identical(sort(list.files(scenariosDir)), filesBefore)
  reloaded <- loadProject(project$projectFilePath)
  expect_named(
    reloaded$scenarios,
    names(project$scenarios),
    ignore.order = TRUE
  )
  expect_false("ok" %in% names(reloaded$scenarios))
})

# createScenariosFromPKML: end-to-end round trips ----

test_that("end-to-end: seed from PKML, snapshot, loadProject round-trips", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "seeded"
  ))

  path <- snapshotProject(project, dir = withr::local_tempdir())
  reloaded <- loadProject(path)

  expect_equal(
    reloaded$scenarios[["seeded"]],
    project$scenarios[["seeded"]]
  )
  expect_equal(reloaded$outputPaths, project$outputPaths)
})

test_that("end-to-end: steadyState = TRUE round-trips through snapshot and load", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarios = "ss",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  ))

  path <- snapshotProject(project, dir = withr::local_tempdir())
  reloaded <- loadProject(path)

  expect_equal(reloaded$scenarios[["ss"]], project$scenarios[["ss"]])
})

# Internal helpers ----

test_that(".dedupeScenarioNames probes suffixes against project and call names", {
  out <- suppressWarnings(.dedupeScenarioNames(c("s", "s", "s_2"), character()))
  expect_identical(out, c("s", "s_2", "s_2_2"))

  expect_warning(
    .dedupeScenarioNames("TestScenario", "TestScenario"),
    "Duplicate scenario names"
  )

  expect_silent(
    expect_identical(
      .dedupeScenarioNames(c("A", "B"), character()),
      c("A", "B")
    )
  )
})

test_that(".extractSimulationTimeFromPkml joins intervals and converts to the target unit", {
  # A self-consistent schema stub: values genuinely in base-unit minutes.
  sim <- list(
    outputSchema = list(
      intervals = list(
        list(
          startTime = list(value = 0, displayUnit = "min"),
          endTime = list(value = 60, displayUnit = "min"),
          resolution = list(value = 2)
        ),
        list(
          startTime = list(value = 60, displayUnit = "min"),
          endTime = list(value = 120, displayUnit = "min"),
          resolution = list(value = 1)
        )
      )
    )
  )

  # No override: intervals are joined with "; " and the schema unit is kept.
  kept <- .extractSimulationTimeFromPkml(sim)
  expect_identical(kept$simulationTimeUnit, "min")
  expect_identical(kept$simulationTime, "0, 60, 2; 60, 120, 1")

  # Override to hours: the bounds are converted (60 min -> 1 h), the
  # resolution is left untouched, and the unit is the requested one.
  converted <- .extractSimulationTimeFromPkml(sim, targetUnit = "h")
  expect_identical(converted$simulationTimeUnit, "h")
  expect_identical(converted$simulationTime, "0, 1, 2; 1, 2, 1")
})

test_that(".extractSimulationTimeFromPkml returns NULLs when the schema has no intervals", {
  out <- .extractSimulationTimeFromPkml(list(
    outputSchema = list(intervals = list())
  ))
  expect_null(out$simulationTime)
  expect_null(out$simulationTimeUnit)
})

test_that(".generateOutputPathId builds readable ids from the last two segments", {
  # Generated ids are canonical (lowercase) so addOutputPath() does not warn.
  expect_identical(
    .generateOutputPathId(
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container",
      character()
    ),
    "aciclovir_concentration_in_container"
  )
  expect_identical(
    .generateOutputPathId(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      "aciclovir_plasma_peripheral_venous_blood"
    ),
    "aciclovir_plasma_peripheral_venous_blood_2"
  )
})

test_that(".resolveScenarioOutputPaths reuses, registers, and aborts per precedence", {
  project <- .fakeProject(
    outputPaths = list(known = "Organism|A|Conc")
  )

  # Reuse: a literal path already registered keeps its id.
  reuse <- .resolveScenarioOutputPaths("Organism|A|Conc", project, character())
  expect_identical(reuse$outputPathIds, "known")
  expect_length(reuse$newEntries, 0)

  # Register: a new unnamed path gets a generated id and a new entry.
  reg <- .resolveScenarioOutputPaths("Organism|B|Conc", project, character())
  expect_length(reg$newEntries, 1)
  expect_identical(reg$outputPathIds, names(reg$newEntries))

  # Abort: a user id colliding with an existing id on a different path.
  expect_error(
    .resolveScenarioOutputPaths(
      c(known = "Organism|Different|Path"),
      project,
      character()
    ),
    "already maps to a different path"
  )
})

test_that(".getScenarioCount returns an integer for scalar, vector, and empty input", {
  # Single pkml, all other args scalar -> exactly one scenario, as integer.
  single <- esqlabsR:::.getScenarioCount("model.pkml", individual = "indiv1")
  expect_type(single, "integer")
  expect_identical(single, 1L)

  # A vector argument of length > 1 sets the count, returned as integer.
  vectorized <- esqlabsR:::.getScenarioCount(
    "model.pkml",
    individual = c("a", "b", "c")
  )
  expect_type(vectorized, "integer")
  expect_identical(vectorized, 3L)

  # Empty pkmlFilePaths -> zero scenarios (a no-op), still integer-typed.
  empty <- esqlabsR:::.getScenarioCount(character())
  expect_type(empty, "integer")
  expect_identical(empty, 0L)
})
