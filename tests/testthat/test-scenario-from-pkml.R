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
      scenarioNames = "Test1",
      modelParameterSets = "Global",
      paramSheets = "Aciclovir"
    )),
    class = "lifecycle_warning_deprecated"
  )
})

# createScenariosFromPKML: in-place mutation ----

test_that("createScenariosFromPKML adds scenarios in place, marks the project modified, and returns it invisibly", {
  project <- testProject()
  expect_false(project$modified)

  expect_snapshot(
    result <- createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarioNames = "Seeded"
    )
  )

  expect_identical(result, project)
  expect_true("Seeded" %in% names(project$scenarios))
  expect_s3_class(project$scenarios[["Seeded"]], "Scenario")
  expect_true(project$modified)
})

# createScenariosFromPKML: output path resolution ----

test_that("PKML-extracted output paths reuse existing project ids for known literal paths", {
  project <- testProject()
  idsBefore <- names(project$outputPaths)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded"
  ))

  sc <- project$scenarios[["Seeded"]]
  # The PVB path is already registered as `Aciclovir_PVB`, so its id is reused.
  expect_true("Aciclovir_PVB" %in% names(sc$outputPaths))
  expect_true(all(names(sc$outputPaths) %in% names(project$outputPaths)))
})

test_that("PKML-extracted output paths register generated readable ids when unknown", {
  project <- .fakeProject(
    modelParameterSets = list(Global = list())
  )
  project$modelFolder <- dirname(pkmlFixture)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded"
  ))

  sc <- project$scenarios[["Seeded"]]
  expect_gt(length(sc$outputPaths), 0)
  # Every id used by the scenario must be registered on the project.
  expect_true(all(names(sc$outputPaths) %in% names(project$outputPaths)))
  # Generated ids are readable (built from the path's last two segments).
  expect_match(names(sc$outputPaths), "^Aciclovir_", all = TRUE)
})

test_that("user-supplied named outputPaths register under the user ids", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded",
    outputPaths = c(
      plasma = "Organism|VenousBlood|Plasma|Aciclovir|Concentration"
    )
  ))

  sc <- project$scenarios[["Seeded"]]
  expect_named(sc$outputPaths, "plasma")
  expect_identical(
    project$outputPaths[["plasma"]],
    "Organism|VenousBlood|Plasma|Aciclovir|Concentration"
  )
})

test_that("user-supplied outputPaths reuse the existing id when the literal path already exists", {
  project <- testProject()
  existingPath <- project$outputPaths[["Aciclovir_PVB"]]
  idsBefore <- names(project$outputPaths)

  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded",
    # User invents a different id for an already-registered path.
    outputPaths = stats::setNames(existingPath, "myAlias")
  ))

  sc <- project$scenarios[["Seeded"]]
  # The registered id wins; the user alias is dropped, no new entry added.
  expect_named(sc$outputPaths, "Aciclovir_PVB")
  expect_identical(names(project$outputPaths), idsBefore)
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
      scenarioNames = "Seeded",
      outputPaths = c(Aciclovir_PVB = "Organism|Some|Other|Path")
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
    scenarioNames = "Seeded",
    outputPaths = "Organism|A|Conc, Organism|B|Conc"
  ))

  sc <- project$scenarios[["Seeded"]]
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
    scenarioNames = c("A", "B"),
    outputPaths = list(
      c(p1 = "Organism|A|Conc"),
      c(p2 = "Organism|B|Conc")
    )
  ))

  expect_named(project$scenarios[["A"]]$outputPaths, "p1")
  expect_named(project$scenarios[["B"]]$outputPaths, "p2")
})

# createScenariosFromPKML: model parameter sets ----

test_that("comma-separated modelParameterSets are split and FK-validated", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded",
    modelParameterSets = "Global, Aciclovir"
  ))
  expect_identical(
    project$scenarios[["Seeded"]]$modelParameterSets,
    c("Global", "Aciclovir")
  )
})

test_that("unknown modelParameterSets abort and leave the project unchanged", {
  project <- testProject()
  scenariosBefore <- names(project$scenarios)
  expect_snapshot(
    error = TRUE,
    createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarioNames = "Seeded",
      modelParameterSets = "DoesNotExist"
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
    scenarioNames = "Seeded"
  ))
  expect_true(is.na(project$scenarios[["Seeded"]]$applicationProtocol))
  expect_false(isAnyCriticalErrors(validateProject(project)))
})

test_that("user applicationProtocols are taken verbatim without Excel sanitization", {
  project <- testProject()
  longProtocol <- "Aciclovir_iv_250mg"
  expect_no_warning(suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded",
    applicationProtocols = longProtocol
  )))
  expect_identical(
    project$scenarios[["Seeded"]]$applicationProtocol,
    longProtocol
  )
})

test_that("unknown applicationProtocols abort", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarioNames = "Seeded",
      applicationProtocols = "NoSuchProtocol"
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
      scenarioNames = c("S", "S")
    )
  )
  expect_true(all(c("S", "S_2") %in% names(project$scenarios)))
})

test_that("duplicate-name expansion respects explicit later names", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)
  suppressWarnings(suppressMessages(createScenariosFromPKML(
    rep(pkmlFixture, 3),
    project = project,
    scenarioNames = c("S", "S", "S_2")
  )))
  expect_true(all(c("S", "S_2", "S_2_2") %in% names(project$scenarios)))
})

test_that("scenario names colliding with pre-existing project scenarios are suffixed", {
  project <- testProject()
  suppressWarnings(suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "TestScenario"
  )))
  expect_true("TestScenario_2" %in% names(project$scenarios))
})

test_that("scenarioNames = NULL derives names from the simulation and dedupes a recycled PKML", {
  project <- .fakeProject()
  project$modelFolder <- dirname(pkmlFixture)
  suppressWarnings(suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    individualId = NULL,
    populationId = NULL,
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
    scenarioNames = "Seeded"
  ))
  sc <- project$scenarios[["Seeded"]]
  expect_false(is.null(sc$simulationTime))
  expect_false(is.null(sc$simulationTimeUnit))
})

test_that("user simulationTime overrides PKML extraction", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded",
    simulationTime = "0, 24, 60",
    simulationTimeUnit = "h"
  ))
  sc <- project$scenarios[["Seeded"]]
  expect_identical(sc$simulationTimeUnit, "h")
  expect_identical(sc$simulationTime, list(c(0, 24, 60)))
})

# createScenariosFromPKML: steady state ----

test_that("steadyState = TRUE seeds a base-unit steadyStateTime and a unit", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "SS",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  ))
  sc <- project$scenarios[["SS"]]
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
    scenarioNames = "SS",
    steadyState = TRUE
  ))
  sc <- project$scenarios[["SS"]]
  expect_equal(sc$steadyStateTime, 1000)
  expect_identical(sc$steadyStateTimeUnit, "min")
})

test_that("seeded scenarios match addScenario-created scenarios field for field", {
  fromPkml <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = fromPkml,
    scenarioNames = "Seeded",
    outputPaths = c(Aciclovir_PVB = fromPkml$outputPaths[["Aciclovir_PVB"]]),
    simulationTime = "0, 24, 60",
    simulationTimeUnit = "h"
  ))

  viaAdd <- testProject()
  addScenario(
    viaAdd,
    scenarioName = "Seeded",
    modelFile = "Aciclovir.pkml",
    outputPathIds = "Aciclovir_PVB",
    simulationTime = "0, 24, 60",
    simulationTimeUnit = "h"
  )

  expect_equal(
    fromPkml$scenarios[["Seeded"]],
    viaAdd$scenarios[["Seeded"]]
  )
})

# createScenariosFromPKML: model file resolution ----

test_that("modelFile is stored relative to project$modelFolder as a plain character", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded"
  ))
  sc <- project$scenarios[["Seeded"]]
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
      scenarioNames = "Seeded"
    ))
  )
  sc <- project$scenarios[["Seeded"]]
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
      scenarioNames = c("A", "B", "C")
    )
  )
})

test_that("a failing addScenario rolls back scenarios and outputPaths", {
  project <- .fakeProject(
    modelParameterSets = list(Global = list())
  )
  project$modelFolder <- dirname(pkmlFixture)
  scenariosBefore <- names(project$scenarios)
  outputsBefore <- names(project$outputPaths)
  modifiedBefore <- project$modified

  expect_error(
    suppressMessages(createScenariosFromPKML(
      rep(pkmlFixture, 2),
      project = project,
      scenarioNames = c("Ok", "Bad"),
      modelParameterSets = c("Global", "DoesNotExist")
    ))
  )

  expect_identical(names(project$scenarios), scenariosBefore)
  expect_identical(names(project$outputPaths), outputsBefore)
  expect_identical(project$modified, modifiedBefore)
})

# createScenariosFromPKML: end-to-end round trips ----

test_that("end-to-end: seed from PKML, saveProject, loadProject round-trips", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Seeded"
  ))

  dir <- withr::local_tempdir()
  path <- file.path(dir, "Project.json")
  saveProject(project, path)
  reloaded <- loadProject(path)

  expect_equal(
    reloaded$scenarios[["Seeded"]],
    project$scenarios[["Seeded"]]
  )
  expect_equal(reloaded$outputPaths, project$outputPaths)
})

test_that("end-to-end: steadyState = TRUE round-trips through save and load", {
  project <- testProject()
  suppressMessages(createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "SS",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  ))

  dir <- withr::local_tempdir()
  path <- file.path(dir, "Project.json")
  saveProject(project, path)
  reloaded <- loadProject(path)

  expect_equal(reloaded$scenarios[["SS"]], project$scenarios[["SS"]])
})

# Internal helpers ----

test_that(".dedupeScenarioNames probes suffixes against project and call names", {
  out <- suppressWarnings(.dedupeScenarioNames(c("S", "S", "S_2"), character()))
  expect_identical(out, c("S", "S_2", "S_2_2"))

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

test_that(".generateOutputPathId builds readable ids from the last two segments", {
  expect_identical(
    .generateOutputPathId(
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container",
      character()
    ),
    "Aciclovir_Concentration_in_container"
  )
  expect_identical(
    .generateOutputPathId(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      "Aciclovir_Plasma_Peripheral_Venous_Blood"
    ),
    "Aciclovir_Plasma_Peripheral_Venous_Blood_2"
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
