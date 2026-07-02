# Tests for the scenarios entity-files format layer (R/entity-files.R):
# the per-scenario JSON tree loader, write-through mutators, lazy
# referential validation, and the derived single-file snapshot.

test_that("loadProject reads scenarios from the definitions/scenarios/ tree", {
  project <- testProject()

  # The fixture stores scenarios as definitions/scenarios/*.json, and
  # Project.json carries no inline scenarios array.
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  expect_true(dir.exists(dir))
  expect_setequal(
    list.files(dir, pattern = "\\.json$"),
    paste0(names(project$scenarios), ".json")
  )
  raw <- jsonlite::fromJSON(project$jsonPath, simplifyVector = FALSE)
  expect_length(raw$scenarios, 0L)
})

test_that("addScenario writes one entity file; removeScenario deletes it", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")

  addScenario(project, "added", modelFile = "Aciclovir.pkml")
  expect_true(file.exists(file.path(dir, "added.json")))

  # The file on disk reloads to the same scenario record.
  reloaded <- loadProject(project$jsonPath)
  expect_identical(
    reloaded$scenarios[["added"]]$modelFile,
    project$scenarios[["added"]]$modelFile
  )

  removeScenario(project, "added")
  expect_false(file.exists(file.path(dir, "added.json")))
})

test_that("addScenario canonicalizes its id to a safe, lowercase form", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")

  # A mixed-case id with a forbidden character is canonicalized (with a
  # warning) rather than rejected; the canonical id names the file and key.
  expect_snapshot(addScenario(
    project,
    "My/Scenario",
    modelFile = "Aciclovir.pkml"
  ))
  expect_true("my_scenario" %in% names(project$scenarios))
  expect_true(file.exists(file.path(dir, "my_scenario.json")))
  expect_false("My/Scenario" %in% names(project$scenarios))
})

test_that("write-through structurally fail-fasts and leaves disk unchanged", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  before <- list.files(dir)

  # A scenario with no modelFile is structurally invalid; the write-through
  # entry point (`.setSection()`, what the authoring functions call) must abort
  # and write no file.
  scenarios <- c(
    project$.getSection("scenarios"),
    list(bad = Scenario(scenarioName = "bad"))
  )
  expect_error(
    project$.setSection("scenarios", scenarios),
    "bad.*modelFile"
  )
  expect_false(file.exists(file.path(dir, "bad.json")))
  expect_setequal(list.files(dir), before)
})

test_that("an unknown outputPathId is a lazy referential finding, not a load error", {
  project <- testProject()
  # Point an existing scenario at an output-path id that does not exist. The
  # authoring functions check this reference eagerly, so the only way a dangling
  # ref reaches the project is a hand-edited file or a raw `.setSection()`
  # write; both leave the structurally-valid record in place for the lazy
  # referential check at validateProject().
  scenarios <- project$.getSection("scenarios")
  sc <- scenarios[["testscenario"]]
  sc$outputPaths <- c(sc$outputPaths, Ghost = NA_character_)
  scenarios[["testscenario"]] <- sc
  # Write-through accepts it (structurally valid); referential check is lazy.
  expect_no_error(project$.setSection("scenarios", scenarios))

  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("Ghost", msgs)))
})

test_that("saveSnapshot writes a self-contained single file with scenarios inlined", {
  project <- testProject()
  # saveSnapshot normalizes the output to a `.esqlabsR` file and returns it.
  snap <- saveSnapshot(project, local_projectPath())
  expect_identical(fs::path_ext(snap), "esqlabsR")

  raw <- jsonlite::fromJSON(snap, simplifyVector = FALSE)
  expect_length(raw$scenarios, length(project$scenarios))
  # The snapshot directory has no definitions/ tree; the inline array suffices.
  expect_false(dir.exists(file.path(dirname(snap), "definitions", "scenarios")))

  reloaded <- loadProject(snap)
  expect_named(
    reloaded$scenarios,
    names(project$scenarios),
    ignore.order = TRUE
  )
})

test_that("snapshot -> load -> snapshot is a fixed point", {
  project <- testProject()

  snap1 <- saveSnapshot(project, local_projectPath())
  reloaded <- loadProject(snap1)

  snap2 <- saveSnapshot(reloaded, local_projectPath())

  expect_identical(
    jsonlite::fromJSON(snap1, simplifyVector = FALSE),
    jsonlite::fromJSON(snap2, simplifyVector = FALSE)
  )
})

test_that("snapshot -> load -> snapshot is a fixed point for the plots trio", {
  # testProject()'s fixture has no dataCombined/plots/plotGrids, so the fixed
  # point above never exercises the three sections reshaped most by this
  # refactor. exampleProject() populates all three.
  project <- exampleProject()
  expect_gt(length(project$dataCombined), 0)
  expect_gt(length(project$plots), 0)
  expect_gt(length(project$plotGrids), 0)

  snap1 <- saveSnapshot(project, local_projectPath())
  reloaded <- loadProject(snap1)
  snap2 <- saveSnapshot(reloaded, local_projectPath())

  json1 <- jsonlite::fromJSON(snap1, simplifyVector = FALSE)
  json2 <- jsonlite::fromJSON(snap2, simplifyVector = FALSE)
  expect_identical(json1, json2)
  # The three sections must actually be inlined, not silently dropped to NULL.
  expect_length(json1$dataCombined, length(project$dataCombined))
  expect_length(json1$plots, length(project$plots))
  expect_length(json1$plotGrids, length(project$plotGrids))
})

test_that("bare mutation of a snapshot-loaded project preserves siblings on reload", {
  snap <- saveSnapshot(testProject(), local_projectPath())

  # A snapshot has no definitions/ tree; loading falls back to the inline array.
  expect_false(dir.exists(file.path(dirname(snap), "definitions", "scenarios")))
  project <- loadProject(snap)
  before <- names(project$scenarios)

  # The first write-through must materialize the whole set to the tree, not
  # just the one new scenario, or the inline siblings are lost on reload.
  addScenario(project, "newlyadded", modelFile = "Aciclovir.pkml")
  reloaded <- loadProject(snap)
  expect_named(
    reloaded$scenarios,
    c(before, "newlyadded"),
    ignore.order = TRUE
  )
})

test_that("write-back on a snapshot-loaded project preserves siblings on reload", {
  snap <- saveSnapshot(testProject(), local_projectPath())
  project <- loadProject(snap)
  before <- names(project$scenarios)

  existing <- before[[1]]
  setScenario(project, existing, simulationTimeUnit = "min")

  reloaded <- loadProject(snap)
  expect_named(reloaded$scenarios, before, ignore.order = TRUE)
})

test_that("mutating a clone leaves the source's on-disk tree untouched", {
  source <- testProject()
  sourceDir <- file.path(source$projectDirPath, "definitions", "scenarios")
  before <- list.files(sourceDir)

  clone <- source$clone()
  addScenario(clone, "cloneonly", modelFile = "Aciclovir.pkml")

  # The clone holds the new scenario in memory, but its write-through is a
  # no-op: it does not own the source's tree.
  expect_true("cloneonly" %in% names(clone$scenarios))
  expect_setequal(list.files(sourceDir), before)
  reloadedSource <- loadProject(source$jsonPath)
  expect_false("cloneonly" %in% names(reloadedSource$scenarios))
})

test_that("saveSnapshot persists a clone's in-memory edits to a single file", {
  clone <- testProject()$clone()
  addScenario(clone, "cloneonly", modelFile = "Aciclovir.pkml")

  newPath <- saveSnapshot(clone, local_projectPath())

  # The snapshot is a single self-contained file (no definitions/ tree), and
  # reloading it yields the clone's edits including the new scenario.
  expect_false(dir.exists(file.path(dirname(newPath), "definitions")))
  expect_true("cloneonly" %in% names(loadProject(newPath)$scenarios))
})

test_that("a scenarioName that disagrees with its list key aborts the write", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  before <- list.files(dir)

  # Store an existing scenario under a different key without updating its
  # scenarioName. The structural backstop in the write path (`.setSection()`)
  # rejects the key/name disagreement before any file is written.
  scenarios <- project$.getSection("scenarios")
  scenarios[["renamed"]] <- scenarios[["testscenario"]]
  expect_snapshot(
    project$.setSection("scenarios", scenarios),
    error = TRUE
  )
  expect_false(file.exists(file.path(dir, "renamed.json")))
  expect_setequal(list.files(dir), before)
})

test_that("a write-back under a non-canonical key aborts the write", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  before <- list.files(dir)

  # The section accessor is read-only and addScenario() canonicalizes ids, so a
  # non-canonical key can only reach the tree through a raw `.setSection()`
  # write. The structural validator is the backstop: a non-canonical key (mixed
  # case or a forbidden character) aborts, pointing the user at addScenario().
  scenarios <- project$.getSection("scenarios")
  sc <- scenarios[["testscenario"]]
  sc$scenarioName <- "Renamed"
  scenarios[["Renamed"]] <- sc
  expect_snapshot(
    project$.setSection("scenarios", scenarios),
    error = TRUE
  )
  expect_false(file.exists(file.path(dir, "Renamed.json")))
  expect_setequal(list.files(dir), before)
})

test_that("a correctly-keyed rename round-trips through the tree", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")

  renameScenario(project, "testscenario", "renamed")

  expect_true(file.exists(file.path(dir, "renamed.json")))
  expect_false(file.exists(file.path(dir, "testscenario.json")))
  # Reload warns about cross-references that still point at the old name
  # (lazy referential check); the rename itself round-trips.
  reloaded <- suppressWarnings(loadProject(project$jsonPath))
  expect_true("renamed" %in% names(reloaded$scenarios))
  expect_false("testscenario" %in% names(reloaded$scenarios))
})

test_that("a scenario id with path separators is canonicalized, not rejected", {
  project <- testProject()
  parentDir <- project$projectDirPath
  scenariosDir <- file.path(parentDir, "definitions", "scenarios")

  # Forbidden path characters are replaced (with a warning); the canonical id
  # is a single safe path segment that cannot escape the scenarios directory.
  expect_snapshot(
    addScenario(project, "../escape", modelFile = "Aciclovir.pkml")
  )
  expect_snapshot(
    addScenario(project, "sub/evil", modelFile = "Aciclovir.pkml")
  )
  expect_true("_escape" %in% names(project$scenarios))
  expect_true("sub_evil" %in% names(project$scenarios))
  # Nothing escaped the definitions/scenarios/ directory.
  expect_false(file.exists(file.path(parentDir, "escape.json")))
  expect_true(file.exists(file.path(scenariosDir, "_escape.json")))
  expect_true(file.exists(file.path(scenariosDir, "sub_evil.json")))
})

test_that("saveSnapshot refuses to overwrite the project's own container", {
  project <- testProject()
  containerBefore <- jsonlite::fromJSON(
    project$jsonPath,
    simplifyVector = FALSE
  )

  # No path defaults to the container; an explicit container path is the same.
  expect_snapshot(saveSnapshot(project), error = TRUE)
  expect_snapshot(saveSnapshot(project, project$jsonPath), error = TRUE)

  # The container is untouched (still scenarios: []) and the tree intact.
  containerAfter <- jsonlite::fromJSON(project$jsonPath, simplifyVector = FALSE)
  expect_length(containerAfter$scenarios, 0L)
  expect_identical(containerAfter, containerBefore)
  expect_true(dir.exists(file.path(
    project$projectDirPath,
    "definitions",
    "scenarios"
  )))
})

# A bulk tree write must be all-or-nothing, and structural
# validation must be a true superset of what the serializer requires (a
# scenario that passes `.validateScenarioStructure` must always serialize).
test_that("structural validation rejects a serializer-hostile scenario", {
  # simulateSteadyState = TRUE with no steadyStateTimeUnit is what
  # `.scenarioToJson` aborts on; the structural validator must catch it first.
  sc <- Scenario(
    scenarioName = "ss",
    modelFile = "m.pkml",
    simulateSteadyState = TRUE,
    steadyStateTimeUnit = NULL
  )
  expect_snapshot(.validateScenarioStructure(sc, "ss"), error = TRUE)

  # outputPaths present but unnamed is the other serializer abort.
  bad <- Scenario(scenarioName = "op", modelFile = "m.pkml")
  bad$outputPaths <- "Organism|Drug"
  expect_snapshot(.validateScenarioStructure(bad, "op"), error = TRUE)
})

test_that("a bulk write aborting on one scenario leaves the tree intact", {
  snap <- saveSnapshot(testProject(), local_projectPath())
  project <- loadProject(snap)
  before <- names(project$scenarios)

  # Make one already-loaded scenario serializer-hostile, in the backing store
  # so it bypasses write-through, then trigger a full materialize by adding a
  # new scenario. The materialize must abort before writing any file, so the
  # reload still yields the original full set (no partial tree).
  poke <- project$.__enclos_env__$private
  hostile <- poke$.scenarios[[before[[1]]]]
  hostile$simulateSteadyState <- TRUE
  hostile$steadyStateTimeUnit <- NULL
  poke$.scenarios[[before[[1]]]] <- hostile

  expect_error(
    addScenario(project, "BrandNew", modelFile = "Aciclovir.pkml"),
    "steadyStateTimeUnit"
  )
  expect_false(dir.exists(file.path(dirname(snap), "definitions", "scenarios")))
  reloaded <- loadProject(snap)
  expect_named(reloaded$scenarios, before, ignore.order = TRUE)
})

# The diff path (a project whose `definitions/scenarios/` already exists on
# disk) serializes every changed entity before writing any file, so a
# whole-section assignment carrying one valid plus one serializer-hostile
# entity must abort with neither landing on disk and memory unchanged. The
# materialize branch is covered above; this covers the common diff branch.
test_that("a multi-entity diff-path write aborting on one entity is atomic", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  beforeFiles <- list.files(dir)
  beforeMem <- project$scenarios

  # Build a whole-section map: one brand-new valid scenario plus one new
  # serializer-hostile scenario (steadyState without a unit). Assigning the
  # whole map exercises the diff path (both are "added" keys it must serialize).
  valid <- Scenario(modelFile = "Aciclovir.pkml", scenarioName = "fresh_ok")
  hostile <- Scenario(modelFile = "Aciclovir.pkml", scenarioName = "fresh_bad")
  hostile$simulateSteadyState <- TRUE
  hostile$steadyStateTimeUnit <- NULL

  newSection <- c(
    beforeMem,
    list(fresh_ok = valid, fresh_bad = hostile)
  )
  expect_error(
    project$.setSection("scenarios", newSection),
    "steadyStateTimeUnit"
  )

  # Neither new entity reached disk, and the in-memory section is unchanged.
  expect_setequal(list.files(dir), beforeFiles)
  expect_identical(project$scenarios, beforeMem)
})

# Two ids differing only in case canonicalize to the same lowercase id, so
# the case-insensitive-filesystem collision dissolves: the second add lands
# on the same id and is rejected as a duplicate.
test_that("an id differing only in case canonicalizes to an existing id", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  before <- list.files(dir)

  expect_snapshot(addScenario(
    project,
    "MyScenario",
    modelFile = "Aciclovir.pkml"
  ))
  # "myscenario" canonicalizes to the same id, which already exists.
  expect_snapshot(
    addScenario(project, "myscenario", modelFile = "Aciclovir.pkml"),
    error = TRUE
  )
  # Only the one (lowercased) file was written.
  expect_setequal(list.files(dir), c(before, "myscenario.json"))
})

# A non-ASCII scenario filename must round-trip; `list.files()`
# returns native-encoding paths that an un-normalized radix sort can reject.
# (The accented letter is not a forbidden character, only lowercased.)
test_that("a non-ASCII scenario name round-trips through the tree", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")

  addScenario(project, "scénario", modelFile = "Aciclovir.pkml")
  expect_true(file.exists(file.path(dir, "scénario.json")))

  reloaded <- loadProject(project$jsonPath)
  expect_true("scénario" %in% names(reloaded$scenarios))
  expect_identical(
    reloaded$scenarios[["scénario"]]$modelFile,
    project$scenarios[["scénario"]]$modelFile
  )
})

# A scenario file with no `name` field used to abort with an opaque base-R
# index error naming nothing; it must now abort naming the file.
test_that("a scenario file missing its name aborts naming the file", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  f <- file.path(dir, paste0(names(project$scenarios)[[1]], ".json"))

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  obj$name <- NULL
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(
    loadProject(project$jsonPath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# A scenario file whose inner `name` disagrees with its filename used to load
# keyed by the inner name, so a canonicalized reference would dangle. It must
# now abort naming the file and the mismatch.
test_that("a scenario file whose name disagrees with its filename aborts", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  f <- file.path(dir, paste0(names(project$scenarios)[[1]], ".json"))

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  obj$name <- "differentname"
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(
    loadProject(project$jsonPath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# A hand-edited scenario file whose scalar field became an empty
# object (the standard jsonlite round-trip of `null`) must fail load with a
# message naming the scenario and field, not an opaque internal error.
test_that("a non-scalar scalar field fails load naming the scenario and field", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")
  f <- file.path(dir, paste0(names(project$scenarios)[[1]], ".json"))

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  # `"population": null` round-tripped the standard jsonlite way becomes {}.
  obj$population <- structure(list(), names = character(0))
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(loadProject(project$jsonPath), error = TRUE)
})

# Every section is write-through, so after a mutation the entity files are
# already on disk and there is nothing for syncStatus() to flag (no Excel
# side-car here, so it reports NA).
test_that("a mutation lands on disk immediately; syncStatus has nothing to flag", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "scenarios")

  addScenario(project, "fresh", modelFile = "Aciclovir.pkml")
  # The scenario file is already on disk right after the mutation.
  expect_true(file.exists(file.path(dir, "fresh.json")))

  status <- project$syncStatus(silent = TRUE)
  expect_identical(status$excel_in_sync, NA)
})

# initialConditions tree kind ----

test_that("the initialConditions spec serializes and parses a set round-trip", {
  spec <- .entityTreeSpec("initialConditions")
  section <- list(
    myset = .asInitialConditionSet(list(
      list(path = "Organism|A|Concentration", value = 1.5, unit = "mg/l"),
      list(path = "Organism|B|Concentration", value = 0.5, unit = "µmol/l")
    ))
  )

  serialized <- spec$serialize(section, NULL)
  expect_named(serialized, "myset")
  expect_identical(serialized$myset$id, "myset")
  expect_length(serialized$myset$initialConditions, 2L)
  expect_identical(
    serialized$myset$initialConditions[[1]]$path,
    "Organism|A|Concentration"
  )

  # Feeding the serialized per-file records back through the parser (the load
  # path reads one `{id, initialConditions}` record per file) reproduces the
  # in-memory section.
  records <- unname(serialized)
  parsed <- spec$parse(records, NULL)
  expect_named(parsed, "myset")
  expect_s3_class(parsed$myset, "InitialConditionSet")
  expect_identical(unclass(parsed$myset), unclass(section$myset))
})

test_that("the initialConditions spec keeps the empty-vs-absent distinction", {
  spec <- .entityTreeSpec("initialConditions")

  # A genuinely absent section (NULL) stays a bare list(); a present empty
  # section ({}) becomes a named-empty list.
  expect_identical(spec$parse(spec$inline(list()), NULL), list())
  emptyPresent <- spec$parse(
    spec$inline(list(
      initialConditions = structure(list(), names = character(0))
    )),
    NULL
  )
  expect_length(emptyPresent, 0L)
  expect_identical(names(emptyPresent), character(0))
})

test_that("a tree-loaded initialConditions id must match its filename stem", {
  spec <- .entityTreeSpec("initialConditions")
  rec <- list(id = "myset", initialConditions = list())
  attr(rec, ".entityFile") <- "somewhere/otherset.json"

  expect_snapshot(spec$parse(list(rec), NULL), error = TRUE)
})
