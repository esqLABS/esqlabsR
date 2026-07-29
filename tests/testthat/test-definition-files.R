# Tests for the scenarios definition-files format layer (R/definition-files.R):
# the per-scenario JSON tree loader, in-memory mutators reconciled to disk by
# saveProject(), lazy referential validation, and the derived single-file
# snapshot.

test_that("loadProject reads scenarios from the definitions/scenarios/ tree", {
  project <- testProject()

  # The fixture stores scenarios as definitions/scenarios/*.json, and
  # Project.json carries no inline scenarios array.
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  expect_true(dir.exists(dir))
  expect_setequal(
    list.files(dir, pattern = "\\.json$"),
    paste0(names(project$definitions$scenarios), ".json")
  )
  raw <- jsonlite::fromJSON(
    project$info$projectFilePath,
    simplifyVector = FALSE
  )
  expect_length(raw$scenarios, 0L)
})

test_that("saveProject() writes one definition file; a removal deletes it", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")

  addScenario(project, "added", modelFile = "Aciclovir.pkml")
  # In-memory only until saved.
  expect_false(file.exists(file.path(dir, "added.json")))
  saveProject(project)
  expect_true(file.exists(file.path(dir, "added.json")))

  # The file on disk reloads to the same scenario record.
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(
    reloaded$definitions$scenarios[["added"]]$modelFile,
    project$definitions$scenarios[["added"]]$modelFile
  )

  removeScenario(project, "added")
  saveProject(project)
  expect_false(file.exists(file.path(dir, "added.json")))
})

test_that("addScenario canonicalizes its id to a safe, lowercase form", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")

  # A mixed-case id with a forbidden character is canonicalized (with a
  # warning) rather than rejected; the canonical id names the key and, on save,
  # the file.
  expect_snapshot(addScenario(
    project,
    "My/Scenario",
    modelFile = "Aciclovir.pkml"
  ))
  expect_true("my_scenario" %in% names(project$definitions$scenarios))
  saveProject(project)
  expect_true(file.exists(file.path(dir, "my_scenario.json")))
  expect_false("My/Scenario" %in% names(project$definitions$scenarios))
})

test_that("saveProject() structurally fail-fasts and leaves disk unchanged", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  saveProject(project) # settle the tree
  before <- list.files(dir)

  # A scenario with no modelFile is structurally invalid. `.setSection()` no
  # longer serializes on write, so the in-memory mutation succeeds; the abort
  # happens at saveProject() (the serialize-in-memory-first guarantee), before
  # any file is written.
  scenarios <- c(
    .getSection(project, "scenarios"),
    list(bad = Scenario(scenarioName = "bad"))
  )
  .setSection(project, "scenarios", scenarios)
  expect_error(
    saveProject(project),
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
  scenarios <- .getSection(project, "scenarios")
  sc <- scenarios[["testscenario"]]
  sc$outputPaths <- c(sc$outputPaths, Ghost = NA_character_)
  scenarios[["testscenario"]] <- sc
  # Write-through accepts it (structurally valid); referential check is lazy.
  expect_no_error(.setSection(project, "scenarios", scenarios))

  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("Ghost", msgs)))
})

test_that("snapshotProject writes a self-contained single file with scenarios inlined", {
  project <- testProject()
  # snapshotProject writes a `.esqlabsR` file and returns its path.
  snap <- snapshotProject(project, dir = withr::local_tempdir())
  expect_identical(fs::path_ext(snap), "esqlabsR")

  raw <- jsonlite::fromJSON(snap, simplifyVector = FALSE)
  expect_length(raw$scenarios, length(project$definitions$scenarios))
  # The snapshot directory has no definitions/ tree; the inline array suffices.
  expect_false(dir.exists(file.path(dirname(snap), "definitions", "scenarios")))

  reloaded <- restoreProject(snap, withr::local_tempdir())
  expect_named(
    reloaded$definitions$scenarios,
    names(project$definitions$scenarios),
    ignore.order = TRUE
  )
})

test_that("snapshot -> restore -> snapshot is a fixed point", {
  project <- testProject()

  snap1 <- snapshotProject(project, dir = withr::local_tempdir())
  reloaded <- restoreProject(snap1, withr::local_tempdir())

  snap2 <- snapshotProject(reloaded, dir = withr::local_tempdir())

  expect_identical(
    jsonlite::fromJSON(snap1, simplifyVector = FALSE),
    jsonlite::fromJSON(snap2, simplifyVector = FALSE)
  )
})

test_that("snapshot -> restore -> snapshot is a fixed point for the plots trio", {
  # testProject()'s fixture has no dataCombined/plots/plotGrids, so the fixed
  # point above never exercises the three sections reshaped most by this
  # refactor. exampleProject() populates all three.
  project <- exampleProject()
  expect_gt(length(project$definitions$dataCombined), 0)
  expect_gt(length(project$definitions$plots), 0)
  expect_gt(length(project$definitions$plotGrids), 0)

  snap1 <- snapshotProject(project, dir = withr::local_tempdir())
  reloaded <- restoreProject(snap1, withr::local_tempdir())
  snap2 <- snapshotProject(reloaded, dir = withr::local_tempdir())

  json1 <- jsonlite::fromJSON(snap1, simplifyVector = FALSE)
  json2 <- jsonlite::fromJSON(snap2, simplifyVector = FALSE)
  expect_identical(json1, json2)
  # The three sections must actually be inlined, not silently dropped to NULL.
  expect_length(json1$dataCombined, length(project$definitions$dataCombined))
  expect_length(json1$plots, length(project$definitions$plots))
  expect_length(json1$plotGrids, length(project$definitions$plotGrids))
})

test_that("saveProject() on a restored project materializes the whole set", {
  snap <- snapshotProject(testProject(), dir = withr::local_tempdir())
  dir <- withr::local_tempdir()
  project <- restoreProject(snap, dir)
  before <- names(project$definitions$scenarios)
  scenariosDir <- file.path(dir, "definitions", "scenarios")

  # A restored project already has a full on-disk tree; an in-memory add plus a
  # save reconciles the whole set, so a reload sees every sibling.
  addScenario(project, "newlyadded", modelFile = "Aciclovir.pkml")
  expect_false(file.exists(file.path(scenariosDir, "newlyadded.json")))
  saveProject(project)
  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_named(
    reloaded$definitions$scenarios,
    c(before, "newlyadded"),
    ignore.order = TRUE
  )
})

test_that("a write-back plus saveProject() on a restored project preserves siblings", {
  snap <- snapshotProject(testProject(), dir = withr::local_tempdir())
  dir <- withr::local_tempdir()
  project <- restoreProject(snap, dir)
  before <- names(project$definitions$scenarios)

  existing <- before[[1]]
  setScenario(project, existing, simulationTimeUnit = "min")
  saveProject(project)

  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_named(reloaded$definitions$scenarios, before, ignore.order = TRUE)
})

test_that("a scenarioName that disagrees with its list key aborts saveProject()", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  saveProject(project)
  before <- list.files(dir)

  # Store an existing scenario under a different key without updating its
  # scenarioName. The structural backstop at save (the serialize-in-memory-first
  # guarantee) rejects the key/name disagreement before any file is written.
  scenarios <- .getSection(project, "scenarios")
  scenarios[["renamed"]] <- scenarios[["testscenario"]]
  .setSection(project, "scenarios", scenarios)
  expect_snapshot(
    saveProject(project),
    error = TRUE
  )
  expect_false(file.exists(file.path(dir, "renamed.json")))
  expect_setequal(list.files(dir), before)
})

test_that("a write-back under a non-canonical key aborts saveProject()", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  saveProject(project)
  before <- list.files(dir)

  # The section accessor is read-only and addScenario() canonicalizes ids, so a
  # non-canonical key can only reach the section through a raw `.setSection()`
  # write. The structural validator at save is the backstop: a non-canonical
  # key (mixed case or a forbidden character) aborts, pointing the user at
  # addScenario().
  scenarios <- .getSection(project, "scenarios")
  sc <- scenarios[["testscenario"]]
  sc$scenarioName <- "Renamed"
  scenarios[["Renamed"]] <- sc
  .setSection(project, "scenarios", scenarios)
  expect_snapshot(
    saveProject(project),
    error = TRUE
  )
  expect_false(file.exists(file.path(dir, "Renamed.json")))
  expect_setequal(list.files(dir), before)
})

test_that("a correctly-keyed rename round-trips through the tree", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")

  renameScenario(project, "testscenario", "renamed")
  saveProject(project)

  expect_true(file.exists(file.path(dir, "renamed.json")))
  expect_false(file.exists(file.path(dir, "testscenario.json")))
  # Reload warns about cross-references that still point at the old name
  # (lazy referential check); the rename itself round-trips.
  reloaded <- suppressWarnings(loadProject(project$info$projectFilePath))
  expect_true("renamed" %in% names(reloaded$definitions$scenarios))
  expect_false("testscenario" %in% names(reloaded$definitions$scenarios))
})

test_that("a scenario id with path separators is canonicalized, not rejected", {
  project <- testProject()
  parentDir <- project$info$projectDirPath
  scenariosDir <- file.path(parentDir, "definitions", "scenarios")

  # Forbidden path characters are replaced (with a warning); the canonical id
  # is a single safe path segment that cannot escape the scenarios directory.
  expect_snapshot(
    addScenario(project, "../escape", modelFile = "Aciclovir.pkml")
  )
  expect_snapshot(
    addScenario(project, "sub/evil", modelFile = "Aciclovir.pkml")
  )
  expect_true("_escape" %in% names(project$definitions$scenarios))
  expect_true("sub_evil" %in% names(project$definitions$scenarios))
  saveProject(project)
  # Nothing escaped the definitions/scenarios/ directory.
  expect_false(file.exists(file.path(parentDir, "escape.json")))
  expect_true(file.exists(file.path(scenariosDir, "_escape.json")))
  expect_true(file.exists(file.path(scenariosDir, "sub_evil.json")))
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

# saveProject() serializes the whole set in memory before writing any file, so
# a section carrying one serializer-hostile definition aborts before touching disk,
# leaving the tree exactly as it was.
test_that("saveProject() aborting on one scenario leaves the tree intact", {
  project <- testProject()
  saveProject(project)
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  before <- names(project$definitions$scenarios)
  beforeFiles <- list.files(dir)

  # Make one loaded scenario serializer-hostile in the backing store (bypassing
  # the authoring API), then save. The save must abort before writing any file.
  # `.setSection()` writes the raw section and marks the project modified.
  scenarios <- .getSection(project, "scenarios")
  hostile <- scenarios[[before[[1]]]]
  hostile$simulateSteadyState <- TRUE
  hostile$steadyStateTimeUnit <- NULL
  scenarios[[before[[1]]]] <- hostile
  .setSection(project, "scenarios", scenarios)

  expect_error(
    saveProject(project),
    "steadyStateTimeUnit"
  )
  # The on-disk tree is untouched; a reload still yields the original set.
  expect_setequal(list.files(dir), beforeFiles)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_named(reloaded$definitions$scenarios, before, ignore.order = TRUE)
})

# A save carrying one valid plus one serializer-hostile new definition must
# abort with neither landing on disk (the serialize-in-memory-first guarantee).
test_that("a multi-definition saveProject() aborting on one definition is atomic", {
  project <- testProject()
  saveProject(project)
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  beforeFiles <- list.files(dir)

  # Build a whole-section map: the existing scenarios plus one brand-new valid
  # scenario plus one new serializer-hostile scenario (steadyState, no unit).
  valid <- Scenario(modelFile = "Aciclovir.pkml", scenarioName = "fresh_ok")
  hostile <- Scenario(modelFile = "Aciclovir.pkml", scenarioName = "fresh_bad")
  hostile$simulateSteadyState <- TRUE
  hostile$steadyStateTimeUnit <- NULL

  newSection <- c(
    .getSection(project, "scenarios"),
    list(fresh_ok = valid, fresh_bad = hostile)
  )
  .setSection(project, "scenarios", newSection)
  expect_error(
    saveProject(project),
    "steadyStateTimeUnit"
  )

  # Neither new definition reached disk.
  expect_setequal(list.files(dir), beforeFiles)
})

# Two ids differing only in case canonicalize to the same lowercase id, so
# the case-insensitive-filesystem collision dissolves: the second add lands
# on the same id and is rejected as a duplicate.
test_that("an id differing only in case canonicalizes to an existing id", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
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
  # Only the one (lowercased) file is written on save.
  saveProject(project)
  expect_setequal(list.files(dir), c(before, "myscenario.json"))
})

# A non-ASCII scenario filename must round-trip; `list.files()`
# returns native-encoding paths that an un-normalized radix sort can reject.
# (The accented letter is not a forbidden character, only lowercased.)
test_that("a non-ASCII scenario name round-trips through the tree", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")

  addScenario(project, "scénario", modelFile = "Aciclovir.pkml")
  saveProject(project)
  expect_true(file.exists(file.path(dir, "scénario.json")))

  reloaded <- loadProject(project$info$projectFilePath)
  expect_true("scénario" %in% names(reloaded$definitions$scenarios))
  expect_identical(
    reloaded$definitions$scenarios[["scénario"]]$modelFile,
    project$definitions$scenarios[["scénario"]]$modelFile
  )
})

# A scenario file with no `name` field used to abort with an opaque base-R
# index error naming nothing; it must now abort naming the file.
test_that("a scenario file missing its name aborts naming the file", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  f <- file.path(
    dir,
    paste0(names(project$definitions$scenarios)[[1]], ".json")
  )

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  obj$name <- NULL
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(
    loadProject(project$info$projectFilePath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# A scenario file whose inner `name` disagrees with its filename used to load
# keyed by the inner name, so a canonicalized reference would dangle. It must
# now abort naming the file and the mismatch.
test_that("a scenario file whose name disagrees with its filename aborts", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  f <- file.path(
    dir,
    paste0(names(project$definitions$scenarios)[[1]], ".json")
  )

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  obj$name <- "differentname"
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(
    loadProject(project$info$projectFilePath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# observedData is keyed like every other section, except its id may be derived
# rather than stored. Both forms must still agree with the filename, or the next
# save files the declaration elsewhere and reconciles the loaded file away.
test_that("an observedData file whose id disagrees with its filename aborts", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  f <- file.path(dir, "mismatch.json")
  jsonlite::write_json(
    list(type = "pkml", file = "obs.pkml", id = "elsewhere"),
    f,
    auto_unbox = TRUE
  )

  expect_snapshot(
    loadProject(project$info$projectFilePath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# A hand-edited id of the wrong type would otherwise reach `basename()` and a
# `character(1)` vapply as a number, failing far from the file that caused it.
test_that("an observedData file whose id is not a string aborts", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  f <- file.path(dir, "obs.pkml.json")
  jsonlite::write_json(
    list(type = "pkml", file = "obs.pkml", id = 2024),
    f,
    auto_unbox = TRUE
  )

  expect_snapshot(
    loadProject(project$info$projectFilePath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

test_that("an observedData file whose declared id matches its filename loads", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  jsonlite::write_json(
    list(type = "pkml", file = "sub/obs.pkml", id = "named"),
    file.path(dir, "named.json"),
    auto_unbox = TRUE
  )

  reloaded <- loadProject(project$info$projectFilePath)
  ids <- vapply(
    reloaded$definitions$observedData,
    function(e) e[["id"]] %||% NA_character_,
    character(1)
  )
  expect_true("named" %in% ids)
})

# A hand-edited scenario file whose scalar field became an empty
# object (the standard jsonlite round-trip of `null`) must fail load with a
# message naming the scenario and field, not an opaque internal error.
test_that("a non-scalar scalar field fails load naming the scenario and field", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  f <- file.path(
    dir,
    paste0(names(project$definitions$scenarios)[[1]], ".json")
  )

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  # `"population": null` round-tripped the standard jsonlite way becomes {}.
  obj$population <- structure(list(), names = character(0))
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(loadProject(project$info$projectFilePath), error = TRUE)
})

# A mutation stays in memory and sets the dirty bit; projectStatus() reports the
# unsaved edit on the tree axis (and NA on the Excel axis, no side-car here).
# After saveProject() the file is on disk and the tree axis is in sync again.
test_that("a mutation is flagged by projectStatus() until saveProject()", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")

  addScenario(project, "fresh", modelFile = "Aciclovir.pkml")
  # In memory only, and flagged as an unsaved change.
  expect_false(file.exists(file.path(dir, "fresh.json")))
  status <- projectStatus(project, silent = TRUE)
  expect_false(status$tree_in_sync)
  expect_identical(status$excel_in_sync, NA)

  saveProject(project)
  expect_true(file.exists(file.path(dir, "fresh.json")))
  expect_true(projectStatus(project, silent = TRUE)$tree_in_sync)
})

test_that("a high-precision numeric value survives a definition write/reload round-trip", {
  project <- testProject()
  preciseValue <- 1.234567890123

  addInitialConditionEntry(
    project,
    "precise",
    path = "Organism|A|Concentration",
    value = preciseValue,
    unit = "mg/l"
  )
  saveProject(project)

  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(
    reloaded$definitions$initialConditions[["precise"]][[1]]$value,
    preciseValue
  )

  snap <- snapshotProject(reloaded, dir = withr::local_tempdir())
  snapshotReloaded <- restoreProject(snap, withr::local_tempdir())
  expect_identical(
    snapshotReloaded$definitions$initialConditions[["precise"]][[1]]$value,
    preciseValue
  )
})

# initialConditions tree kind ----

test_that("the initialConditions spec serializes and parses a set round-trip", {
  spec <- .definitionTreeSpec("initialConditions")
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
  spec <- .definitionTreeSpec("initialConditions")

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
  spec <- .definitionTreeSpec("initialConditions")
  rec <- list(id = "myset", initialConditions = list())
  attr(rec, ".definitionFile") <- "somewhere/otherset.json"

  expect_snapshot(spec$parse(list(rec), NULL), error = TRUE)
})

# observed-data id escape guard ----

# The observed-data id becomes a filename, and a programmatic `name` reaches it
# verbatim. A name carrying a path separator (or `..`) must be rejected so it
# cannot escape the observed-data definition directory.
test_that("a programmatic observedData name that escapes its directory aborts", {
  entries <- list(
    list(type = "programmatic", name = "../escape")
  )

  expect_snapshot(
    .serializeObservedDataSet(entries),
    error = TRUE
  )
})

# Stale-file policy: the full-tree reconciler owns the `<kind>/` directory ----

# A minimal on-disk tree project with one scenario, loaded without the
# cross-reference warning pass, so these tests exercise the definition-tree writers
# directly.
.stalePolicyProject <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir("stale_policy_", .local_envir = envir)
  initProject(dir, type = "example", createExcel = FALSE, overwrite = TRUE)
  Project$new(file.path(dir, "Project.json"))
}

test_that("a full-tree write removes a stale definition file", {
  # A full-tree write owns the `definitions/<kind>/` directory: any `.json`
  # file not in the freshly-written keep-set is stale and is deleted.
  project <- .stalePolicyProject()
  scenariosDir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "scenarios"
  )
  # Drop an orphan file that no in-memory scenario corresponds to. Written
  # after load, so the loader never parses it.
  orphan <- file.path(scenariosDir, "orphandefinition.json")
  writeLines("{}", orphan)
  expect_true(file.exists(orphan))

  .writeDefinitionTree(
    project$definitions$scenarios,
    "scenarios",
    project,
    project$info$projectDirPath
  )
  expect_false(file.exists(orphan))
})

test_that("saveProject() reconciles an orphan away (make-disk-look-like-memory)", {
  # Under explicit-save, saveProject() is the full-tree reconciler: it deletes
  # any `definitions/<kind>/` file with no in-memory definition, so disk mirrors
  # memory exactly after a save.
  project <- .stalePolicyProject()
  scenariosDir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "scenarios"
  )
  orphan <- file.path(scenariosDir, "orphandefinition.json")
  writeLines("{}", orphan)
  expect_true(file.exists(orphan))

  # An authoring edit sets the dirty bit; saveProject() then reconciles.
  addScenario(project, "keptadd", modelFile = "Aciclovir.pkml")
  saveProject(project)
  expect_false(file.exists(orphan))
  expect_true(file.exists(file.path(scenariosDir, "keptadd.json")))
})

test_that("a full-tree write aborts when a stale file cannot be removed", {
  # Simulate a delete failure by making the definition directory read-only, so
  # `file.remove()` on its contents returns FALSE. This relies on POSIX
  # directory-write permission gating removal, which is not portable to
  # Windows.
  skip_on_os("windows")
  # CI runners execute as root, and root ignores the `0500` permission this test
  # sets to force the removal to fail, so the expected warning/error never fires
  # and the snapshot diverges. Skip on CI; it still runs on POSIX dev machines.
  skip_on_ci()

  project <- .stalePolicyProject()
  scenariosDir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "scenarios"
  )
  orphan <- file.path(scenariosDir, "orphandefinition.json")
  writeLines("{}", orphan)

  Sys.chmod(scenariosDir, mode = "0500")
  # Restore write permission on exit so the temp tree can be cleaned up even if
  # the expectation fails.
  withr::defer(Sys.chmod(scenariosDir, mode = "0700"))

  expect_snapshot(
    error = TRUE,
    .writeDefinitionTree(
      project$definitions$scenarios,
      "scenarios",
      project,
      project$info$projectDirPath
    ),
    transform = .redactTmpPath
  )
})
