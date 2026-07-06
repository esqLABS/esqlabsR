# Tests for the entity-files write-through generalization (Phase 2b):
# every Project section, not only scenarios, is persisted as a per-entity
# tree under `definitions/<kind>/`. Mirrors test-entity-files.R but exercises
# the other eight sections. The scenarios slice keeps its own dedicated tests.

test_that("loadProject reads every section from its definitions/<kind>/ tree", {
  project <- exampleProject()
  defs <- file.path(project$projectDirPath, "definitions")

  # The example fixture materializes each non-empty section as a tree, and
  # Project.json carries no inline copy of it.
  raw <- jsonlite::fromJSON(project$jsonPath, simplifyVector = FALSE)

  expect_true(dir.exists(file.path(defs, "individuals")))
  expect_setequal(
    list.files(file.path(defs, "individuals"), pattern = "\\.json$"),
    paste0(names(project$individuals), ".json")
  )
  expect_length(raw$individuals, 0L)

  expect_true(dir.exists(file.path(defs, "parameter-sets")))
  expect_setequal(
    list.files(file.path(defs, "parameter-sets"), pattern = "\\.json$"),
    paste0(names(project$parameterSets), ".json")
  )
  expect_length(raw$parameterSets, 0L)

  expect_true(dir.exists(file.path(defs, "initial-conditions")))
  expect_setequal(
    list.files(file.path(defs, "initial-conditions"), pattern = "\\.json$"),
    paste0(names(project$initialConditions), ".json")
  )
  expect_length(raw$initialConditions, 0L)

  expect_true(dir.exists(file.path(defs, "output-paths")))
  expect_setequal(
    list.files(file.path(defs, "output-paths"), pattern = "\\.json$"),
    paste0(names(project$outputPaths), ".json")
  )
  expect_length(raw$outputPaths, 0L)
})

test_that("addIndividual writes one entity file; removeIndividual deletes it", {
  project <- exampleProject()
  dir <- file.path(project$projectDirPath, "definitions", "individuals")

  addIndividual(project, "newindiv", species = "Human", gender = "MALE")
  expect_true(file.exists(file.path(dir, "newindiv.json")))

  reloaded <- loadProject(project$jsonPath)
  expect_identical(
    reloaded$individuals[["newindiv"]]$species,
    project$individuals[["newindiv"]]$species
  )

  removeIndividual(project, "newindiv")
  expect_false(file.exists(file.path(dir, "newindiv.json")))
})

test_that("addPopulation / addApplication / addOutputPath write entity files", {
  project <- exampleProject()
  defs <- file.path(project$projectDirPath, "definitions")

  addPopulation(project, "newpop", species = "Human", numberOfIndividuals = 5)
  expect_true(file.exists(file.path(defs, "populations", "newpop.json")))

  addApplication(project, "newapp")
  expect_true(file.exists(file.path(defs, "applications", "newapp.json")))

  addOutputPath(project, "newpath", "Organism|Drug|Concentration")
  expect_true(file.exists(file.path(defs, "output-paths", "newpath.json")))

  reloaded <- loadProject(project$jsonPath)
  expect_true("newpop" %in% names(reloaded$populations))
  expect_true("newapp" %in% names(reloaded$applications))
  expect_identical(
    reloaded$outputPaths[["newpath"]],
    "Organism|Drug|Concentration"
  )
})

test_that("addParameterSet / addParameterEntry write the set's entity file", {
  project <- exampleProject()
  dir <- file.path(project$projectDirPath, "definitions", "parameter-sets")

  addParameterSet(project, "newset")
  expect_true(file.exists(file.path(dir, "newset.json")))

  addParameterEntry(project, "newset", "Organism", "Param", 1.5, "mg")
  reloaded <- loadProject(project$jsonPath)
  expect_length(reloaded$parameterSets[["newset"]], 1L)
  expect_identical(reloaded$parameterSets[["newset"]][[1]]$value, 1.5)
})

# A single mutation must rewrite only the changed entity's file. A full
# re-serialize of the whole section would rewrite every sibling file too,
# which is both the O(N^2) cost and a needless churn of unrelated git diffs.
# Hand-edit a sibling file on disk with content the canonical serializer would
# never emit, mutate a different entity, then confirm the sibling is untouched.
test_that("a single mutation rewrites only the changed entity's file", {
  project <- exampleProject()
  dir <- file.path(project$projectDirPath, "definitions", "parameter-sets")

  addParameterSet(project, "seta")
  addParameterSet(project, "setb")
  siblingFile <- file.path(dir, "seta.json")
  sentinel <- "NOT-CANONICAL-SENTINEL"
  writeLines(sentinel, siblingFile)

  # Mutate a different set; the sibling's hand-written content must survive.
  addParameterEntry(project, "setb", "Organism", "Param", 1.5, "mg")
  expect_identical(readLines(siblingFile), sentinel)
  expect_true(file.exists(file.path(dir, "setb.json")))
})

# Per-mutation write-through must be ~linear in the number of mutations, not
# quadratic. Adding N entities one at a time serializes only the one changed
# entity each time, so the cost of adding the Nth entity is independent of how
# many already exist. The old whole-section-serialize path made each add
# O(section size), i.e. O(N^2) for N adds (the panel measured ~132x at 10x N).
test_that("adding many parameter sets one at a time scales linearly", {
  skip_on_cran()

  timeFor <- function(n) {
    p <- exampleProject()
    system.time(
      for (i in seq_len(n)) {
        addParameterSet(p, paste0("set", i))
      }
    )[["elapsed"]]
  }

  t100 <- timeFor(100L)
  t1000 <- timeFor(1000L)

  # 10x the work in O(N) is ~10x the time; the old O(N^2) path was ~100x. A
  # generous ceiling keeps the test robust to machine noise while still failing
  # loudly on a quadratic regression.
  ratio <- t1000 / max(t100, 1e-3)
  expect_lt(ratio, 40)
})

test_that("addPITask writes a per-task entity file; removePITask deletes it", {
  project <- exampleProject()
  dir <- file.path(
    project$projectDirPath,
    "definitions",
    "parameter-identification"
  )

  param <- PIParameter(
    id = "p1",
    scenarios = "aciclovir_iv",
    path = "Aciclovir|Lipophilicity",
    minValue = -2,
    maxValue = 2,
    startValue = 0
  )
  mapping <- PIOutputMapping(
    id = "m1",
    scenarios = "aciclovir_iv",
    outputPath = "aciclovir_pvb",
    observedData = "obs"
  )
  addPITask(
    project,
    "newtask",
    scenarios = "aciclovir_iv",
    parameters = list(param),
    outputMappings = list(mapping)
  )
  expect_true(file.exists(file.path(dir, "newtask.json")))

  reloaded <- loadProject(project$jsonPath)
  expect_true("newtask" %in% names(reloaded$parameterIdentification))

  removePITask(project, "newtask")
  expect_false(file.exists(file.path(dir, "newtask.json")))
})

test_that("the three plots parts write to data-combined / plots / plot-grids", {
  project <- exampleProject()
  defs <- file.path(project$projectDirPath, "definitions")
  dcDir <- file.path(defs, "data-combined")
  plotsDir <- file.path(defs, "plots")
  gridsDir <- file.path(defs, "plot-grids")

  # Each part persists one file per entity, keyed by its rationalized id.
  addDataCombined(
    project,
    "newdc",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = "Organism|Drug"
    ))
  )
  expect_true(file.exists(file.path(dcDir, "newdc.json")))

  addPlot(project, "newplot", "newdc", "individual")
  expect_true(file.exists(file.path(plotsDir, "newplot.json")))

  addPlotGrid(project, "newgrid", plots = "newplot")
  expect_true(file.exists(file.path(gridsDir, "newgrid.json")))

  # A reload reads each of the three plots sections from its own folder.
  reloaded <- loadProject(project$jsonPath)
  expect_true("newdc" %in% names(reloaded$dataCombined))
  expect_true("newplot" %in% names(reloaded$plots))
  expect_true("newgrid" %in% names(reloaded$plotGrids))
})

test_that("removing a plot entity deletes only its file, leaving siblings", {
  project <- exampleProject()
  plotsDir <- file.path(project$projectDirPath, "definitions", "plots")
  dcDir <- file.path(project$projectDirPath, "definitions", "data-combined")

  addPlot(project, "p_extra", "aciclovir_individual", "individual")
  expect_true(file.exists(file.path(plotsDir, "p_extra.json")))
  # The pre-existing plot p1 and the dataCombined are untouched by this add.
  expect_true(file.exists(file.path(plotsDir, "p1.json")))
  expect_true(file.exists(file.path(dcDir, "aciclovir_individual.json")))

  removePlot(project, "p_extra")
  expect_false(file.exists(file.path(plotsDir, "p_extra.json")))
  expect_true(file.exists(file.path(plotsDir, "p1.json")))
})

test_that("addObservedData (config) writes an observed-data entity file", {
  project <- exampleProject()
  dir <- file.path(project$projectDirPath, "definitions", "observed-data")

  addObservedData(
    project,
    list(type = "pkml", file = "extra.pkml")
  )
  expect_true(file.exists(file.path(dir, "extra.pkml.json")))

  reloaded <- loadProject(project$jsonPath)
  files <- vapply(
    reloaded$observedData,
    function(e) basename(e[["file"]] %||% ""),
    character(1)
  )
  expect_true("extra.pkml" %in% files)
})

test_that("a section write-through is an in-memory no-op on a clone", {
  source <- exampleProject()
  defs <- file.path(source$projectDirPath, "definitions")
  before <- list.files(file.path(defs, "individuals"))

  clone <- source$clone()
  addIndividual(clone, "cloneonly", species = "Human", gender = "MALE")

  expect_true("cloneonly" %in% names(clone$individuals))
  expect_setequal(list.files(file.path(defs, "individuals")), before)
  reloadedSource <- loadProject(source$jsonPath)
  expect_false("cloneonly" %in% names(reloadedSource$individuals))
})

test_that("a snapshot-loaded project materializes the full section tree on first write", {
  snap <- saveSnapshot(exampleProject(), local_projectPath())
  # A snapshot has no definitions/ tree; sections come from the inline arrays.
  expect_false(dir.exists(file.path(
    dirname(snap),
    "definitions",
    "individuals"
  )))

  project <- loadProject(snap)
  before <- names(project$individuals)
  addIndividual(project, "freshindiv", species = "Human", gender = "MALE")

  reloaded <- loadProject(snap)
  expect_named(
    reloaded$individuals,
    c(before, "freshindiv"),
    ignore.order = TRUE
  )
})

# A `definitions/<kind>/` path that is a regular file (not a directory) is a
# corrupted tree, not an absent section. `dir.exists()` is FALSE for both, so
# without a guard the project would load as structurally-valid but empty. The
# load must abort naming the path.
test_that("a definitions/<kind>/ path that is a file aborts the load", {
  project <- exampleProject()
  jsonPath <- project$jsonPath
  kindDir <- file.path(project$projectDirPath, "definitions", "individuals")

  unlink(kindDir, recursive = TRUE)
  writeLines("not a directory", kindDir)

  expect_snapshot(
    loadProject(jsonPath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

test_that("a definitions/ root that is a file aborts the load", {
  project <- exampleProject()
  jsonPath <- project$jsonPath
  defs <- file.path(project$projectDirPath, "definitions")

  unlink(defs, recursive = TRUE)
  writeLines("not a directory", defs)

  expect_snapshot(
    loadProject(jsonPath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# A keyed entity file missing its id field used to abort with an opaque base-R
# `list[[NULL]] <- x` error that named nothing. It must now abort naming the
# file and the missing field.
test_that("a keyed file missing its id field aborts naming the file", {
  project <- exampleProject()
  jsonPath <- project$jsonPath
  dir <- file.path(project$projectDirPath, "definitions", "individuals")
  f <- list.files(dir, pattern = "\\.json$", full.names = TRUE)[[1]]

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  obj$individualId <- NULL
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(
    loadProject(jsonPath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# A keyed file whose inner id disagrees with its filename used to load keyed by
# the inner id, contradicting the "id is the filename" contract and breaking
# canonicalized references. It must now abort naming the file and the mismatch.
test_that("a keyed file whose inner id disagrees with its filename aborts", {
  project <- exampleProject()
  jsonPath <- project$jsonPath
  dir <- file.path(project$projectDirPath, "definitions", "output-paths")
  f <- list.files(dir, pattern = "\\.json$", full.names = TRUE)[[1]]

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  obj$id <- "different"
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(
    loadProject(jsonPath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# Two output-path files declaring the same inner id used to silently collapse
# to one on load (silent loss). The filename-stem check makes that impossible:
# the file whose stem disagrees with its inner id aborts.
test_that("two files with the same inner id cannot silently collapse", {
  project <- exampleProject()
  jsonPath <- project$jsonPath
  dir <- file.path(project$projectDirPath, "definitions", "output-paths")
  f <- list.files(dir, pattern = "\\.json$", full.names = TRUE)[[1]]

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  # A second file under a different name but carrying the first file's inner id.
  jsonlite::write_json(
    obj,
    file.path(dir, "duplicate.json"),
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  expect_snapshot(
    loadProject(jsonPath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

# The natural jsonlite hand-edit round-trip re-emits a JSON `null` as `{}`. A
# scalar field that became an empty object on a non-scenario kind used to slip
# through with no guard; it must now fail the load with a clear message.
test_that("an empty-object scalar field on a non-scenario kind aborts the load", {
  project <- exampleProject()
  jsonPath <- project$jsonPath
  dir <- file.path(project$projectDirPath, "definitions", "individuals")
  f <- list.files(dir, pattern = "\\.json$", full.names = TRUE)[[1]]

  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  # `"species": null` round-tripped the standard jsonlite way becomes {}.
  obj$species <- structure(list(), names = character(0))
  jsonlite::write_json(obj, f, auto_unbox = TRUE, null = "null", pretty = TRUE)

  expect_snapshot(
    loadProject(jsonPath),
    error = TRUE,
    transform = .redactTmpPath
  )
})

test_that("an inline-section snapshot loads identically to its tree source", {
  source <- exampleProject()
  snap <- saveSnapshot(source, local_projectPath())
  reloaded <- loadProject(snap)

  expect_named(
    reloaded$individuals,
    names(source$individuals),
    ignore.order = TRUE
  )
  expect_named(
    reloaded$parameterSets,
    names(source$parameterSets),
    ignore.order = TRUE
  )
  expect_named(
    reloaded$outputPaths,
    names(source$outputPaths),
    ignore.order = TRUE
  )
  expect_named(
    reloaded$applications,
    names(source$applications),
    ignore.order = TRUE
  )
})

# Plots split into three kinds ----
#
# The plots concern is three independent top-level keyed sections
# (`dataCombined`, `plots`, `plotGrids`), each its own `definitions/<kind>/`
# tree. These exercise loading each section from its folder, the lazy
# referential contract across the three files, and the snapshot fixed-point
# over the tree.

test_that("loadProject reads each plots section from its own folder", {
  project <- exampleProject()
  defs <- file.path(project$projectDirPath, "definitions")

  # The example fixture materializes each plots part as its own keyed tree.
  expect_setequal(
    list.files(file.path(defs, "data-combined"), pattern = "\\.json$"),
    paste0(names(project$dataCombined), ".json")
  )
  expect_setequal(
    list.files(file.path(defs, "plots"), pattern = "\\.json$"),
    paste0(names(project$plots), ".json")
  )
  expect_setequal(
    list.files(file.path(defs, "plot-grids"), pattern = "\\.json$"),
    paste0(names(project$plotGrids), ".json")
  )
  # Project.json carries no inline copy of the plots section.
  raw <- jsonlite::fromJSON(project$jsonPath, simplifyVector = FALSE)
  expect_null(raw$plots)
})

test_that("a dangling cross-file plot ref is a lazy error, not a write abort", {
  project <- exampleProject()
  plotsDir <- file.path(project$projectDirPath, "definitions", "plots")

  # A plot referencing a non-existent dataCombined: the write itself is
  # structural-only (the plot's own shape), so it does not abort. The dangling
  # ref is caught lazily by validateProject(), exactly as before the split.
  pc <- project$.getSection("plots")
  pc$p1$dataCombinedId <- "ghost_dc"
  project$.setSection("plots", pc)
  expect_true(file.exists(file.path(plotsDir, "p1.json")))

  reloaded <- loadProject(project$jsonPath)
  results <- suppressWarnings(validateProject(reloaded))
  msgs <- vapply(
    results$plots$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("ghost_dc", msgs)))
})

test_that("a grid pointing at a missing plot stays a lazy error after reload", {
  project <- exampleProject()
  gridsDir <- file.path(project$projectDirPath, "definitions", "plot-grids")

  # Repoint the grid at a plot id that does not exist; the grid write succeeds
  # (its own structure is valid), the dangling plotIds ref is lazy.
  pg <- project$.getSection("plotGrids")
  pg$individual_diagnostics$plotIds <- "ghost_plot"
  project$.setSection("plotGrids", pg)
  expect_true(file.exists(file.path(gridsDir, "individual_diagnostics.json")))

  reloaded <- loadProject(project$jsonPath)
  results <- suppressWarnings(validateProject(reloaded))
  msgs <- vapply(
    results$plots$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("ghost_plot", msgs)))
})

test_that("an emptied plots part clears its folder's files", {
  project <- exampleProject()
  gridsDir <- file.path(project$projectDirPath, "definitions", "plot-grids")
  expect_true(file.exists(file.path(gridsDir, "individual_diagnostics.json")))

  removePlotGrid(project, "individual_diagnostics")
  expect_false(file.exists(file.path(gridsDir, "individual_diagnostics.json")))

  reloaded <- loadProject(project$jsonPath)
  expect_length(reloaded$plotGrids %||% list(), 0L)
})

test_that("snapshot then load is a fixed point over the three plots folders", {
  source <- exampleProject()
  snap <- saveSnapshot(source, local_projectPath())
  reloaded <- loadSnapshot(snap, dirname(local_projectPath()))

  # Each plots section is identical (same ids) and the three folders
  # materialized on the loaded snapshot.
  defs <- file.path(reloaded$projectDirPath, "definitions")
  expect_setequal(
    names(reloaded$dataCombined),
    names(source$dataCombined)
  )
  expect_true(file.exists(file.path(
    defs,
    "data-combined",
    "aciclovir_individual.json"
  )))
  expect_true(file.exists(file.path(defs, "plots", "p1.json")))
  expect_true(file.exists(file.path(
    defs,
    "plot-grids",
    "individual_diagnostics.json"
  )))
  # The inlined plots JSON (all three top-level sections) is a fixed point
  # across the round-trip.
  reloadedJson <- esqlabsR:::.projectToJson(reloaded)
  sourceJson <- esqlabsR:::.projectToJson(source)
  expect_identical(reloadedJson$dataCombined, sourceJson$dataCombined)
  expect_identical(reloadedJson$plots, sourceJson$plots)
  expect_identical(reloadedJson$plotGrids, sourceJson$plotGrids)
})

# plots / PI serialize key-alignment guards ----

# The on-disk filename is the map key, but the record carries its own stored id.
# When they diverge the reload aborts (the load side compares the inner id to
# the filename), so the write must reject the mismatch up front rather than
# emit an unreadable file.
test_that("a plots entry whose stored id differs from its map key aborts", {
  entries <- list(p1 = list(plotId = "p2"))

  expect_snapshot(
    esqlabsR:::.serializePlotEntrySet(entries, "plotId", "plot"),
    error = TRUE
  )
})

test_that("a PI task whose $id differs from its map key aborts", {
  tasks <- list(task1 = list(id = "task2"))

  expect_snapshot(
    esqlabsR:::.serializePITaskSet(tasks),
    error = TRUE
  )
})
