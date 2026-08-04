# The bundled `TestProjectExcel/ProjectConfiguration.json` is a monolithic
# snapshot written by a previous esqlabsR version, so it exercises the
# previous-version upgrade path end to end.
.legacySnapshotFixture <- function() {
  test_path("data", "TestProjectExcel", "ProjectConfiguration.json")
}

test_that(".isLegacySnapshot recognizes a previous-version monolithic snapshot", {
  legacy <- jsonlite::fromJSON(
    .legacySnapshotFixture(),
    simplifyVector = FALSE
  )
  expect_true(.isLegacySnapshot(legacy))

  # A v6 snapshot carries a schemaVersion and no `projectConfiguration` mirror
  # key, so it must not be mistaken for a previous-version one.
  expect_false(.isLegacySnapshot(list(
    schemaVersion = "2.0",
    scenarios = list()
  )))
  expect_false(.isLegacySnapshot(list(scenarios = list())))

  # A schemaVersion always wins even when a mirror-looking key is present, and a
  # lone `projectConfiguration` (no workbook section) is not enough to misfire.
  expect_false(.isLegacySnapshot(list(
    schemaVersion = "2.0",
    projectConfiguration = list(),
    Scenarios = list()
  )))
  expect_false(.isLegacySnapshot(list(projectConfiguration = list())))
})

test_that(".legacySheetToDf reads cells by name and rejects a malformed sheet", {
  # Row keys in a different order than column_names still land in the right
  # columns (name-keyed, not positional).
  sheet <- list(
    column_names = list("A", "B"),
    rows = list(list(B = "2", A = "1"), list(A = "3", B = "4"))
  )
  df <- .legacySheetToDf(sheet)
  expect_equal(df$A, c(1, 3))
  expect_equal(df$B, c(2, 4))

  # A null cell round-trips as blank, not the string "NA".
  blankSheet <- list(
    column_names = list("A", "B"),
    rows = list(list(A = "x", B = NULL))
  )
  expect_equal(.legacySheetToDf(blankSheet)$B, "")

  # A value that is not a {column_names, rows} object aborts clearly.
  expect_error(.legacySheetToDf("oops"), "malformed")

  # An absent `rows` is a valid empty sheet; a malformed `rows` (a scalar, or a
  # non-list row record) aborts with the same clear message.
  expect_equal(
    nrow(.legacySheetToDf(list(column_names = list("A")))),
    0
  )
  expect_error(
    .legacySheetToDf(list(column_names = list("A"), rows = "oops")),
    "malformed"
  )
  expect_error(
    .legacySheetToDf(list(column_names = list("A"), rows = list("oops"))),
    "malformed"
  )
})

test_that("loadProject on a previous-version snapshot points at restoreProject", {
  # `loadProject()` is the name a migrating user reaches for first, so the
  # snapshot's absent `schemaVersion` must not be reported to them as an
  # unsupported version: the file is an older project, and `restoreProject()`
  # is what opens it.
  dir <- withr::local_tempdir()
  snapshot <- file.path(dir, "ProjectConfiguration.json")
  file.copy(.legacySnapshotFixture(), snapshot)

  # The message names the absolute path it was handed, which differs per
  # machine; only the shape of the message is under test. Anchored on the
  # filename rather than a leading `/`, so a Windows drive-prefixed path
  # (`C:/Users/...`) is redacted too.
  expect_snapshot(
    loadProject(snapshot),
    error = TRUE,
    transform = \(lines) {
      sub("'[^']*ProjectConfiguration\\.json'", "'<path>'", lines)
    }
  )
})

test_that("restoreProject upgrades a previous-version snapshot to a v6 tree", {
  dir <- withr::local_tempdir()

  expect_message(
    project <- restoreProject(.legacySnapshotFixture(), dir),
    "previous-version project snapshot"
  )

  # Counts match a faithful migration of the fixture through the Excel bridge:
  # the previous-version layout carries one protocol sheet per application and a
  # `PITaskName`-keyed parameter-identification layout, both of which the bridge
  # reifies (3 applications, 3 PI tasks).
  expect_s3_class(project, "Project")
  expect_length(project$definitions$scenarios, 8)
  expect_length(project$definitions$applications, 3)
  expect_length(project$definitions$parameterIdentification, 3)
  expect_length(project$definitions$individuals, 1)

  # The upgrade materializes a real tree project at `dir` that loads on its own.
  expect_true(file.exists(file.path(dir, "Project.json")))
  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_length(reloaded$definitions$scenarios, 8)
})

# #1213 item 26: a previous-version snapshot carries no project name, so an
# upgraded project had none and nothing could give it one. `restoreProject(name =)`
# names the project it produces, on this path and on the v6 one.
test_that("restoreProject names the project it upgrades", {
  dir <- withr::local_tempdir()

  project <- suppressMessages(
    restoreProject(.legacySnapshotFixture(), dir, name = "Aciclovir study")
  )

  expect_identical(project$info$name, "Aciclovir study")
  # Written into the project file, so it survives on its own.
  expect_identical(
    loadProject(file.path(dir, "Project.json"))$info$name,
    "Aciclovir study"
  )
  # And nothing is left unsaved by the naming.
  expect_true(project$status$tree_in_sync)
})

test_that("upgrading a snapshot with a configured data file emits no warning", {
  # The fixture configures a `dataFile` that no snapshot carries, so the bridge
  # would warn "data file not found"; the upgrade muffles that redundant
  # warning (it is covered by the upgrade's own observed-data notice), which
  # matters under testthat 3e where an uncaught warning fails the test.
  dir <- withr::local_tempdir()
  expect_no_warning(
    suppressMessages(restoreProject(.legacySnapshotFixture(), dir))
  )
})

# A snapshot is a single file: the models, data and population folders its
# definitions reference live beside it. Nothing travelled at all before, because
# the upgrade handed the bridge a scratch directory holding only the rebuilt
# workbooks, so every scenario validated as File Not Found (#1213).
test_that("restoreProject carries the asset folders beside the snapshot into the project", {
  dir <- withr::local_tempdir()
  suppressMessages(restoreProject(.legacySnapshotFixture(), dir))

  expect_true(file.exists(file.path(
    dir,
    "Models",
    "Simulations",
    "Aciclovir.pkml"
  )))
  expect_true(file.exists(file.path(
    dir,
    "Data",
    "TestProject_TimeValuesData.xlsx"
  )))
  # The population csv files the snapshot carries itself land where the upgraded
  # project resolves them.
  project <- loadProject(file.path(dir, "Project.json"))
  expect_true(file.exists(file.path(
    project$paths$populationsFolder,
    "TestPopulation.csv"
  )))
})

test_that("restoreProject names the asset folders a lone snapshot could not bring", {
  # The one case the report always applies to, and the one where it used to be
  # suppressed: nothing sits beside the snapshot, so the user has to be told why
  # the upgraded project cannot run.
  lone <- withr::local_tempdir()
  snapshot <- file.path(lone, "shared.json")
  file.copy(.legacySnapshotFixture(), snapshot)

  dir <- withr::local_tempdir()
  expect_message(
    suppressWarnings(restoreProject(snapshot, dir)),
    "missing from the upgraded project"
  )
  expect_false(dir.exists(file.path(dir, "Models", "Simulations")))
})

test_that("restoreProject refuses an out-of-project folder before writing anything", {
  # The value makes the finished project unopenable, and the upgrade has to
  # return a loaded project, so it is refused up front rather than after a
  # complete definitions tree is on disk (#1213).
  fixture <- jsonlite::fromJSON(
    .legacySnapshotFixture(),
    simplifyVector = FALSE
  )
  elsewhere <- withr::local_tempdir()
  pc <- fixture$projectConfiguration
  for (i in seq_along(pc$rows)) {
    if (identical(pc$rows[[i]][["Property"]], "modelFolder")) {
      pc$rows[[i]][["Value"]] <- elsewhere
    }
  }
  fixture$projectConfiguration <- pc

  snapshot <- file.path(withr::local_tempdir(), "absolute.json")
  jsonlite::write_json(fixture, snapshot, auto_unbox = TRUE, null = "null")

  dir <- withr::local_tempdir()
  expect_error(
    restoreProject(snapshot, dir),
    "resolves outside the project folder"
  )
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("materializer writes the population csvs under the recorded folder name", {
  # Same reason each workbook is written under its recorded filename: the folder
  # name is user-customizable, and a hardcoded one leaves the csv files where the
  # bridge does not look.
  fixture <- jsonlite::fromJSON(
    .legacySnapshotFixture(),
    simplifyVector = FALSE
  )
  pc <- fixture$projectConfiguration
  for (i in seq_along(pc$rows)) {
    if (identical(pc$rows[[i]][["Property"]], "populationsFolder")) {
      pc$rows[[i]][["Value"]] <- "MyPops"
    }
  }
  fixture$projectConfiguration <- pc

  scratch <- withr::local_tempdir()
  .materializeLegacySnapshot(fixture, scratch)
  expect_true(file.exists(file.path(
    scratch,
    "Configurations",
    "MyPops",
    "TestPopulation.csv"
  )))
})

test_that("materializer writes each workbook under its recorded filename", {
  # A snapshot that renamed a workbook (here Scenarios) must have the workbook
  # written under that custom name, since the copied projectConfiguration sheet
  # points the bridge at it; a hardcoded name would silently drop the section.
  fixture <- jsonlite::fromJSON(
    .legacySnapshotFixture(),
    simplifyVector = FALSE
  )
  pc <- fixture$projectConfiguration
  for (i in seq_along(pc$rows)) {
    if (identical(pc$rows[[i]][["Property"]], "scenariosFile")) {
      pc$rows[[i]][["Value"]] <- "MyScenarios.xlsx"
    }
  }
  fixture$projectConfiguration <- pc

  scratch <- withr::local_tempdir()
  .materializeLegacySnapshot(fixture, scratch)
  expect_true(file.exists(file.path(
    scratch,
    "Configurations",
    "MyScenarios.xlsx"
  )))
  expect_false(file.exists(file.path(
    scratch,
    "Configurations",
    "Scenarios.xlsx"
  )))
})

test_that(".restoreColumnTypes keeps a zero-padded id column as text", {
  df <- data.frame(
    id = c("01", "02", "010"),
    n = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  out <- .restoreColumnTypes(df)
  expect_type(out$id, "character")
  expect_equal(out$id, c("01", "02", "010"))
  # A genuine numeric column is still coerced.
  expect_type(out$n, "double")
})

test_that("restoreProject warns of a stale handle when it overwrites a tree", {
  # First restore creates a tree; the second, with overwrite, must warn that a
  # Project loaded from `dir` before is now stale, matching the v6 path.
  dir <- withr::local_tempdir()
  suppressMessages(restoreProject(.legacySnapshotFixture(), dir))
  expect_warning(
    suppressMessages(
      restoreProject(.legacySnapshotFixture(), dir, overwrite = TRUE)
    ),
    "still contain the old project"
  )
})

test_that("restoreProject still restores a v6 snapshot (regression)", {
  src <- withr::local_tempdir()
  initProject(src, type = "example", createExcel = FALSE)
  snapshot <- snapshotProject(
    loadProject(file.path(src, "Project.json")),
    dir = withr::local_tempdir(),
    name = "v6"
  )

  dir <- withr::local_tempdir()
  restored <- restoreProject(snapshot, dir)
  expect_s3_class(restored, "Project")
  expect_true(file.exists(file.path(dir, "Project.json")))
})
