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

test_that("loadProject on a previous-version snapshot points at importProjectFromExcel", {
  # `loadProject()` is the name a migrating user reaches for first, so the
  # snapshot's absent `schemaVersion` must not be reported to them as an
  # unsupported version: the file is an older project, and
  # `importProjectFromExcel()` is what upgrades it.
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

test_that("importProjectFromExcel upgrades a previous-version snapshot to a v6 tree", {
  dir <- withr::local_tempdir()

  shown <- capture_messages(
    result <- withVisible(
      importProjectFromExcel(.legacySnapshotFixture(), outputDir = dir)
    )
  )
  expect_match(shown, "previous-version project snapshot", all = FALSE)

  # The path of the written project file comes back invisibly, as for a
  # workbook import.
  expect_false(result$visible)
  expect_identical(result$value, file.path(dir, "Project.json"))
  expect_true(dir.exists(file.path(dir, "definitions", "scenarios")))

  # Counts match a faithful migration of the fixture through the Excel bridge:
  # the previous-version layout carries one protocol sheet per application and a
  # `PITaskName`-keyed parameter-identification layout, both of which the bridge
  # reifies (3 applications, 3 PI tasks).
  project <- loadProject(dir)
  expect_length(project$definitions$scenarios, 8)
  expect_length(project$definitions$applications, 3)
  expect_length(project$definitions$parameterIdentification, 3)
  expect_length(project$definitions$individuals, 1)
})

test_that("importProjectFromExcel warns about a snapshot's missing data file, also when silent", {
  # The fixture configures a `dataFile`; imported from a folder of its own, no
  # data workbook sits beside the snapshot. The import names it, as a workbook
  # import does, and `silent = TRUE` does not hide it, since it is a warning
  # about the result rather than part of the summary.
  lone <- withr::local_tempdir()
  snapshot <- file.path(lone, "ProjectConfiguration.json")
  file.copy(.legacySnapshotFixture(), snapshot)

  dir <- withr::local_tempdir()
  expect_snapshot(
    importProjectFromExcel(snapshot, outputDir = dir, silent = TRUE)
  )
  expect_warning(
    importProjectFromExcel(
      snapshot,
      outputDir = dir,
      overwrite = TRUE,
      silent = TRUE
    ),
    class = "esqlabsR_importSkippedObservedData"
  )
})

# A snapshot is a single file: the models, data and population folders its
# definitions reference live beside it, and have to travel into the folder the
# project is written to (#1213).
test_that("importProjectFromExcel carries the asset folders beside the snapshot into the project", {
  dir <- withr::local_tempdir()
  # The data workbook sits in the data folder beside the fixture, so its
  # observed data is imported and nothing warns about it.
  expect_no_warning(suppressMessages(importProjectFromExcel(
    .legacySnapshotFixture(),
    outputDir = dir
  )))
  expect_length(
    list.files(file.path(dir, "definitions", "observed-data")),
    1L
  )

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
  project <- loadProject(dir)
  expect_true(file.exists(file.path(
    project$paths$populationsFolder,
    "TestPopulation.csv"
  )))
})

test_that("importProjectFromExcel names the asset folders a lone snapshot could not bring", {
  # Nothing sits beside the snapshot, so the user has to be told why the
  # upgraded project cannot run.
  lone <- withr::local_tempdir()
  snapshot <- file.path(lone, "shared.json")
  file.copy(.legacySnapshotFixture(), snapshot)

  dir <- withr::local_tempdir()
  expect_snapshot(
    importProjectFromExcel(snapshot, outputDir = dir),
    transform = .redactTmpDir
  )
  expect_false(dir.exists(file.path(dir, "Models", "Simulations")))
})

test_that("importProjectFromExcel refuses an out-of-project folder in a snapshot before writing anything", {
  # The value makes the finished project unopenable, so it is refused up front
  # rather than after a complete definitions tree is on disk (#1213).
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
    importProjectFromExcel(snapshot, outputDir = dir),
    "resolves outside the project folder"
  )
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("importProjectFromExcel writes the upgraded project beside the snapshot by default", {
  # The usual case: the snapshot still sits in the 5.x project folder, beside
  # the models, data and configuration workbooks. The project is written there,
  # and none of the files already in the folder is touched.
  src <- withr::local_tempdir()
  file.copy(
    list.files(test_path("data", "TestProjectExcel"), full.names = TRUE),
    src,
    recursive = TRUE
  )
  snapshot <- file.path(src, "ProjectConfiguration.json")
  before <- list.files(src, recursive = TRUE, all.files = TRUE)
  checksumsBefore <- tools::md5sum(file.path(src, before))

  expect_no_warning(
    path <- suppressMessages(importProjectFromExcel(snapshot))
  )

  expect_identical(
    normalizePath(path),
    normalizePath(file.path(src, "Project.json"))
  )
  # Nothing that was there before changed; the import only added the project
  # file and its definitions.
  expect_identical(tools::md5sum(file.path(src, before)), checksumsBefore)
  added <- setdiff(list.files(src, recursive = TRUE, all.files = TRUE), before)
  expect_true(all(added == "Project.json" | startsWith(added, "definitions/")))

  project <- loadProject(src)
  expect_length(project$definitions$scenarios, 8)
  expect_true(file.exists(file.path(
    project$paths$populationsFolder,
    "TestPopulation.csv"
  )))

  # Importing again with `overwrite = TRUE` replaces the project, not the
  # folders it was made from: the population csv files rebuilt from the
  # snapshot would otherwise replace the originals beside it.
  suppressMessages(importProjectFromExcel(snapshot, overwrite = TRUE))
  expect_identical(tools::md5sum(file.path(src, before)), checksumsBefore)
})

test_that("importProjectFromExcel refuses to import a snapshot over an existing project unless overwrite", {
  dir <- withr::local_tempdir()
  suppressMessages(importProjectFromExcel(
    .legacySnapshotFixture(),
    outputDir = dir
  ))

  expect_snapshot(
    importProjectFromExcel(.legacySnapshotFixture(), outputDir = dir),
    error = TRUE,
    transform = .redactTmpDir
  )

  expect_no_error(suppressMessages(importProjectFromExcel(
    .legacySnapshotFixture(),
    outputDir = dir,
    overwrite = TRUE
  )))
  expect_length(loadProject(dir)$definitions$scenarios, 8)
})

test_that("importProjectFromExcel upgrades a snapshot silently with silent = TRUE", {
  # Every folder and the data workbook sit beside the fixture, so there is
  # nothing to warn about either.
  dir <- withr::local_tempdir()

  expect_silent(importProjectFromExcel(
    .legacySnapshotFixture(),
    outputDir = dir,
    silent = TRUE
  ))
  expect_true(file.exists(file.path(dir, "Project.json")))
})

test_that("importProjectFromExcel copies no folder for a snapshot with copyAssets = FALSE", {
  dir <- withr::local_tempdir()
  suppressMessages(importProjectFromExcel(
    .legacySnapshotFixture(),
    outputDir = dir,
    copyAssets = FALSE
  ))

  expect_setequal(
    list.files(dir, all.files = TRUE, no.. = TRUE),
    c("Project.json", "definitions")
  )
  # The definitions are the same as with the folders copied: the observed data
  # is still read from the data workbook beside the snapshot, as a workbook
  # import reads it from beside `Project.xlsx`.
  expect_true(dir.exists(file.path(dir, "definitions", "observed-data")))
  expect_length(
    list.files(file.path(dir, "definitions", "observed-data")),
    1L
  )
})

test_that("importProjectFromExcel honors projectFileName for a snapshot", {
  dir <- withr::local_tempdir()
  path <- suppressMessages(importProjectFromExcel(
    .legacySnapshotFixture(),
    outputDir = dir,
    projectFileName = "AciclovirStudy"
  ))

  expect_identical(path, file.path(dir, "AciclovirStudy.json"))
  expect_false(file.exists(file.path(dir, "Project.json")))
  expect_length(loadProject(path)$definitions$scenarios, 8)
})

test_that("importProjectFromExcel refuses to write the project over the snapshot it imports", {
  # Naming the project file after the snapshot, in the snapshot's own folder,
  # would replace the only copy of the 5.x configuration with the new project.
  # `overwrite = TRUE` is about an earlier project, not about the source.
  src <- withr::local_tempdir()
  snapshot <- file.path(src, "ProjectConfiguration.json")
  file.copy(.legacySnapshotFixture(), snapshot)
  checksumBefore <- tools::md5sum(snapshot)

  expect_snapshot(
    importProjectFromExcel(
      snapshot,
      projectFileName = "ProjectConfiguration.json",
      overwrite = TRUE
    ),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(
    list.files(src, all.files = TRUE, no.. = TRUE),
    basename(snapshot)
  )
  expect_identical(tools::md5sum(snapshot), checksumBefore)
})

test_that("importProjectFromExcel points a current project file at loadProject", {
  project <- testProject()

  expect_snapshot(
    importProjectFromExcel(
      project$info$projectFilePath,
      outputDir = withr::local_tempdir()
    ),
    error = TRUE,
    transform = .redactTmpDir
  )
})

test_that("importProjectFromExcel rejects a JSON file that is no project configuration", {
  dir <- withr::local_tempdir()
  other <- file.path(dir, "settings.json")
  jsonlite::write_json(list(solver = "CVODE", tolerance = 1e-6), other)

  expect_snapshot(
    importProjectFromExcel(other, outputDir = withr::local_tempdir()),
    error = TRUE,
    transform = .redactTmpDir
  )
})

test_that("importProjectFromExcel names a JSON file it cannot read", {
  dir <- withr::local_tempdir()
  broken <- file.path(dir, "ProjectConfiguration.json")
  writeLines("{\"projectConfiguration\": {\"column_names\": [", broken)

  # Only the esqlabsR message is under test; the parser's own text chained onto
  # it belongs to jsonlite.
  error <- expect_error(
    importProjectFromExcel(broken, outputDir = withr::local_tempdir())
  )
  expect_snapshot(
    cat(rlang::cnd_message(error, inherit = FALSE), "\n"),
    transform = .redactTmpDir
  )
  expect_s3_class(error$parent, "error")
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

