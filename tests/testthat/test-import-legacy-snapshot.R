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
