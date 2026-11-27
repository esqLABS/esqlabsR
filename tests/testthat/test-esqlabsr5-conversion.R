# convertProjectToV6() runs only on throwaway copies of the fixtures: it moves
# the workbooks of the folder it is given.

# Every file and folder under `dir`, with the content hash of each file, so a
# test can say the folder is exactly as it was.
.folderState <- function(dir) {
  paths <- sort(list.files(
    dir,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE
  ))
  full <- file.path(dir, paths)
  isDir <- dir.exists(full)
  hashes <- rep("<dir>", length(paths))
  hashes[!isDir] <- unname(tools::md5sum(full[!isDir]))
  stats::setNames(hashes, paths)
}

# Refusals ----

test_that("convertProjectToV6() refuses a path that is not an esqlabsR 5 workbook", {
  projectDir <- localExcelProjectDir()
  before <- .folderState(projectDir)

  expect_snapshot(
    convertProjectToV6(file.path(projectDir, "Missing.xlsx")),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_snapshot(
    convertProjectToV6(file.path(projectDir, "Data")),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_snapshot(
    convertProjectToV6(
      file.path(projectDir, "Data", "esqlabs_dataImporter_configuration.xml")
    ),
    error = TRUE,
    transform = .redactTmpDir
  )

  # A data workbook has sheets but no property table.
  expect_snapshot(
    convertProjectToV6(
      file.path(projectDir, "Data", "TestProject_TimeValuesData.xlsx")
    ),
    error = TRUE,
    transform = .redactTmpDir
  )

  expect_identical(.folderState(projectDir), before)
})

test_that("convertProjectToV6() refuses a workbook that is already in the v6 layout", {
  project <- testProject()
  workbook <- exportProjectToExcel(project, silent = TRUE)
  file.remove(project$info$projectFilePath)
  unlink(
    file.path(project$info$projectDirPath, "definitions"),
    recursive = TRUE
  )
  before <- .folderState(project$info$projectDirPath)

  expect_snapshot(
    convertProjectToV6(workbook),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(project$info$projectDirPath), before)
})

test_that("convertProjectToV6() refuses a folder that holds a v6 project or an earlier conversion", {
  configPath <- function(projectDir) {
    file.path(projectDir, "ProjectConfiguration.xlsx")
  }

  projectDir <- localExcelProjectDir()
  writeLines("{}", file.path(projectDir, "Project.json"))
  dir.create(
    file.path(projectDir, "definitions", "scenarios"),
    recursive = TRUE
  )
  writeLines("{}", file.path(projectDir, "definitions", "scenarios", "a.json"))
  before <- .folderState(projectDir)
  expect_snapshot(
    convertProjectToV6(configPath(projectDir)),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)

  projectDir <- localExcelProjectDir()
  dir.create(file.path(projectDir, "esqlabsR5"))
  file.copy(configPath(projectDir), file.path(projectDir, "Project.xlsx"))
  before <- .folderState(projectDir)
  expect_snapshot(
    convertProjectToV6(configPath(projectDir)),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)
})
