test_that("`sourceAll()` sources all files in the directory", {
  withr::with_tempdir(
    code = {
      f1 <- file.create("f1.R")
      f2 <- file.create("f2.R")
      writeLines("var1 <- 1 + 1", "f1.R")
      writeLines("var2 <- paste0('a', 'b')", "f2.R")

      sourceAll(".")

      expect_true(exists("var1"))
      expect_true(exists("var2"))
    }
  )
})


test_that("`pathFromClipboard()` converts paths as expected", {
  # This will work only in interactive mode, i.e. with
  # `devtools::test_active_file()` or `devtools::test()`, but not during R CMD
  # Check on CRAN or AppVeyor where the system clipboard is not available

  skip_on_ci()
  skip_if_not(interactive())
  skip_if_not_installed("clipr")

  path <- "C:\\Users\\Documents"
  clipr::write_clip(path, allow_non_interactive = TRUE)

  expect_equal(
    pathFromClipboard(),
    "C:/Users/Documents"
  )
})

test_that(".resolveProjectPath resolves a legitimate path under the root", {
  root <- withr::local_tempdir()

  # A direct file, a not-yet-created nested file, and the root itself all
  # resolve to a path under the (lexically-absolute) root.
  absRoot <- as.character(fs::path_abs(root))
  expect_true(startsWith(.resolveProjectPath("model.pkml", root), absRoot))
  expect_true(startsWith(.resolveProjectPath("sub/pop.csv", root), absRoot))
  expect_identical(.resolveProjectPath(".", root), absRoot)
})

test_that(".resolveProjectPath rejects a path that escapes the root", {
  # The error names the offending field and value. The root is a random temp
  # dir, so match on the stable message parts rather than snapshotting a path
  # that changes every run.
  root <- withr::local_tempdir()

  expect_error(
    .resolveProjectPath("../../../../etc/passwd", root, "modelFile"),
    "resolves outside the project folder"
  )
  expect_error(
    .resolveProjectPath("../../../../etc/passwd", root, "modelFile"),
    "modelFile"
  )
  # A `..` that only climbs back inside the root is still contained.
  expect_error(.resolveProjectPath("sub/../model.pkml", root), NA)
  # A `..` that climbs above the root is rejected even after descending first.
  expect_error(
    .resolveProjectPath("sub/../../escape.csv", root),
    "outside the project"
  )
})

# The containment exemption is granted to a path naming an environment
# variable, on the grounds that the variable is expanded and its value is the
# user's own choice. `$PATH` is deliberately never expanded, so exempting it
# would let a literal token bypass the check: `$PATH/../../etc` would stay
# literal and still resolve two levels above the root.
test_that(".declaresEnvVarPath exempts only references that are expanded", {
  expect_true(.declaresEnvVarPath("${MYDATA}/Data"))
  expect_true(.declaresEnvVarPath("$MYDATA"))
  # A real variable alongside `$PATH` still expands, so it keeps the exemption.
  expect_true(.declaresEnvVarPath("${MYDATA}/$PATH"))

  expect_false(.declaresEnvVarPath("$PATH/../../etc"))
  expect_false(.declaresEnvVarPath("${PATH}/x"))
  expect_false(.declaresEnvVarPath("Data/"))
  expect_false(.declaresEnvVarPath("../Data"))

  # Not a usable value: never exempt (and never a `logical(0)` / `NA` condition).
  expect_false(.declaresEnvVarPath(NULL))
  expect_false(.declaresEnvVarPath(NA_character_))
  expect_false(.declaresEnvVarPath(c("a", "b")))
})

# A `$PATH`-only value is not expanded, so it must be contained like any other
# literal rather than slipping past on the exemption.
test_that("a $PATH-only working folder is still contained", {
  root <- withr::local_tempdir()
  expect_error(
    .resolveProjectPath("$PATH/../../etc", root, "dataFolder"),
    "resolves outside the project folder"
  )
})

# Stray cell formatting extends a sheet's used range past its last real row, so
# a workbook edited over time reports trailing rows that hold nothing. A parser
# that takes each row for a definition aborts on the first of them for having no
# id, which blocked one legacy project's import outright (#1191). Dropping them
# at the one place every sheet is read keeps every parser out of it.
test_that("readExcel drops rows that are blank in every column", {
  path <- file.path(withr::local_tempdir(), "sheet.xlsx")
  .writeExcel(
    list(
      S = data.frame(
        A = c("a1", NA, NA, "a2"),
        B = c("b1", NA, NA, "b2"),
        stringsAsFactors = FALSE
      )
    ),
    path
  )

  # readxl reports the blank rows; readExcel does not pass them on.
  expect_identical(nrow(readxl::read_excel(path, sheet = "S")), 4L)
  data <- readExcel(path, sheet = "S")
  expect_identical(nrow(data), 2L)
  expect_identical(data$A, c("a1", "a2"))
})

# Only an entirely blank row goes. A row with a value in any column is a record
# with gaps, which is the parser's business to accept or report, not this one's.
test_that("readExcel keeps a row that is blank in only some columns", {
  path <- file.path(withr::local_tempdir(), "sheet.xlsx")
  .writeExcel(
    list(
      S = data.frame(
        A = c("a1", NA),
        B = c("b1", "b2"),
        stringsAsFactors = FALSE
      )
    ),
    path
  )

  data <- readExcel(path, sheet = "S")
  expect_identical(nrow(data), 2L)
  expect_identical(data$B, c("b1", "b2"))
})

# A whitespace-only cell is as empty as an absent one: a row of them is still a
# blank row.
test_that("readExcel treats a whitespace-only row as blank", {
  path <- file.path(withr::local_tempdir(), "sheet.xlsx")
  .writeExcel(
    list(
      S = data.frame(
        A = c("a1", "   "),
        B = c("b1", ""),
        stringsAsFactors = FALSE
      )
    ),
    path
  )

  expect_identical(nrow(readExcel(path, sheet = "S")), 1L)
})

# A header-only sheet has no rows to drop and must survive as itself: the
# importer reads such a sheet as an empty parameter set.
test_that("readExcel leaves a header-only sheet alone", {
  path <- file.path(withr::local_tempdir(), "sheet.xlsx")
  .writeExcel(
    list(S = data.frame(A = character(0), B = character(0))),
    path
  )

  data <- readExcel(path, sheet = "S")
  expect_identical(nrow(data), 0L)
  expect_named(data, c("A", "B"))
})
