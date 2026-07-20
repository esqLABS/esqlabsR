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
