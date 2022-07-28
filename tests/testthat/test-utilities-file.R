##  context("sourceAll")

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

##  context("pathFromClipboard")

test_that("`pathFromClipboard()` converts paths as expected", {
  # This will work only in interactive mode, i.e. with
  # `devtools::test_active_file()` or `devtools::test()`, but not during R CMD
  # Check on CRAN or AppVeyor where the system clipboard is not available
  if (clipr::clipr_available()) {
    path <- "C:\\Users\\Documents"
    clipr::write_clip(path, allow_non_interactive = TRUE)

    expect_equal(
      pathFromClipboard(),
      "C:/Users/Documents"
    )
  }
})
