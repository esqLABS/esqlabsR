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

test_that("readExcel trims whitespace and removes special characters from text columns", {
  withr::with_tempdir(
    code = {
      # Create test data with messy text - 3 rows for consistency
      testData <- data.frame(
        ScenarioName = c(" Scenario1 ", "Scenario2\n\t", "Scenario3  "),
        ModelParamSheets = c(
          "\n Global",
          '"Global", "Aciclovir", "Sheet, with comma"',
          "Global, MissingParam"
        ),
        SimulationTime = c("0, 24, 60", "0, 24, 60\n", "  0, 12  "),
        `Container Path` = c(
          "Organism|Liver",
          " Organism\n|Kidney ",
          "\tOrganism|Heart  "
        ),
        MinValue = c(-2.0, -1.0, 0.0),
        SteadyState = c(TRUE, NA, FALSE),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      testPath <- "test_text_cleaning.xlsx"
      .writeExcel(testData, testPath)

      result <- readExcel(testPath) |> 
        .cleanTextColumns()

      expect_equal(result$ScenarioName[1], "Scenario1")
      expect_equal(result$ScenarioName[2], "Scenario2")
      expect_equal(result$ScenarioName[3], "Scenario3")
      
      expect_equal(result$ModelParamSheets[1], "Global")
      expect_equal(result$ModelParamSheets[2], '"Global", "Aciclovir", "Sheet, with comma"')
      expect_equal(result$ModelParamSheets[3], "Global, MissingParam")
      
      expect_equal(result$SimulationTime[1], "0, 24, 60")
      expect_equal(result$SimulationTime[2], "0, 24, 60")
      expect_equal(result$SimulationTime[3], "0, 12")
      
      expect_equal(result$`Container Path`[1], "Organism|Liver")
      expect_equal(result$`Container Path`[2], "Organism|Kidney")
      expect_equal(result$`Container Path`[3], "Organism|Heart")
      
      expect_equal(result$MinValue, c(-2.0, -1.0, 0.0))
      
      expect_equal(result$SteadyState[1], TRUE)
      expect_true(is.na(result$SteadyState[2]))
      expect_equal(result$SteadyState[3], FALSE)
    }
  )
})
