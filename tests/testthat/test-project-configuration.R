test_that("`createProjectConfiguration()` works as expected with project template", {
  myConfig <- testProjectConfiguration()
  expect_true(isOfType(myConfig, "ProjectConfiguration"))
})

test_that("A warning is (not) displayed if path/file does not exist", {
  myConfig <- testProjectConfiguration()
  expect_no_warning(myConfig$outputFolder <- "this/directory/does/not/exist")
  expect_warning(myConfig$dataFolder <- "this/directory/does/not/exist")
})


test_that("`createDefaultProjectConfiguration()` is deprecated", {
  expect_warning(createDefaultProjectConfiguration(path = exampleProjectConfigurationPath()))
})



test_that("Project Configuration can be created from V5 project configuration file but raises a warning", {
  expect_warning(createProjectConfiguration(test_path("..", "data", "ProjectConfiguration-V5.xlsx")))
})

test_that("Project Configuration can be customized but throws warning if path are wrong", {
  myConfig <- testProjectConfiguration()

  expect_warning({
    myConfig$configurationsFolder <- "Wrong/Folder"
  })

  myConfig <- testProjectConfiguration()
  expect_warning({
    myConfig$dataFolder <- "folder/that/does/not/exist"
  })
  expect_warning({
    myConfig$modelFolder <- "folder/that/does/not/exist"
  })
  expect_warning({
    myConfig$populationsFolder <- "folder/that/does/not/exist"
  })

  myConfig <- testProjectConfiguration()
  expect_warning({
    myConfig$modelParamsFile <- "donotexist.xslx"
  })
  expect_warning({
    myConfig$individualsFile <- "donotexist.xslx"
  })
  expect_warning({
    myConfig$populationsFile <- "donotexist.xslx"
  })
  expect_warning({
    myConfig$scenariosFile <- "donotexist.xslx"
  })
  expect_warning({
    myConfig$applicationsFile <- "donotexist.xslx"
  })
  expect_warning({
    myConfig$plotsFile <- "donotexist.xslx"
  })
  expect_warning({
    myConfig$dataFile <- "donotexist.xslx"
  })
  expect_warning({
    myConfig$dataImporterConfigurationFile <- "donotexist.xslx"
  })
})



test_that("Project Configuration can be exported", {
  # Copy test file to temp location
  temp_file1 <- withr::local_tempfile(fileext = ".xlsx")
  file.copy(test_path("..", "data", "ProjectConfiguration-V5.xlsx"), temp_file1)
  myConfig <- suppressWarnings(createProjectConfiguration(temp_file1))

  # Import exported configuration and check if identical to original
  temp_file2 <- withr::local_tempfile(fileext = ".xlsx")
  myConfig$save(path = temp_file2)
  myConfig2 <- suppressWarnings(createProjectConfiguration(temp_file2))

  suppressWarnings({
    myConfig2$projectConfigurationFilePath <- myConfig$projectConfigurationFilePath
  })

  suppressWarnings(expect_equal(myConfig, myConfig2))
})

test_that("Project Configuration supports environment variable", {

  withr::with_envvar(new = c("ENV_VARIABLE_1"="path/from/env/variable/1",
                             "ENV_VARIABLE_2"="path/from/env/variable/2"),
                     code = {
                       # Using Env Variable in Excel files
                       pc <-
                         createProjectConfiguration(test_path("..", "data", "ProjectConfigurationEnvironmentVariable.xlsx"))

                       suppressWarnings(expect_match(pc$configurationsFolder, Sys.getenv("ENV_VARIABLE_1")))

                       # Set environment variable directly in the object
                       pc <- testProjectConfiguration()
                       suppressWarnings(pc$configurationsFolder <- "ENV_VARIABLE_1")
                     })
})
