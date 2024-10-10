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
  expect_warning(createDefaultProjectConfiguration(path = example_ProjectConfiguration()))
})



test_that("Project Configuration can be created from V5 project configuration file but raises a warning", {
  expect_warning(createProjectConfiguration(test_path("..", "data", "ProjectConfiguration-V5.xlsx")))
})

test_that("Project Configuration can be customized but throws warning if path are wrong", {
  myConfig <- testProjectConfiguration()

  expect_warning({myConfig$configurationsFolder <- "Wrong/Folder"})

  myConfig <- testProjectConfiguration()
  expect_warning({myConfig$dataFolder <- "folder/that/does/not/exist"})
  expect_warning({myConfig$modelFolder <- "folder/that/does/not/exist"})
  expect_warning({myConfig$populationsFolder <- "folder/that/does/not/exist"})

  myConfig <- testProjectConfiguration()
  expect_warning({myConfig$modelParamsFile <- "donotexist.xslx"})
  expect_warning({myConfig$individualsFile <- "donotexist.xslx"})
  expect_warning({myConfig$populationsFile <- "donotexist.xslx"})
  expect_warning({myConfig$scenariosFile <- "donotexist.xslx"})
  expect_warning({myConfig$applicationsFile <- "donotexist.xslx"})
  expect_warning({myConfig$plotsFile <- "donotexist.xslx"})
  expect_warning({myConfig$dataFile <- "donotexist.xslx"})
  expect_warning({myConfig$dataImporterConfigurationFile <- "donotexist.xslx"})
})
