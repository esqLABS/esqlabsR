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
  expect_warning(createDefaultProjectConfiguration(
    path = exampleProjectConfigurationPath()
  ))
})


test_that("Project Configuration can be created from V5 project configuration file but raises a warning", {
  expect_warning(createProjectConfiguration(test_path(
    "..",
    "data",
    "ProjectConfiguration-V5.xlsx"
  )))
})

test_that("Project Configuration can be customized but throws warning if path are wrong", {
  # Create a project configuration using temporary project
  temp_project <- with_temp_project()
  myConfig <- temp_project$config

  expect_warning({
    myConfig$configurationsFolder <- "Wrong/Folder"
  })

  # Create a new temporary project for each test to avoid interference
  temp_project2 <- with_temp_project()
  myConfig <- temp_project2$config
  expect_warning({
    myConfig$dataFolder <- "folder/data/does/not/exist"
  })
  expect_warning({
    myConfig$modelFolder <- "folder/model/does/not/exist"
  })
  expect_warning({
    myConfig$populationsFolder <- "folder/populations/does/not/exist"
  })

  # Create a new temporary project for each test to avoid interference
  temp_project3 <- with_temp_project()
  myConfig <- temp_project3$config
  expect_warning({
    myConfig$modelParamsFile <- "modelparams_donotexist.xlsx"
  })
  expect_warning({
    myConfig$individualsFile <- "individuals_donotexist.xlsx"
  })
  expect_warning({
    myConfig$populationsFile <- "populations_donotexist.xlsx"
  })
  expect_warning({
    myConfig$scenariosFile <- "scenarios_donotexist.xlsx"
  })
  expect_warning({
    myConfig$applicationsFile <- "applications_donotexist.xlsx"
  })
  expect_warning({
    myConfig$plotsFile <- "plots_donotexist.xlsx"
  })
  expect_warning({
    myConfig$dataFile <- "data_donotexist.xlsx"
  })
  expect_warning({
    myConfig$dataImporterConfigurationFile <- "importer_donotexist.xml"
  })
})


test_that("Project Configuration can be exported", {
  # Copy test file to temp location
  temp_file1 <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(testProjectConfiguration()$save(path = temp_file1))

  # Load the file and compare
  expect_true(file.exists(temp_file1))
  imported_pc <- createProjectConfiguration(path = temp_file1)

  imported_pc$projectConfigurationFilePath <- testProjectConfigurationPath()

  expect_equal(testProjectConfiguration(), imported_pc)
})

test_that("Project Configuration supports environment variable", {
  withr::with_envvar(
    new = c(
      "ENV_VARIABLE_1" = "C:/path/from/env/variable/1",
      "ENV_VARIABLE_2" = "path/from/env/variable/2"
    ),
    code = {
      # Using Env Variable in Excel files
      pc <-
        createProjectConfiguration(test_path(
          "..",
          "data",
          "ProjectConfigurationEnvironmentVariable.xlsx"
        ))

      suppressWarnings(expect_match(
        pc$configurationsFolder,
        Sys.getenv("ENV_VARIABLE_1")
      ))

      # Set environment variable directly in the object
      pc <- testProjectConfiguration()
      suppressWarnings(pc$configurationsFolder <- "ENV_VARIABLE_1")
      suppressWarnings(expect_match(
        pc$configurationsFolder,
        Sys.getenv("ENV_VARIABLE_1")
      ))
    }
  )
})

# Tests for the modified flag behavior
test_that("modified flag is FALSE when ProjectConfiguration is first created", {
  myConfig <- testProjectConfiguration()
  expect_false(myConfig$modified)
})

test_that("modified flag is read-only and cannot be set directly", {
  myConfig <- testProjectConfiguration()
  expect_error(myConfig$modified <- TRUE, "modified is readonly")
})

test_that("modified flag becomes TRUE when any configuration property is changed", {
  myConfig <- testProjectConfiguration()
  expect_false(myConfig$modified)

  # Test each property that should set modified flag
  suppressWarnings(myConfig$modelFolder <- "new/model/folder")
  expect_true(myConfig$modified)

  # Reset and test configurationsFolder
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$configurationsFolder <- "new/config/folder")
  expect_true(myConfig$modified)

  # Reset and test modelParamsFile
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$modelParamsFile <- "newModelParams.xlsx")
  expect_true(myConfig$modified)

  # Reset and test individualsFile
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$individualsFile <- "newIndividuals.xlsx")
  expect_true(myConfig$modified)

  # Reset and test populationsFile
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$populationsFile <- "newPopulations.xlsx")
  expect_true(myConfig$modified)

  # Reset and test populationsFolder
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$populationsFolder <- "newPopulationsFolder")
  expect_true(myConfig$modified)

  # Reset and test scenariosFile
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$scenariosFile <- "newScenarios.xlsx")
  expect_true(myConfig$modified)

  # Reset and test applicationsFile
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$applicationsFile <- "newApplications.xlsx")
  expect_true(myConfig$modified)

  # Reset and test plotsFile
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$plotsFile <- "newPlots.xlsx")
  expect_true(myConfig$modified)

  # Reset and test dataFolder
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$dataFolder <- "new/data/folder")
  expect_true(myConfig$modified)

  # Reset and test dataFile
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$dataFile <- "newData.xlsx")
  expect_true(myConfig$modified)

  # Reset and test dataImporterConfigurationFile
  myConfig <- testProjectConfiguration()
  suppressWarnings(myConfig$dataImporterConfigurationFile <- "newImporter.xml")
  expect_true(myConfig$modified)

  # Reset and test outputFolder
  myConfig <- testProjectConfiguration()
  myConfig$outputFolder <- "new/output/folder"
  expect_true(myConfig$modified)
})

test_that("modified flag becomes FALSE after saving the configuration", {
  myConfig <- testProjectConfiguration()

  # Modify a property
  suppressWarnings(myConfig$modelFolder <- "modified/folder")
  expect_true(myConfig$modified)

  # Save to temporary file
  temp_file <- withr::local_tempfile(fileext = ".xlsx")
  myConfig$save(path = temp_file)

  # After saving, modified should be FALSE
  expect_false(myConfig$modified)
})

test_that("modified flag becomes FALSE after loading configuration from file", {
  myConfig <- testProjectConfiguration()

  # Modify a property
  suppressWarnings(myConfig$modelFolder <- "modified/folder")
  expect_true(myConfig$modified)

  # Load from file again (this should reset modified flag)
  myConfig$projectConfigurationFilePath <- testProjectConfigurationPath()
  expect_false(myConfig$modified)
})

test_that("modified flag persists across multiple property changes", {
  myConfig <- testProjectConfiguration()
  expect_false(myConfig$modified)

  # Make multiple changes
  suppressWarnings(myConfig$modelFolder <- "new/model/folder")
  expect_true(myConfig$modified)

  suppressWarnings(myConfig$dataFolder <- "new/data/folder")
  expect_true(myConfig$modified)

  myConfig$outputFolder <- "new/output/folder"
  expect_true(myConfig$modified)
})

test_that("empty ProjectConfiguration has modified flag FALSE", {
  # Create empty ProjectConfiguration without file path
  emptyConfig <- ProjectConfiguration$new()
  expect_false(emptyConfig$modified)
})

test_that("modified flag behavior with cloned ProjectConfiguration", {
  myConfig <- testProjectConfiguration()
  expect_false(myConfig$modified)

  # Clone the configuration
  clonedConfig <- myConfig$clone()
  expect_false(clonedConfig$modified)

  # Modify the original
  suppressWarnings(myConfig$modelFolder <- "modified/folder")
  expect_true(myConfig$modified)
  expect_false(clonedConfig$modified) # Clone should not be affected

  # Modify the clone
  suppressWarnings(clonedConfig$dataFolder <- "modified/data/folder")
  expect_true(myConfig$modified) # Original should still be modified
  expect_true(clonedConfig$modified) # Clone should now be modified
})

# ---- Accessor method tests ----
# These test the accessor methods with manually populated fields.
# Full JSON-loading integration is tested after Task 3.

describe("getModelParameters", {
  it("combines multiple sheets into one parameter structure", {
    pc <- ProjectConfiguration$new()
    pc$modelParameters <- list(
      Global = list(
        paths = c("Organism|Liver|EHC continuous fraction"),
        values = c(1),
        units = c("")
      ),
      Aciclovir = list(
        paths = c("Aciclovir|Lipophilicity"),
        values = c(-0.1),
        units = c("Log Units")
      )
    )
    result <- pc$getModelParameters(c("Global", "Aciclovir"))
    expect_equal(length(result$paths), 2)
    expect_equal(result$paths, c(
      "Organism|Liver|EHC continuous fraction",
      "Aciclovir|Lipophilicity"
    ))
    expect_equal(result$values, c(1, -0.1))
  })

  it("returns NULL when sheetNames is NULL", {
    pc <- ProjectConfiguration$new()
    pc$modelParameters <- list(
      Global = list(paths = "a|b", values = 1, units = "")
    )
    expect_null(pc$getModelParameters(NULL))
  })
})

describe("getIndividual", {
  it("returns NULL for unknown individualId", {
    pc <- ProjectConfiguration$new()
    pc$individuals <- list()
    expect_null(pc$getIndividual("nonexistent"))
  })
})

describe("getIndividualParameterSets", {
  it("returns NULL when individual has no parameterSets", {
    pc <- ProjectConfiguration$new()
    pc$individualParameterSetMapping <- list(Ind1 = character(0))
    pc$individualParameterSets <- list()
    expect_null(pc$getIndividualParameterSets("Ind1"))
  })
})

describe("getApplicationParameters", {
  it("returns parameters for a known protocol", {
    pc <- ProjectConfiguration$new()
    pc$applications <- list(
      Protocol1 = list(
        paths = c("App|Dose"),
        values = c(250),
        units = c("mg")
      )
    )
    result <- pc$getApplicationParameters("Protocol1")
    expect_equal(result$paths, "App|Dose")
    expect_equal(result$values, 250)
  })

  it("returns NULL for unknown protocol", {
    pc <- ProjectConfiguration$new()
    pc$applications <- list()
    expect_null(pc$getApplicationParameters("unknown"))
  })
})

describe("getPopulation", {
  it("returns NULL for unknown populationId", {
    pc <- ProjectConfiguration$new()
    pc$populations <- list()
    expect_null(pc$getPopulation("unknown"))
  })
})

describe("getOutputPaths", {
  it("resolves IDs to path strings", {
    pc <- ProjectConfiguration$new()
    pc$outputPaths <- c(
      id1 = "Organism|Path1",
      id2 = "Organism|Path2"
    )
    result <- pc$getOutputPaths(c("id1", "id2"))
    expect_equal(result, c("Organism|Path1", "Organism|Path2"))
  })

  it("returns NULL when outputPathIds is NULL", {
    pc <- ProjectConfiguration$new()
    pc$outputPaths <- c(id1 = "Organism|Path1")
    expect_null(pc$getOutputPaths(NULL))
  })
})
