test_that("validationResult class works correctly", {
  result <- validationResult$new()

  expect_true(result$is_valid())
  expect_false(result$has_critical_errors())

  result$add_critical_error("Test", "Test error")
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())

  result$add_warning("Test", "Test warning")
  expect_equal(length(result$warnings), 1)

  summary <- result$get_summary()
  expect_equal(summary$critical_error_count, 1)
  expect_equal(summary$warning_count, 1)
})

test_that("validateScenariosFile detects missing sheets", {
  # Create temp file without required sheets
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(WrongSheet = data.frame(a = 1)), temp_file)

  # Use ::: to access internal function for unit testing
  result <- esqlabsR:::.validateScenariosFile(temp_file)
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())

  unlink(temp_file)
})

test_that("validateAllConfigurations processes all files", {
  # Use existing test data from inst/extdata/examples/TestProject
  test_config_path <- system.file(
    "extdata/examples/TestProject/ProjectConfiguration.xlsx",
    package = "esqlabsR"
  )

  if (file.exists(test_config_path)) {
    results <- validateAllConfigurations(test_config_path)
    expect_true(inherits(results, "ValidationResults"))
    expect_true(
      "scenarios" %in%
        names(results) ||
        "projectConfiguration" %in% names(results)
    )
  }
})

test_that("validateAllConfigurations handles NA file paths", {
  # Create a mock ProjectConfiguration with NA values
  mockConfig <- list(
    scenariosFile = NA_character_,
    plotsFile = NA_character_,
    individualsFile = NA_character_,
    populationsFile = NA_character_,
    modelParamsFile = NA_character_,
    applicationsFile = NA_character_
  )
  class(mockConfig) <- c("ProjectConfiguration", class(mockConfig))

  results <- validateAllConfigurations(mockConfig)
  expect_true(inherits(results, "ValidationResults"))
  # Should only have crossReferences validation when all files are NA
  expect_true("crossReferences" %in% names(results))
})

test_that("validateAllConfigurations validates all file types when provided", {
  # Create temp files for each type
  temp_dir <- tempdir()
  scenarios_file <- file.path(temp_dir, "Scenarios.xlsx")
  plots_file <- file.path(temp_dir, "Plots.xlsx")
  individuals_file <- file.path(temp_dir, "Individuals.xlsx")
  populations_file <- file.path(temp_dir, "Populations.xlsx")
  models_file <- file.path(temp_dir, "Models.xlsx")
  applications_file <- file.path(temp_dir, "Applications.xlsx")

  # Create minimal valid files
  openxlsx::write.xlsx(
    list(
      Scenarios = data.frame(
        IndividualId = "ID1",
        PopulationId = "Pop1",
        ApplicationProtocol = "App1",
        SteadyStateTime = 0,
        ScenarioName = "S1"
      ),
      OutputPaths = data.frame(
        OutputPathId = "OP1",
        OutputPath = "Path1"
      )
    ),
    scenarios_file
  )

  openxlsx::write.xlsx(
    list(
      DataCombined = data.frame(
        DataCombinedName = "DC1",
        dataType = "simulated",
        scenario = "S1"
      ),
      plotConfiguration = data.frame(
        DataCombinedName = "DC1",
        plotID = "P1",
        plotType = "line"
      )
    ),
    plots_file
  )

  openxlsx::write.xlsx(
    list(
      IndividualBiometrics = data.frame(
        IndividualId = "ID1",
        Age = 30
      )
    ),
    individuals_file
  )

  openxlsx::write.xlsx(
    list(
      Demographics = data.frame(
        PopulationName = "Pop1",
        NumberOfIndividuals = 100
      )
    ),
    populations_file
  )

  openxlsx::write.xlsx(
    list(Model1 = data.frame(paths = "P1", values = 1)),
    models_file
  )

  openxlsx::write.xlsx(
    list(App1 = data.frame(dose = 100, time = 0)),
    applications_file
  )

  # Create mock config with all files
  mockConfig <- list(
    scenariosFile = scenarios_file,
    plotsFile = plots_file,
    individualsFile = individuals_file,
    populationsFile = populations_file,
    modelParamsFile = models_file,
    applicationsFile = applications_file
  )
  class(mockConfig) <- c("ProjectConfiguration", class(mockConfig))

  results <- validateAllConfigurations(mockConfig)

  expect_true(inherits(results, "ValidationResults"))
  expect_true("scenarios" %in% names(results))
  expect_true("plots" %in% names(results))
  expect_true("individuals" %in% names(results))
  expect_true("populations" %in% names(results))
  expect_true("models" %in% names(results))
  expect_true("applications" %in% names(results))
  expect_true("crossReferences" %in% names(results))

  # Clean up
  unlink(c(
    scenarios_file,
    plots_file,
    individuals_file,
    populations_file,
    models_file,
    applications_file
  ))
})

test_that("validateAllConfigurations stops early on invalid project config", {
  # Test with invalid path
  results <- validateAllConfigurations("nonexistent_config.xlsx")

  expect_true(inherits(results, "ValidationResults"))
  expect_true("projectConfiguration" %in% names(results))
  expect_false(results$projectConfiguration$is_valid())
  # Should not have other validation results if project config is invalid
  expect_false("scenarios" %in% names(results))
})

test_that("Cross-reference validation detects invalid individual references", {
  # Mock validation results with mismatched IDs
  mock_scenarios <- validationResult$new()
  mock_scenarios$set_data(data.frame(
    IndividualId = c("ID1", "ID2", "INVALID_ID"),
    PopulationId = c("Pop1", "Pop2", "Pop3"),
    ScenarioName = c("S1", "S2", "S3")
  ))

  mock_individuals <- validationResult$new()
  mock_individuals$set_data(data.frame(
    IndividualId = c("ID1", "ID2")
  ))

  validationResults <- list(
    scenarios = mock_scenarios,
    individuals = mock_individuals
  )

  # Use ::: to access internal function for unit testing
  result <- esqlabsR:::.validateCrossReferences(NULL, validationResults)
  expect_true(result$has_critical_errors())
  expect_true(length(result$critical_errors) > 0)
})

test_that("Cross-reference validation detects invalid population references", {
  mock_scenarios <- validationResult$new()
  mock_scenarios$set_data(data.frame(
    IndividualId = c("ID1", "ID2"),
    PopulationId = c("Pop1", "INVALID_POP"),
    ScenarioName = c("S1", "S2")
  ))

  mock_populations <- validationResult$new()
  mock_populations$set_data(data.frame(
    PopulationName = c("Pop1", "Pop2")
  ))

  validationResults <- list(
    scenarios = mock_scenarios,
    populations = mock_populations
  )

  result <- esqlabsR:::.validateCrossReferences(NULL, validationResults)
  expect_true(result$has_critical_errors())
})

test_that("Cross-reference validation detects invalid plot scenario references", {
  mock_scenarios <- validationResult$new()
  mock_scenarios$set_data(data.frame(
    IndividualId = c("ID1"),
    PopulationId = c("Pop1"),
    ScenarioName = c("ValidScenario")
  ))

  mock_plots <- validationResult$new()
  mock_plots$set_data(list(
    DataCombined = data.frame(
      dataType = c("simulated", "simulated"),
      scenario = c("ValidScenario", "InvalidScenario")
    )
  ))

  validationResults <- list(
    scenarios = mock_scenarios,
    plots = mock_plots
  )

  result <- esqlabsR:::.validateCrossReferences(NULL, validationResults)
  expect_true(result$has_critical_errors())
})

test_that("Cross-reference validation skips when previous critical errors exist", {
  mock_result_with_error <- validationResult$new()
  mock_result_with_error$add_critical_error("Test", "Previous error")

  validationResults <- list(
    scenarios = mock_result_with_error
  )

  result <- esqlabsR:::.validateCrossReferences(NULL, validationResults)
  expect_false(result$has_critical_errors())
  expect_equal(length(result$warnings), 1)
})

test_that("Cross-reference validation passes with valid references", {
  mock_scenarios <- validationResult$new()
  mock_scenarios$set_data(data.frame(
    IndividualId = c("ID1", "ID2"),
    PopulationId = c("Pop1", "Pop2"),
    ScenarioName = c("S1", "S2")
  ))

  mock_individuals <- validationResult$new()
  mock_individuals$set_data(data.frame(
    IndividualId = c("ID1", "ID2")
  ))

  mock_populations <- validationResult$new()
  mock_populations$set_data(data.frame(
    PopulationName = c("Pop1", "Pop2")
  ))

  mock_plots <- validationResult$new()
  mock_plots$set_data(list(
    DataCombined = data.frame(
      dataType = c("simulated", "simulated"),
      scenario = c("S1", "S2")
    )
  ))

  validationResults <- list(
    scenarios = mock_scenarios,
    individuals = mock_individuals,
    populations = mock_populations,
    plots = mock_plots
  )

  result <- esqlabsR:::.validateCrossReferences(NULL, validationResults)
  expect_false(result$has_critical_errors())
  expect_true(result$is_valid())
})

test_that("isAnyCriticalErrors detects errors in validation results", {
  result_with_error <- validationResult$new()
  result_with_error$add_critical_error("Test", "Error message")

  result_no_error <- validationResult$new()

  validationResults <- list(
    file1 = result_no_error,
    file2 = result_with_error
  )

  expect_true(isAnyCriticalErrors(validationResults))
})

test_that("isAnyCriticalErrors returns FALSE when no errors", {
  result1 <- validationResult$new()
  result2 <- validationResult$new()

  validationResults <- list(
    file1 = result1,
    file2 = result2
  )

  expect_false(isAnyCriticalErrors(validationResults))
})

test_that("isAnyCriticalErrors handles non-validationResult objects", {
  result_with_error <- validationResult$new()
  result_with_error$add_critical_error("Test", "Error message")

  validationResults <- list(
    file1 = result_with_error,
    file2 = "not a validation result",
    file3 = NULL
  )

  expect_true(isAnyCriticalErrors(validationResults))
})

test_that("validationSummary correctly counts errors and warnings", {
  result1 <- validationResult$new()
  result1$add_critical_error("Test", "Error 1")
  result1$add_critical_error("Test", "Error 2")
  result1$add_warning("Test", "Warning 1")

  result2 <- validationResult$new()
  result2$add_warning("Test", "Warning 2")

  validationResults <- list(
    scenarios = result1,
    plots = result2
  )
  class(validationResults) <- c("ValidationResults", class(validationResults))

  summary <- validationSummary(validationResults)

  expect_equal(summary$total_critical_errors, 2)
  expect_equal(summary$total_warnings, 2)
  expect_equal(length(summary$files_with_errors), 1)
  expect_equal(length(summary$files_with_warnings), 2)
  expect_true("scenarios" %in% summary$files_with_errors)
  expect_true("scenarios" %in% summary$files_with_warnings)
  expect_true("plots" %in% summary$files_with_warnings)
})

test_that("validationSummary handles empty validation results", {
  validationResults <- list()
  class(validationResults) <- c("ValidationResults", class(validationResults))

  summary <- validationSummary(validationResults)

  expect_equal(summary$total_critical_errors, 0)
  expect_equal(summary$total_warnings, 0)
  expect_equal(length(summary$files_with_errors), 0)
  expect_equal(length(summary$files_with_warnings), 0)
})

test_that(".validateModelsFile detects missing file", {
  result <- esqlabsR:::.validateModelsFile("nonexistent_file.xlsx")
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())
})

test_that(".validateModelsFile handles empty sheets", {
  temp_file <- tempfile(fileext = ".xlsx")
  # Create a file with an empty sheet
  openxlsx::write.xlsx(list(Model1 = data.frame()), temp_file)

  result <- esqlabsR:::.validateModelsFile(temp_file)
  expect_equal(length(result$warnings), 1)

  unlink(temp_file)
})

test_that(".validateModelsFile warns about missing parameter path column", {
  temp_file <- tempfile(fileext = ".xlsx")
  # Create a sheet without parameter path column
  openxlsx::write.xlsx(
    list(Model1 = data.frame(values = c(1, 2, 3))),
    temp_file
  )

  result <- esqlabsR:::.validateModelsFile(temp_file)
  expect_true(length(result$warnings) > 0)

  unlink(temp_file)
})

test_that(".validateModelsFile handles valid models file", {
  temp_file <- tempfile(fileext = ".xlsx")
  # Create a valid models file
  openxlsx::write.xlsx(
    list(
      Model1 = data.frame(
        paths = c("Param1", "Param2"),
        values = c(1, 2)
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateModelsFile(temp_file)
  expect_true(result$is_valid())

  unlink(temp_file)
})

test_that(".validateApplicationsFile detects missing file", {
  result <- esqlabsR:::.validateApplicationsFile("nonexistent_file.xlsx")
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())
})

test_that(".validateApplicationsFile handles empty sheets", {
  temp_file <- tempfile(fileext = ".xlsx")
  # Create a file with an empty sheet
  openxlsx::write.xlsx(list(Application1 = data.frame()), temp_file)

  result <- esqlabsR:::.validateApplicationsFile(temp_file)
  expect_equal(length(result$warnings), 1)

  unlink(temp_file)
})

test_that(".validateApplicationsFile handles valid applications file", {
  temp_file <- tempfile(fileext = ".xlsx")
  # Create a valid applications file
  openxlsx::write.xlsx(
    list(
      Application1 = data.frame(
        dose = c(100, 200),
        time = c(0, 24)
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateApplicationsFile(temp_file)
  expect_true(result$is_valid())

  unlink(temp_file)
})

# Tests for .validateIndividualsFile ----

test_that(".validateIndividualsFile detects missing file", {
  result <- esqlabsR:::.validateIndividualsFile("nonexistent_file.xlsx")
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())
})

test_that(".validateIndividualsFile detects missing required sheet", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(WrongSheet = data.frame(a = 1)), temp_file)

  result <- esqlabsR:::.validateIndividualsFile(temp_file)
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())

  unlink(temp_file)
})

test_that(".validateIndividualsFile detects missing required columns", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      IndividualBiometrics = data.frame(
        IndividualId = "ID1",
        Species = "Human"
        # Missing other required columns
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateIndividualsFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validateIndividualsFile detects duplicate IndividualId", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      IndividualBiometrics = data.frame(
        IndividualId = c("ID1", "ID1"),
        Species = c("Human", "Human"),
        Population = c("European", "European"),
        Gender = c("Male", "Female"),
        `Age [year(s)]` = c(30, 40),
        `Height [cm]` = c(175, 165),
        `Weight [kg]` = c(70, 60),
        check.names = FALSE
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateIndividualsFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validateIndividualsFile warns about non-numeric columns", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      IndividualBiometrics = data.frame(
        IndividualId = c("ID1", "ID2"),
        Species = c("Human", "Human"),
        Population = c("European", "European"),
        Gender = c("Male", "Female"),
        `Age [year(s)]` = c("thirty", "forty"),
        `Height [cm]` = c(175, 165),
        `Weight [kg]` = c(70, 60),
        check.names = FALSE
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateIndividualsFile(temp_file)
  expect_true(length(result$warnings) > 0)

  unlink(temp_file)
})

test_that(".validateIndividualsFile handles valid file", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      IndividualBiometrics = data.frame(
        IndividualId = c("ID1", "ID2"),
        Species = c("Human", "Human"),
        Population = c("European", "European"),
        Gender = c("Male", "Female"),
        `Age [year(s)]` = c(30, 40),
        `Height [cm]` = c(175, 165),
        `Weight [kg]` = c(70, 60),
        check.names = FALSE
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateIndividualsFile(temp_file)
  expect_true(result$is_valid())

  unlink(temp_file)
})

# Tests for .validatePopulationsFile ----

test_that(".validatePopulationsFile detects missing file", {
  result <- esqlabsR:::.validatePopulationsFile("nonexistent_file.xlsx")
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())
})

test_that(".validatePopulationsFile detects missing required sheet", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(WrongSheet = data.frame(a = 1)), temp_file)

  result <- esqlabsR:::.validatePopulationsFile(temp_file)
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())

  unlink(temp_file)
})

test_that(".validatePopulationsFile detects missing required columns", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Demographics = data.frame(
        PopulationName = "Pop1",
        species = "Human"
        # Missing other required columns
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validatePopulationsFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validatePopulationsFile detects duplicate PopulationName", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Demographics = data.frame(
        PopulationName = c("Pop1", "Pop1"),
        species = c("Human", "Human"),
        population = c("European", "European"),
        numberOfIndividuals = c(100, 100),
        proportionOfFemales = c(0.5, 0.5),
        ageMin = c(20, 20),
        ageMax = c(60, 60),
        weightMin = c(50, 50),
        weightMax = c(100, 100),
        heightMin = c(150, 150),
        heightMax = c(200, 200),
        BMIMin = c(18, 18),
        BMIMax = c(30, 30)
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validatePopulationsFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validatePopulationsFile warns about invalid proportionOfFemales", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Demographics = data.frame(
        PopulationName = c("Pop1", "Pop2"),
        species = c("Human", "Human"),
        population = c("European", "European"),
        numberOfIndividuals = c(100, 100),
        proportionOfFemales = c(1.5, -0.1),
        ageMin = c(20, 20),
        ageMax = c(60, 60),
        weightMin = c(50, 50),
        weightMax = c(100, 100),
        heightMin = c(150, 150),
        heightMax = c(200, 200),
        BMIMin = c(18, 18),
        BMIMax = c(30, 30)
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validatePopulationsFile(temp_file)
  expect_true(length(result$warnings) > 0)

  unlink(temp_file)
})

test_that(".validatePopulationsFile handles valid file", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Demographics = data.frame(
        PopulationName = c("Pop1", "Pop2"),
        species = c("Human", "Human"),
        population = c("European", "European"),
        numberOfIndividuals = c(100, 100),
        proportionOfFemales = c(0.5, 0.5),
        ageMin = c(20, 20),
        ageMax = c(60, 60),
        weightMin = c(50, 50),
        weightMax = c(100, 100),
        heightMin = c(150, 150),
        heightMax = c(200, 200),
        BMIMin = c(18, 18),
        BMIMax = c(30, 30)
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validatePopulationsFile(temp_file)
  expect_true(result$is_valid())

  unlink(temp_file)
})

# Tests for .validatePlotsFile ----

test_that(".validatePlotsFile detects missing file", {
  result <- esqlabsR:::.validatePlotsFile("nonexistent_file.xlsx")
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())
})

test_that(".validatePlotsFile detects missing required sheets", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(WrongSheet = data.frame(a = 1)), temp_file)

  result <- esqlabsR:::.validatePlotsFile(temp_file)
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())

  unlink(temp_file)
})

test_that(".validatePlotsFile detects missing columns in DataCombined", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      DataCombined = data.frame(WrongColumn = "value"),
      plotConfiguration = data.frame(
        DataCombinedName = "DC1",
        plotID = "P1",
        plotType = "line"
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validatePlotsFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validatePlotsFile detects missing columns in plotConfiguration", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      DataCombined = data.frame(
        DataCombinedName = "DC1",
        dataType = "simulated"
      ),
      plotConfiguration = data.frame(WrongColumn = "value")
    ),
    temp_file
  )

  result <- esqlabsR:::.validatePlotsFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validatePlotsFile warns about missing optional sheets", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      DataCombined = data.frame(
        DataCombinedName = "DC1",
        dataType = "simulated"
      ),
      plotConfiguration = data.frame(
        DataCombinedName = "DC1",
        plotID = "P1",
        plotType = "line"
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validatePlotsFile(temp_file)
  # Should have warnings about missing plotGrids and exportConfiguration
  expect_true(length(result$warnings) >= 2)

  unlink(temp_file)
})

test_that(".validatePlotsFile warns about empty DataCombined sheet", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      DataCombined = data.frame(
        DataCombinedName = character(),
        dataType = character()
      ),
      plotConfiguration = data.frame(
        DataCombinedName = "DC1",
        plotID = "P1",
        plotType = "line"
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validatePlotsFile(temp_file)
  expect_true(length(result$warnings) > 0)

  unlink(temp_file)
})

# Tests for .validateScenariosFile ----

test_that(".validateScenariosFile detects missing file", {
  result <- esqlabsR:::.validateScenariosFile("nonexistent_file.xlsx")
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())
})

test_that(".validateScenariosFile detects missing required sheets", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(WrongSheet = data.frame(a = 1)), temp_file)

  result <- esqlabsR:::.validateScenariosFile(temp_file)
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())

  unlink(temp_file)
})

test_that(".validateScenariosFile detects missing columns in Scenarios", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Scenarios = data.frame(WrongColumn = "value"),
      OutputPaths = data.frame(
        OutputPathId = "OP1",
        OutputPath = "Path1"
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateScenariosFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validateScenariosFile detects missing columns in OutputPaths", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Scenarios = data.frame(
        IndividualId = "ID1",
        PopulationId = "Pop1",
        ApplicationProtocol = "App1",
        SteadyStateTime = 0
      ),
      OutputPaths = data.frame(WrongColumn = "value")
    ),
    temp_file
  )

  result <- esqlabsR:::.validateScenariosFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validateScenariosFile detects duplicate OutputPathId", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Scenarios = data.frame(
        IndividualId = "ID1",
        PopulationId = "Pop1",
        ApplicationProtocol = "App1",
        SteadyStateTime = 0
      ),
      OutputPaths = data.frame(
        OutputPathId = c("OP1", "OP1"),
        OutputPath = c("Path1", "Path2")
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateScenariosFile(temp_file)
  expect_false(result$is_valid())

  unlink(temp_file)
})

test_that(".validateScenariosFile warns about empty Scenarios sheet", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Scenarios = data.frame(
        IndividualId = character(),
        PopulationId = character(),
        ApplicationProtocol = character(),
        SteadyStateTime = numeric()
      ),
      OutputPaths = data.frame(
        OutputPathId = "OP1",
        OutputPath = "Path1"
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateScenariosFile(temp_file)
  expect_true(length(result$warnings) > 0)

  unlink(temp_file)
})

test_that(".validateScenariosFile handles valid file", {
  temp_file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Scenarios = data.frame(
        IndividualId = c("ID1", "ID2"),
        PopulationId = c("Pop1", "Pop2"),
        ApplicationProtocol = c("App1", "App2"),
        SteadyStateTime = c(0, 0)
      ),
      OutputPaths = data.frame(
        OutputPathId = c("OP1", "OP2"),
        OutputPath = c("Path1", "Path2")
      )
    ),
    temp_file
  )

  result <- esqlabsR:::.validateScenariosFile(temp_file)
  expect_true(result$is_valid())

  unlink(temp_file)
})

# Tests for validationResult class methods ----

test_that("validationResult get_formatted_messages works correctly", {
  result <- validationResult$new()
  result$add_critical_error("Structure", "Missing required field")
  result$add_warning("Data", "Value out of range")

  formatted <- result$get_formatted_messages()

  expect_true(is.list(formatted))
  expect_true("critical" %in% names(formatted))
  expect_true("warnings" %in% names(formatted))
  expect_equal(length(formatted$critical), 1)
  expect_equal(length(formatted$warnings), 1)
  expect_true(grepl("Structure", formatted$critical[[1]]))
  expect_true(grepl("Data", formatted$warnings[[1]]))
})

test_that("validationResult add_critical_error with details works", {
  result <- validationResult$new()
  result$add_critical_error(
    "Structure",
    "Missing field",
    details = list(sheet = "Sheet1", row = 5)
  )

  expect_equal(length(result$critical_errors), 1)
  expect_equal(result$critical_errors[[1]]$details$sheet, "Sheet1")
  expect_equal(result$critical_errors[[1]]$details$row, 5)
})

test_that("validationResult add_warning with details works", {
  result <- validationResult$new()
  result$add_warning(
    "Data",
    "Value warning",
    details = list(column = "Age", value = -5)
  )

  expect_equal(length(result$warnings), 1)
  expect_equal(result$warnings[[1]]$details$column, "Age")
  expect_equal(result$warnings[[1]]$details$value, -5)
})
