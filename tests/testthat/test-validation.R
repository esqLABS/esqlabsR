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

  result <- validateScenariosFile(temp_file)
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
    expect_true("scenarios" %in% names(results) || "projectConfiguration" %in% names(results))
  }
})

test_that("Cross-reference validation detects invalid references", {
  # Mock validation results with mismatched IDs
  mock_scenarios <- validationResult$new()
  mock_scenarios$set_data(data.frame(
    IndividualId = c("ID1", "ID2", "INVALID_ID"),
    PopulationId = c("Pop1", "Pop2", "Pop3")
  ))

  mock_individuals <- validationResult$new()
  mock_individuals$set_data(data.frame(
    IndividualId = c("ID1", "ID2")
  ))

  validationResults <- list(
    scenarios = mock_scenarios,
    individuals = mock_individuals
  )

  result <- validateCrossReferences(NULL, validationResults)
  expect_true(result$has_critical_errors())
})
