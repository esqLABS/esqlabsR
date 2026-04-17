# Tests for validation infrastructure (validationResult class)

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

test_that("validationResult set_data works correctly", {
  result <- validationResult$new()
  test_data <- data.frame(a = 1:3, b = letters[1:3])

  expect_null(result$data)
  result$set_data(test_data)
  expect_equal(result$data, test_data)
})

# Tests for helper functions

test_that(".check_no_duplicates detects duplicates", {
  result <- validationResult$new()
  ids <- c("a", "b", "a", "c")

  result <- esqlabsR:::.check_no_duplicates(ids, "testId", result)

  expect_true(result$has_critical_errors())
  expect_true(grepl("Duplicate", result$critical_errors[[1]]$message))
})

test_that(".check_no_duplicates passes with no duplicates", {
  result <- validationResult$new()
  ids <- c("a", "b", "c")

  result <- esqlabsR:::.check_no_duplicates(ids, "testId", result)

  expect_false(result$has_critical_errors())
})

test_that(".check_no_duplicates handles NA values", {
  result <- validationResult$new()
  ids <- c("a", NA, "b", NA)

  result <- esqlabsR:::.check_no_duplicates(ids, "testId", result)

  expect_false(result$has_critical_errors())
})

test_that(".check_required_fields detects missing fields", {
  result <- validationResult$new()
  entry <- list(field1 = "value", field2 = NULL)

  result <- esqlabsR:::.check_required_fields(
    entry,
    c("field1", "field2", "field3"),
    "test entry",
    result
  )

  expect_true(result$has_critical_errors())
  expect_equal(length(result$critical_errors), 2)
})

test_that(".check_required_fields detects empty string fields", {
  result <- validationResult$new()
  entry <- list(field1 = "value", field2 = "")

  result <- esqlabsR:::.check_required_fields(
    entry,
    c("field1", "field2"),
    "test entry",
    result
  )

  expect_true(result$has_critical_errors())
})

test_that(".check_required_fields detects NA fields", {
  result <- validationResult$new()
  entry <- list(field1 = "value", field2 = NA)

  result <- esqlabsR:::.check_required_fields(
    entry,
    c("field1", "field2"),
    "test entry",
    result
  )

  expect_true(result$has_critical_errors())
})

test_that(".check_required_fields passes with all fields present", {
  result <- validationResult$new()
  entry <- list(field1 = "value1", field2 = "value2")

  result <- esqlabsR:::.check_required_fields(
    entry,
    c("field1", "field2"),
    "test entry",
    result
  )

  expect_false(result$has_critical_errors())
})

test_that(".categorize_message categorizes messages correctly", {
  expect_equal(
    esqlabsR:::.categorize_message("missing field X"),
    "Missing Fields"
  )
  expect_equal(
    esqlabsR:::.categorize_message("required field not found"),
    "Missing Fields"
  )
  expect_equal(
    esqlabsR:::.categorize_message("duplicate ID found"),
    "Uniqueness"
  )
  expect_equal(
    esqlabsR:::.categorize_message("references undefined scenario"),
    "Invalid Reference"
  )
  expect_equal(
    esqlabsR:::.categorize_message("length mismatch"),
    "Structure"
  )
  expect_equal(
    esqlabsR:::.categorize_message("some other error"),
    "Validation"
  )
})
