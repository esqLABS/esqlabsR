---
name: testing-r-packages
description: >
  Best practices for writing R package tests using testthat version 3+. Use
  when writing, organizing, or improving tests for esqlabsR. Covers test
  structure, expectations, fixtures, snapshots, mocking, and modern testthat 3
  patterns including self-sufficient tests, proper cleanup with withr, and
  snapshot testing.
---

# Testing esqlabsR with testthat

Modern best practices for R package testing using testthat 3+.

## File Organization

**Mirror package structure:**
- Code in `R/foofy.R` → tests in `tests/testthat/test-foofy.R`
- Use `usethis::use_r("foofy")` and `usethis::use_test("foofy")` to create paired files

**Special files:**
- `tests/testthat/setup.R` – Shared test setup (loaded before all tests)
- `tests/testthat/helper-*.R` – Helper functions and custom expectations
- `tests/testthat/fixtures/` – Static test data files accessed via `test_path()`

## Test Structure

```r
test_that("descriptive behavior", {
  result <- my_function(input)
  expect_equal(result, expected_value)
})
```

Test descriptions should read naturally and describe behavior, not implementation.

## Running Tests

```r
# Run all tests
devtools::test()

# Run tests for a specific file
devtools::test(filter = "^utilities$")

# Run a single test
devtools::test_active_file("R/utilities.R", desc = "my test name")

# Review snapshot changes
testthat::snapshot_review()

# Accept snapshot changes
testthat::snapshot_accept()
```

## Expectations

### Errors and Warnings — Use Snapshots

Do **not** use `expect_error()` or `expect_warning()` alone. Use snapshots to capture full output:

```r
# Good
test_that("validation catches wrong type", {
  expect_snapshot(error = TRUE, myFunction("wrong_type"))
})

test_that("function warns about deprecated usage", {
  expect_snapshot(myFunction(old_arg = TRUE))
})

# Bad
test_that("validation catches wrong type", {
  expect_error(myFunction("wrong_type"))
})
```

### Avoid `expect_true()` / `expect_false()`

Prefer specific expectations for better failure messages:

```r
# Good
expect_equal(result, 42)
expect_null(result)
expect_s3_class(result, "data.frame")
expect_r6_class(result, "Scenario")

# Bad
expect_true(result == 42)
expect_true(is.null(result))
```

### Common Expectations

```r
expect_equal(result, expected)          # numeric tolerance by default
expect_identical(result, expected)      # exact match
expect_null(result)
expect_type(result, "list")
expect_s3_class(result, "data.frame")
expect_r6_class(result, "MyR6Class")
expect_length(result, 3)
expect_named(result, c("x", "y"))
expect_contains(result, "item")
expect_in("item", result)
```

## Design Principles

### Self-Sufficient Tests

Each test should contain all setup, execution, and teardown code:

```r
# Good: self-contained
test_that("Scenario initialises correctly", {
  config <- ScenarioConfiguration$new(...)
  scenario <- Scenario$new(config)
  expect_r6_class(scenario, "Scenario")
})

# Bad: relies on ambient state
config <- ScenarioConfiguration$new(...)
test_that("Scenario initialises correctly", {
  scenario <- Scenario$new(config)  # Where did 'config' come from?
  expect_r6_class(scenario, "Scenario")
})
```

### Cleanup Side Effects with `withr`

```r
test_that("function respects options", {
  withr::local_options(my_option = "test_value")
  withr::local_tempfile()  # auto-cleaned up after test

  result <- my_function()
  expect_equal(result$setting, "test_value")
})
```

**Common withr functions:**
- `local_options()` – Temporarily set options
- `local_envvar()` – Temporarily set environment variables
- `local_tempfile()` / `local_tempdir()` – Temp files with automatic cleanup

### Guard Tests that Require OSPSuite

Tests that require a running OSPSuite environment should be guarded:

```r
test_that("simulation runs", {
  skip_if_not(ospsuite:::.isOspsuiteAvailable(), "OSPSuite not available")
  # ... test code
})
```

## Snapshot Testing

For complex output (error messages, print methods, plots), use snapshot tests:

```r
test_that("print method shows key fields", {
  config <- ScenarioConfiguration$new(...)
  expect_snapshot(print(config))
})

test_that("error message is informative", {
  expect_snapshot(error = TRUE, Scenario$new(NULL))
})
```

Snapshots are stored in `tests/testthat/_snaps/`.

**Workflow:**
```r
devtools::test()                     # Creates new snapshots on first run
testthat::snapshot_review("name")    # Review changes interactively
testthat::snapshot_accept("name")    # Accept all changes
```

**Important:** Never accept new snapshots without manual review. If a CI build fails because of snapshot tests, review the new output before accepting.

## Test Fixtures and Data

```r
# Constructor functions — create test data on-demand
new_test_scenario_config <- function(...) {
  ScenarioConfiguration$new(...)
}

# Temporary files
test_that("reads Excel input", {
  temp_file <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Sheet1 = data.frame(x = 1)), temp_file)
  result <- readExcelInput(temp_file)
  expect_s3_class(result, "data.frame")
})

# Static fixtures — access via test_path()
data <- readRDS(test_path("fixtures", "my_data.rds"))
```

## testthat 3 Modernizations

When working with existing tests, prefer modern patterns:

**Deprecated → Modern:**
- `context()` → Remove (duplicates filename)
- `expect_equivalent()` → `expect_equal(ignore_attr = TRUE)`
- `with_mock()` → `local_mocked_bindings()`
