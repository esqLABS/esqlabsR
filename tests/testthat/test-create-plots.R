test_that(".parseExcelMultiValueField numeric conversion path is covered", {
  result <- esqlabsR:::.parseExcelMultiValueField(
    value = "72.5, 80.5",
    fieldName = "test",
    plotID = "P1",
    expectedLength = 2,
    expectedType = "numeric"
  )
  expect_equal(result, c(72.5, 80.5))
  expect_true(is.numeric(result))

  expect_error(
    esqlabsR:::.parseExcelMultiValueField(
      value = "72 80",
      fieldName = "test",
      plotID = "P1",
      expectedLength = 2,
      expectedType = "numeric"
    ),
    regexp = "Invalid format.*Expected.*Values separated by commas",
    fixed = FALSE
  )
})

# createPlots(project, ...) tests ----

test_that("createPlots errors on non-Project input", {
  expect_error(createPlots("not a project"), "expected <Project>")
})

test_that("createPlots returns empty list when project has no plots", {
  project <- testProject()
  expect_identical(createPlots(project), list())
})

test_that("createPlots builds plot grids for Example project", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarioNames = "Aciclovir_iv")
  gridName <- project$plots$plotGrids$name[[1]]

  result <- createPlots(
    project,
    plotGridNames = gridName,
    simulatedScenarios = simulated
  )

  expect_named(result, gridName)
  expect_s3_class(result[[gridName]], "patchwork")
})
