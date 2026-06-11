pkmlFixture <- test_path(
  "data",
  "TestProject",
  "Models",
  "Simulations",
  "Aciclovir.pkml"
)

test_that("createScenariosFromPKML errors on non-Project input", {
  skip_if_not(file.exists(pkmlFixture))
  expect_error(
    createScenariosFromPKML(pkmlFixture, project = "not a project"),
    "Project"
  )
})

test_that("createScenariosFromPKML returns empty list when no PKML files provided", {
  project <- testProject()
  result <- createScenariosFromPKML(character(), project)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("createScenariosFromPKML returns a named list of Scenario objects", {
  skip_if_not(file.exists(pkmlFixture))
  project <- testProject()
  scenarios <- createScenariosFromPKML(
    pkmlFixture,
    project = project,
    scenarioNames = "Test1"
  )
  expect_type(scenarios, "list")
  expect_named(scenarios, "Test1")
  expect_s3_class(scenarios[["Test1"]], "Scenario")
})

test_that("paramSheets argument is soft-deprecated", {
  skip_if_not(file.exists(pkmlFixture))
  withr::local_options(lifecycle_verbosity = "warning")
  project <- testProject()
  expect_warning(
    createScenariosFromPKML(
      pkmlFixture,
      project = project,
      scenarioNames = "Test1",
      paramSheets = "anything"
    ),
    class = "lifecycle_warning_deprecated"
  )
})
