test_that("`sampleRandomValue()` generates needed distribution", {
  expect_error(
    sampleRandomValue("xyz", 5, 2, 10),
    messages$errorDistributionNotSupported("xyz")
  )

  set.seed(123)
  expect_equal(
    sampleRandomValue(Distributions$Normal, 5, 2, 10),
    c(
      3.87904870689558,
      4.53964502103344,
      8.11741662829825,
      5.14101678284915,
      5.25857547032189,
      8.43012997376656,
      5.9218324119784,
      2.46987753078693,
      3.62629429621295,
      4.10867605980008
    ),
    tolerance = 0.001
  )

  set.seed(123)
  expect_equal(
    sampleRandomValue(Distributions$LogNormal, 5, 2, 10),
    c(
      3.74081271106427,
      4.24843764475839,
      8.46318202896501,
      4.77021554349172,
      4.87946908411847,
      8.98864517081978,
      5.54444951200875,
      2.85153959957418,
      3.56304555191325,
      3.90999158989997
    ),
    tolerance = 0.001
  )
})

test_that("extendPopulationByUserDefinedParams works", {
  set.seed(42)

  population <- ospsuite::loadPopulation(system.file(
    "extdata",
    "pop.csv",
    package = "ospsuite"
  ))

  esqlabsR::extendPopulationByUserDefinedParams(
    population = population,
    parameterPaths = c("Organism|Kidney|GFR"),
    meanValues = 0.12,
    sdValues = 0.001,
    distributions = "Normal"
  )

  expect_snapshot(
    population$getParameterValuesForIndividual(4)
  )
})

test_that("extendPopulationFromXLS works", {
  withr::with_tempfile(
    new = "PopulationParameters",
    fileext = ".xlsx",
    code = {
      .writeExcel(
        path = PopulationParameters,
        data = list(
          "UserDefinedVariability" = data.frame(
            `Container Path` = c("Organism|Kidney", "Organism|Kidney"),
            `Parameter Name` = c("GFR", "eGFR"),
            "Mean" = 0.12,
            "SD" = 0.001,
            "Distribution" = "Normal",
            check.names = FALSE
          )
        )
      )

      population <- ospsuite::loadPopulation(
        system.file(
          "extdata",
          "pop.csv",
          package = "ospsuite"
        )
      )

      set.seed(42)
      extendPopulationFromXLS(
        population,
        PopulationParameters,
        sheet = "UserDefinedVariability"
      )
      expect_snapshot(
        population$getParameterValuesForIndividual(4)
      )
      expect_true(all(
        c("Organism|Kidney|GFR", "Organism|Kidney|eGFR") %in%
          population$allParameterPaths
      ))
    }
  )
})

test_that("extendPopulationFromXLS throws an error if the sheet has wrong structure", {
  withr::with_tempfile(
    new = "PopulationParameters",
    fileext = ".xlsx",
    code = {
      population <- ospsuite::loadPopulation(system.file(
        "extdata",
        "pop.csv",
        package = "ospsuite"
      ))

      .writeExcel(
        path = PopulationParameters,
        data = list(
          "UserDefinedVariability" = data.frame(
            `Container Path` = character(),
            `Parameter Name` = character(),
            "Mean" = numeric(),
            "SD" = numeric(),
            # "Distribution" = character(),  # Distribution column is missing
            check.names = FALSE
          )
        )
      )

      expect_error(
        extendPopulationFromXLS(
          population,
          PopulationParameters,
          sheet = "UserDefinedVariability"
        ),
        regexp = "has wrong structure"
      )

      .writeExcel(
        path = PopulationParameters,
        data = list(
          "UserDefinedVariability" = data.frame(
            "Container.Path" = character(), # column name is wrong
            `Parameter Name` = character(),
            "Mean" = numeric(),
            "SD" = numeric(),
            "Distribution" = character(),
            check.names = FALSE
          )
        )
      )

      expect_error(
        extendPopulationFromXLS(
          population,
          PopulationParameters,
          sheet = "UserDefinedVariability"
        ),
        regexp = "has wrong structure"
      )
    }
  )
})

test_that("extendPopulationFromXLS throws an error if specified sheet is empty or data is missing", {
  withr::with_tempfile(
    new = "PopulationParameters",
    fileext = ".xlsx",
    code = {
      .writeExcel(
        path = PopulationParameters,
        data = list(
          "UserDefinedVariability" = data.frame(
            `Container Path` = character(),
            `Parameter Name` = character(),
            "Mean" = numeric(),
            "SD" = numeric(),
            "Distribution" = character(), # Distribution column is missing
            check.names = FALSE
          )
        )
      )

      population <- ospsuite::loadPopulation(system.file(
        "extdata",
        "pop.csv",
        package = "ospsuite"
      ))

      expect_error(
        extendPopulationFromXLS(
          population,
          PopulationParameters,
          sheet = "UserDefinedVariability"
        ),
        regexp = "does not contain any rows with data"
      )

      .writeExcel(
        path = PopulationParameters,
        data = list(
          "UserDefinedVariability" = data.frame(
            `Container Path` = "Organism|Kidney",
            `Parameter Name` = "GFR",
            "Mean" = 0.12,
            "SD" = 0.001,
            "Distribution" = NA,
            check.names = FALSE
          )
        )
      )

      expect_snapshot(
        error = TRUE,
        extendPopulationFromXLS(
          population,
          PopulationParameters,
          sheet = "UserDefinedVariability"
        )
      )
    }
  )
})

# setPopulation ----

test_that("setPopulation changes a field and persists to file and memory", {
  project <- testProject()
  setPopulation(project, "testpopulation", numberOfIndividuals = 50)
  expect_equal(project$populations[["testpopulation"]]$numberOfIndividuals, 50)

  # The write-through must reach disk: a throwaway reload sees the new value.
  reloaded <- loadProject(project$jsonPath)
  expect_equal(
    reloaded$populations[["testpopulation"]]$numberOfIndividuals,
    50
  )
})

test_that("setPopulation coerces numeric fields like addPopulation", {
  project <- testProject()
  setPopulation(project, "testpopulation", proportionOfFemales = "75")
  expect_identical(
    project$populations[["testpopulation"]]$proportionOfFemales,
    75
  )
})

test_that("setPopulation partial update leaves other fields untouched", {
  project <- testProject()
  before <- project$populations[["testpopulation"]]
  setPopulation(project, "testpopulation", ageMin = 20)

  after <- project$populations[["testpopulation"]]
  expect_equal(after$ageMin, 20)
  for (f in setdiff(names(before), "ageMin")) {
    expect_equal(after[[f]], before[[f]])
  }
})

test_that("setPopulation clears validatedSinceMutation", {
  project <- testProject()
  project$.markValidated()
  setPopulation(project, "testpopulation", numberOfIndividuals = 10)
  expect_false(project$validatedSinceMutation)
})

test_that("setPopulation aborts on a non-existent population", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    setPopulation(project, "Ghost", numberOfIndividuals = 10)
  )
})

test_that("setPopulation rejects a non-positive numberOfIndividuals", {
  project <- testProject()
  before <- project$populations[["testpopulation"]]
  expect_snapshot(
    error = TRUE,
    setPopulation(project, "testpopulation", numberOfIndividuals = 0)
  )
  expect_equal(project$populations[["testpopulation"]], before)
})

test_that("addPopulation rejects a non-integer numberOfIndividuals", {
  project <- testProject()
  expect_error(
    addPopulation(
      project,
      "frac",
      species = "Human",
      numberOfIndividuals = 2.5
    ),
    "whole number"
  )
  expect_false("frac" %in% names(project$populations))
})

test_that("setPopulation on a clone does not affect the source on disk", {
  source <- testProject()
  before <- source$populations[["testpopulation"]]
  clone <- source$clone()
  setPopulation(clone, "testpopulation", numberOfIndividuals = 7)

  expect_equal(clone$populations[["testpopulation"]]$numberOfIndividuals, 7)
  expect_equal(source$populations[["testpopulation"]], before)
  # The clone's edit must not reach the source's on-disk tree.
  reloaded <- loadProject(source$jsonPath)
  expect_equal(reloaded$populations[["testpopulation"]], before)
})

# Vectorized authoring ----

test_that("addPopulation adds N populations in one call equal to N scalar adds", {
  vectorized <- testProject()
  addPopulation(
    vectorized,
    c("young", "old"),
    species = "Human",
    numberOfIndividuals = c(10, 20),
    ageMin = c(18, 65),
    ageMax = c(40, 90)
  )

  scalar <- testProject()
  addPopulation(
    scalar,
    "young",
    species = "Human",
    numberOfIndividuals = 10,
    ageMin = 18,
    ageMax = 40
  )
  addPopulation(
    scalar,
    "old",
    species = "Human",
    numberOfIndividuals = 20,
    ageMin = 65,
    ageMax = 90
  )

  expect_identical(
    vectorized$populations[c("young", "old")],
    scalar$populations[c("young", "old")]
  )
})

test_that("addPopulation recycles a scalar field and aligns a length-N field", {
  project <- testProject()
  addPopulation(
    project,
    c("a", "b"),
    species = "Human",
    numberOfIndividuals = c(5, 7)
  )
  expect_identical(project$populations$a$species, "Human")
  expect_identical(project$populations$b$species, "Human")
  expect_identical(project$populations$a$numberOfIndividuals, 5)
  expect_identical(project$populations$b$numberOfIndividuals, 7)
})

test_that("addPopulation persists all N to disk in one write-through", {
  project <- testProject()
  addPopulation(
    project,
    c("a", "b"),
    species = "Human",
    numberOfIndividuals = 5
  )
  reloaded <- loadProject(project$jsonPath)
  expect_true(all(c("a", "b") %in% names(reloaded$populations)))
})

test_that("addPopulation aborts the whole batch and writes nothing on one bad entry", {
  project <- testProject()
  before <- names(project$populations)
  expect_error(
    addPopulation(
      project,
      c("a", "b"),
      species = "Human",
      numberOfIndividuals = c(5, -1)
    )
  )
  expect_identical(names(project$populations), before)
  reloaded <- loadProject(project$jsonPath)
  expect_identical(names(reloaded$populations), before)
})

test_that("addPopulation aborts on a mismatched scalar field length", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addPopulation(
      project,
      c("a", "b", "c"),
      species = "Human",
      numberOfIndividuals = c(5, 7)
    )
  )
})

test_that("addPopulation aborts on a duplicate id in the batch", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addPopulation(
      project,
      c("a", "a"),
      species = "Human",
      numberOfIndividuals = 5
    )
  )
})

test_that("setPopulation vectorizes a partial update across N ids", {
  project <- testProject()
  addPopulation(
    project,
    c("a", "b"),
    species = "Human",
    numberOfIndividuals = 5
  )
  setPopulation(project, c("a", "b"), numberOfIndividuals = c(50, 60))
  expect_identical(project$populations$a$numberOfIndividuals, 50)
  expect_identical(project$populations$b$numberOfIndividuals, 60)
  expect_identical(project$populations$a$species, "Human")
})

test_that("removePopulation removes a vector of ids in one write-through", {
  project <- testProject()
  addPopulation(
    project,
    c("a", "b"),
    species = "Human",
    numberOfIndividuals = 5
  )
  removePopulation(project, c("a", "b"))
  expect_false(any(c("a", "b") %in% names(project$populations)))
  reloaded <- loadProject(project$jsonPath)
  expect_false(any(c("a", "b") %in% names(reloaded$populations)))
})

test_that("removePopulation warns when still referenced by a scenario, removes anyway", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  # `testpopulation` is the `populationId` of two scenarios in the fixture.
  expect_snapshot(removePopulation(project, "testpopulation"))
  expect_false("testpopulation" %in% names(project$populations))
})

# Print method ----

test_that("print.Population renders the configured fields", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$populations[["testpopulation"]]))
})

test_that("print.Population renders a minimal population", {
  project <- testProject()
  addPopulation(project, "minimal", species = "Human", numberOfIndividuals = 10)
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$populations[["minimal"]]))
})
