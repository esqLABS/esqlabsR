test_that("`sampleRandomValue()` rejects an unsupported distribution", {
  expect_error(
    sampleRandomValue("xyz", 5, 2, 10),
    messages$errorDistributionNotSupported("xyz")
  )
})

test_that("`sampleRandomValue()` returns the requested number of values", {
  withr::local_seed(123)
  expect_length(sampleRandomValue(Distributions$Normal, 5, 2, 10), 10)
  expect_length(sampleRandomValue(Distributions$LogNormal, 5, 2, 10), 10)
})

# The exact draws are coupled to R's RNG stream (fragile across R versions), so
# these assert the sampler's statistical shape instead: over a large sample the
# mean and sd must match the requested target moments (both distributions are
# parameterized so their realized mean is `mean` and sd is `sd`), within a
# generous tolerance that still catches a broken sampler.
test_that("`sampleRandomValue()` Normal draws match the target mean and sd", {
  withr::local_seed(123)
  mean <- 5
  sd <- 2
  values <- sampleRandomValue(Distributions$Normal, mean, sd, 1e5)

  expect_equal(base::mean(values), mean, tolerance = 0.05)
  expect_equal(stats::sd(values), sd, tolerance = 0.05)
})

test_that("`sampleRandomValue()` LogNormal draws are positive and match target moments", {
  withr::local_seed(123)
  mean <- 5
  sd <- 2
  values <- sampleRandomValue(Distributions$LogNormal, mean, sd, 1e5)

  expect_true(all(values > 0))
  expect_equal(base::mean(values), mean, tolerance = 0.05)
  expect_equal(stats::sd(values), sd, tolerance = 0.05)
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

test_that("setPopulation clears a numeric field passed NULL", {
  # A NULL clears (removes) the optional field: the key must be ABSENT, not
  # present as numeric(0). The testpopulation fixture carries ageMin, so its
  # removal is observable.
  project <- testProject()
  before <- project$populations[["testpopulation"]]
  setPopulation(project, "testpopulation", ageMin = NULL)

  after <- project$populations[["testpopulation"]]
  expect_false("ageMin" %in% names(after))
  expect_null(after$ageMin)
  # No other field changed, and no unexpected key was added.
  expect_setequal(names(after), setdiff(names(before), "ageMin"))
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

test_that("setPopulation rejects a non-numeric range field", {
  project <- testProject()
  before <- project$populations[["testpopulation"]]
  # "heavy" would silently coerce to NA via as.double(); it must abort instead,
  # mirroring the numeric-field guard on the individual set path.
  expect_snapshot(
    error = TRUE,
    setPopulation(project, "testpopulation", weightMin = "heavy")
  )
  expect_equal(project$populations[["testpopulation"]], before)
})

test_that("setPopulation rejects a non-integer numberOfIndividuals", {
  project <- testProject()
  before <- project$populations[["testpopulation"]]
  # 2.5 would be stored as-is; the set path must reject it the same way the
  # add path does.
  expect_snapshot(
    error = TRUE,
    setPopulation(project, "testpopulation", numberOfIndividuals = 2.5)
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
