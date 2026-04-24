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

      population <- ospsuite::loadPopulation(system.file(
        "extdata",
        "pop.csv",
        package = "ospsuite"
      ))

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

test_that("addPopulation adds entry with required fields", {
  pc <- testProject()
  initial <- length(pc$populations)
  addPopulation(pc, "NewPop", species = "Human", numberOfIndividuals = 10)
  expect_equal(length(pc$populations), initial + 1)
  entry <- pc$populations[["NewPop"]]
  expect_equal(entry$species, "Human")
  expect_equal(entry$numberOfIndividuals, 10)
  expect_true(pc$modified)
})

test_that("addPopulation accepts range fields via ...", {
  pc <- testProject()
  addPopulation(
    pc,
    "Pop2",
    species = "Human",
    numberOfIndividuals = 20,
    weightMin = 50,
    weightMax = 90,
    ageMin = 18,
    ageMax = 65,
    proportionOfFemales = 50
  )
  entry <- pc$populations[["Pop2"]]
  expect_equal(entry$weightMin, 50)
  expect_equal(entry$ageMax, 65)
  expect_equal(entry$proportionOfFemales, 50)
})

test_that("addPopulation errors on duplicate id", {
  pc <- testProject()
  existing <- names(pc$populations)[[1]]
  expect_error(
    addPopulation(pc, existing, species = "Human", numberOfIndividuals = 5),
    regexp = "already exists"
  )
})

test_that("addPopulation errors on unknown ... field", {
  pc <- testProject()
  expect_error(
    addPopulation(
      pc,
      "BadPop",
      species = "Human",
      numberOfIndividuals = 10,
      mango = 42
    ),
    regexp = "mango"
  )
})

test_that("addPopulation errors on non-positive numberOfIndividuals", {
  pc <- testProject()
  expect_error(
    addPopulation(pc, "Zero", species = "Human", numberOfIndividuals = 0),
    regexp = "numberOfIndividuals"
  )
})

test_that("removePopulation removes entry and sets modified", {
  pc <- testProject()
  addPopulation(pc, "Gone", species = "Human", numberOfIndividuals = 5)
  pc$modified <- FALSE
  removePopulation(pc, "Gone")
  expect_false("Gone" %in% names(pc$populations))
  expect_true(pc$modified)
})

test_that("removePopulation warns on missing id", {
  pc <- testProject()
  expect_warning(
    removePopulation(pc, "NoSuchPop_QQQ"),
    regexp = "not found"
  )
})

test_that("project$addPopulation delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addPopulation(pc1, "Via1", species = "Human", numberOfIndividuals = 8)
  pc2$addPopulation("Via1", species = "Human", numberOfIndividuals = 8)
  expect_equal(pc1$populations[["Via1"]], pc2$populations[["Via1"]])
})

test_that("addPopulation survives round-trip", {
  pc <- testProject()
  addPopulation(
    pc,
    "RT",
    species = "Human",
    numberOfIndividuals = 10,
    weightMin = 50,
    weightMax = 90
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(pc, tmp)
  reloaded <- loadProject(tmp)
  entry <- reloaded$populations[["RT"]]
  expect_equal(entry$numberOfIndividuals, 10)
  expect_equal(entry$weightMin, 50)
})
