test_that("`sampleRandomValue()` generates needed distribution", {
  expect_error(
    sampleRandomValue("xyz", 5, 2, 10),
    messages$errorDistributionNotSupported("xyz")
  )

  set.seed(123)
  expect_equal(
    sampleRandomValue(Distributions$Normal, 5, 2, 10),
    c(
      3.87904870689558, 4.53964502103344, 8.11741662829825, 5.14101678284915,
      5.25857547032189, 8.43012997376656, 5.9218324119784, 2.46987753078693,
      3.62629429621295, 4.10867605980008
    ),
    tolerance = 0.001
  )

  set.seed(123)
  expect_equal(
    sampleRandomValue(Distributions$LogNormal, 5, 2, 10),
    c(
      3.74081271106427, 4.24843764475839, 8.46318202896501, 4.77021554349172,
      4.87946908411847, 8.98864517081978, 5.54444951200875, 2.85153959957418,
      3.56304555191325, 3.90999158989997
    ),
    tolerance = 0.001
  )
})

test_that("It creates population characteristics with ontogenies from excel", {
  excelPath <- testConfigurationsPath("Populations.xlsx")

  populationCharacteristics <- readPopulationCharacteristicsFromXLS(
    XLSpath = excelPath,
    populationName = "TestPopulation"
  )

  expect_equal(
    c(
      populationCharacteristics$species,
      populationCharacteristics$population,
      populationCharacteristics$numberOfIndividuals,
      populationCharacteristics$proportionOfFemales,
      populationCharacteristics$age$min,
      populationCharacteristics$age$max,
      c(
        populationCharacteristics$allMoleculeOntogenies[[1]]$molecule,
        populationCharacteristics$allMoleculeOntogenies[[2]]$molecule
      )
    ),
    c(
      ospsuite::Species$Human,
      ospsuite::HumanPopulation$European_ICRP_2002,
      2,
      0,
      22,
      41,
      c("CYP3A4", "CYP2D6")
    )
  )
})

test_that("It creates population characteristics without ontogenies from excel", {
  excelPath <- testConfigurationsPath("Populations.xlsx")

  populationCharachterstics <- readPopulationCharacteristicsFromXLS(
    XLSpath = excelPath,
    populationName = "TestPopulation_noOnto"
  )
  expect_equal(
    c(
      populationCharachterstics$species,
      populationCharachterstics$population,
      populationCharachterstics$numberOfIndividuals,
      populationCharachterstics$proportionOfFemales,
      populationCharachterstics$age$min,
      populationCharachterstics$age$max,
      populationCharachterstics$allMoleculeOntogenies
    ),
    c(
      ospsuite::Species$Human,
      ospsuite::HumanPopulation$European_ICRP_2002,
      2,
      0,
      22,
      41,
      NULL
    )
  )
})

test_that("extendPopulationByUserDefinedParams works", {
  set.seed(42)

  population <- ospsuite::loadPopulation(system.file("extdata", "SimResults_pop.csv", package = "ospsuite"))

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
        data = list("UserDefinedVariability" = data.frame(
          `Container Path` = "Organism|Kidney",
          `Parameter Name` = "GFR",
          "Mean" = 0.12,
          "SD" = 0.001,
          "Distribution" = "Normal",
          check.names = FALSE
        ))
      )

      population <- ospsuite::loadPopulation(system.file("extdata", "SimResults_pop.csv", package = "ospsuite"))

      set.seed(42)

      extendPopulationFromXLS(population,
        PopulationParameters,
        sheet = "UserDefinedVariability"
      )

      expect_snapshot(
        population$getParameterValuesForIndividual(4)
      )
    }
  )
})

test_that("extendPopulationFromXLS throws an error if the sheet has wrong structure", {
  withr::with_tempfile(
    new = "PopulationParameters",
    fileext = ".xlsx",
    code = {
      population <- ospsuite::loadPopulation(system.file("extdata", "SimResults_pop.csv", package = "ospsuite"))

      .writeExcel(
        path = PopulationParameters,
        data = list("UserDefinedVariability" = data.frame(
          `Container Path` = character(),
          `Parameter Name` = character(),
          "Mean" = numeric(),
          "SD" = numeric(),
          # "Distribution" = character(),  # Distribution column is missing
          check.names = FALSE
        ))
      )


      expect_error(
        extendPopulationFromXLS(population,
          PopulationParameters,
          sheet = "UserDefinedVariability"
        ),
        regexp = "has wrong structure"
      )

      .writeExcel(
        path = PopulationParameters,
        data = list("UserDefinedVariability" = data.frame(
          "Container.Path" = character(), # column name is wrong
          `Parameter Name` = character(),
          "Mean" = numeric(),
          "SD" = numeric(),
          "Distribution" = character(),
          check.names = FALSE
        ))
      )

      expect_error(
        extendPopulationFromXLS(population,
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
        data = list("UserDefinedVariability" = data.frame(
          `Container Path` = character(),
          `Parameter Name` = character(),
          "Mean" = numeric(),
          "SD" = numeric(),
          "Distribution" = character(), # Distribution column is missing
          check.names = FALSE
        ))
      )

      population <- ospsuite::loadPopulation(system.file("extdata", "SimResults_pop.csv", package = "ospsuite"))

      expect_error(
        extendPopulationFromXLS(population,
          PopulationParameters,
          sheet = "UserDefinedVariability"
        ),
        regexp = "does not contain any rows with data"
      )

      .writeExcel(
        path = PopulationParameters,
        data = list("UserDefinedVariability" = data.frame(
          `Container Path` = "Organism|Kidney",
          `Parameter Name` = "GFR",
          "Mean" = 0.12,
          "SD" = 0.001,
          "Distribution" = NA,
          check.names = FALSE
        ))
      )

      expect_snapshot(
        error = TRUE,
        extendPopulationFromXLS(population,
          PopulationParameters,
          sheet = "UserDefinedVariability"
        )
      )
    }
  )
})
