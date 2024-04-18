## context("sampleRandomValue")

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
  excelPath <-  testConfigurationsPath("Populations.xlsx")

  populationCharachterstics <- readPopulationCharacteristicsFromXLS(
    XLSpath = excelPath,
    populationName = "TestPopulation"
  )

  expect_equal(
    c(
      populationCharachterstics$species,
      populationCharachterstics$population,
      populationCharachterstics$numberOfIndividuals,
      populationCharachterstics$proportionOfFemales,
      populationCharachterstics$age$min,
      populationCharachterstics$age$max,
      c(
        populationCharachterstics$allMoleculeOntogenies[[1]]$molecule,
        populationCharachterstics$allMoleculeOntogenies[[2]]$molecule
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
