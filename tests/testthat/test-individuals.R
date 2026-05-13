XLSpath <- getTestDataFilePath("Individuals.xlsx")

test_that("It returns NULL if the specified individual Id cannot be found in
          the file and nullIfNotFound is TRUE", {
  individualId <- "notPresent"

  expect_null(readIndividualCharacteristicsFromXLS(
    XLSpath = XLSpath,
    individualId = individualId
  ))
})

test_that("It throws an error if the specified individual Id cannot be found in
          the file and nullIfNotFound is FALSE", {
  individualId <- "notPresent"

  expect_error(
    readIndividualCharacteristicsFromXLS(
      XLSpath = XLSpath,
      individualId = individualId,
      nullIfNotFound = FALSE
    ),
    messages$errorWrongIndividualId(individualId)
  )
})

test_that("It create IndividualCharacteristics with the correct values", {
  individualId <- "Vicini_1999"
  individualCharacteristics <- readIndividualCharacteristicsFromXLS(
    XLSpath = XLSpath,
    individualId = individualId
  )
  expect_equal(individualCharacteristics$species, "Human")
  expect_equal(individualCharacteristics$population, "European_ICRP_2002")
  expect_equal(individualCharacteristics$gender, "MALE")
  expect_equal(individualCharacteristics$weight$value, 62)
  expect_equal(individualCharacteristics$height$value, 167)
  expect_equal(individualCharacteristics$age$value, 27)
})

test_that("It create IndividualCharacteristics when numerical values are empty", {
  individualId <- "Individual_with_NAs"
  individualCharacteristics <- readIndividualCharacteristicsFromXLS(
    XLSpath = XLSpath,
    individualId = individualId
  )
  expect_equal(individualCharacteristics$species, "Human")
  expect_equal(individualCharacteristics$population, "European_ICRP_2002")
  expect_equal(individualCharacteristics$gender, "MALE")
})

test_that("`.readIndividualParameterSetsFromXLS()` returns NULL if individual is not found", {
  expect_null(esqlabsR:::.readIndividualParameterSetsFromXLS(
    XLSpath = XLSpath,
    individualId = "notPresent",
    scenarioName = "TestScenario"
  ))
})

test_that("`.readIndividualParameterSetsFromXLS()` returns empty params when 'Individual Parameter Sets' is empty", {
  result <- esqlabsR:::.readIndividualParameterSetsFromXLS(
    XLSpath = XLSpath,
    individualId = "Vicini_1999",
    scenarioName = "TestScenario"
  )
  expect_length(result$paths, 0)
  expect_length(result$values, 0)
  expect_length(result$units, 0)
})

test_that("`.readIndividualParameterSetsFromXLS()` errors for missing sheets", {
  expect_error(
    esqlabsR:::.readIndividualParameterSetsFromXLS(
      XLSpath = XLSpath,
      individualId = "Individual_with_param_sets",
      scenarioName = "TestScenario"
    ),
    regexp = messages$errorIndividualParameterSetNotFound(
      "TestScenario",
      "ParamSet2"
    )
  )
})


test_that("`writeIndividualToXLS()` writes correct data to a spreadsheet", {
  withr::with_tempdir(
    code = {
      humanIndividualCharacteristics <- createIndividualCharacteristics(
        species = Species$Human,
        population = HumanPopulation$European_ICRP_2002,
        gender = Gender$Male,
        weight = 70
      )
      tmp <- writeIndividualToXLS(
        humanIndividualCharacteristics,
        "ParameterSet.xlsx"
      )
      df <- readxl::read_xlsx(tmp)

      expect_equal(dim(df), c(96L, 4L))
      expect_equal(
        colnames(df),
        c("Container Path", "Parameter Name", "Value", "Units")
      )
      expect_equal(
        unique(df$Units),
        c(
          "year(s)",
          "week(s)",
          "dm",
          NA,
          "kg",
          "l",
          "l/min/kg organ",
          "l/min",
          "min"
        )
      )
    }
  )
})
