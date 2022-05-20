## context("readIndividualCharacteristicsFromXLS")

test_that("It returns NULL if the specified individual Id cannot be found in
          the file and nullIfNotFound is TRUE", {
  XLSpath <- "../data/IndividualPhysiology.xlsx"
  individualId <- "notPresent"

  expect_null(readIndividualCharacteristicsFromXLS(
    XLSpath = XLSpath,
    individualId = individualId
  ))
})

test_that("It throws an error if the specified individual Id cannot be found in
          the file and nullIfNotFound is FALSE", {
  XLSpath <- "../data/IndividualPhysiology.xlsx"
  individualId <- "notPresent"

  expect_error(readIndividualCharacteristicsFromXLS(
    XLSpath = XLSpath,
    individualId = individualId,
    nullIfNotFound = FALSE
  ), messages$errorWrongIndividualId(individualId))
})

test_that("It create IndividualCharacteristics with the correct values", {
  XLSpath <- "../data/IndividualPhysiology.xlsx"
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
  XLSpath <- "../data/IndividualPhysiology.xlsx"
  individualId <- "Individual_with_NAs"
  individualCharacteristics <- readIndividualCharacteristicsFromXLS(
    XLSpath = XLSpath,
    individualId = individualId
  )
  expect_equal(individualCharacteristics$species, "Human")
  expect_equal(individualCharacteristics$population, "European_ICRP_2002")
  expect_equal(individualCharacteristics$gender, "MALE")
})
