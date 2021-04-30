context("readIndividualCharacteristicsFromXLS")

test_that("It throws an error if the specified individual Id cannot be found in the file", {
  XLSpath <- "../data/IndividualPhysiology.xlsx"
  individualId <- "notPresent"

  expect_error(readIndividualCharacteristicsFromXLS(XLSpath = XLSpath,
                                                    individualId = individualId), messages$errorWrongIndividualId(individualId))
})

test_that("It create IndividualCharacteristics with the correct values", {
  XLSpath <- "../data/IndividualPhysiology.xlsx"
  individualId <- "Vicini_1999"
  individualCharacteristics <- readIndividualCharacteristicsFromXLS(XLSpath = XLSpath,
                                                                    individualId = individualId)

