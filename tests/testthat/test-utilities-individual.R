test_that("addIndividual adds entry with required fields", {
  pc <- testProject()
  initial <- length(pc$individuals)
  addIndividual(pc, "NewIndiv", species = "Human")
  expect_equal(length(pc$individuals), initial + 1)
  expect_equal(pc$individuals[["NewIndiv"]]$species, "Human")
  expect_true(pc$modified)
})

test_that("addIndividual accepts optional fields via ...", {
  pc <- testProject()
  addIndividual(
    pc,
    "Indiv2",
    species = "Human",
    population = "European_ICRP_2002",
    gender = "Male",
    weight = 70,
    height = 175,
    age = 30
  )
  entry <- pc$individuals[["Indiv2"]]
  expect_equal(entry$weight, 70)
  expect_equal(entry$height, 175)
  expect_equal(entry$age, 30)
  expect_equal(entry$gender, "Male")
})

test_that("addIndividual coerces numeric fields", {
  pc <- testProject()
  addIndividual(pc, "I3", species = "Human", weight = "70", height = "175")
  expect_identical(pc$individuals[["I3"]]$weight, 70)
  expect_identical(pc$individuals[["I3"]]$height, 175)
})

test_that("addIndividual errors on duplicate id", {
  pc <- testProject()
  existing <- names(pc$individuals)[[1]]
  expect_error(
    addIndividual(pc, existing, species = "Human"),
    regexp = "already exists"
  )
})

test_that("addIndividual errors on unknown ... fields", {
  pc <- testProject()
  expect_error(
    addIndividual(pc, "IBad", species = "Human", banana = 1, kiwi = 2),
    regexp = "banana.*kiwi|kiwi.*banana"
  )
})

test_that("addIndividual errors on empty species", {
  pc <- testProject()
  expect_error(addIndividual(pc, "IEmpty", species = ""), regexp = "species")
})

test_that("addIndividual returns project invisibly", {
  pc <- testProject()
  out <- withVisible(addIndividual(pc, "IRet", species = "Human"))
  expect_false(out$visible)
  expect_identical(out$value, pc)
})

test_that("removeIndividual removes entry and sets modified", {
  pc <- testProject()
  addIndividual(pc, "ToGo", species = "Human")
  pc$modified <- FALSE
  removeIndividual(pc, "ToGo")
  expect_false("ToGo" %in% names(pc$individuals))
  expect_true(pc$modified)
})

test_that("removeIndividual warns on missing id and does not change project", {
  pc <- testProject()
  before <- pc$individuals
  expect_warning(
    removeIndividual(pc, "NoSuchIndiv_QQ"),
    regexp = "not found"
  )
  expect_identical(pc$individuals, before)
})

test_that("removeIndividual warns when referenced by a scenario", {
  pc <- testProject()
  # Indiv1 is referenced by scenarios in TestProject
  expect_warning(
    removeIndividual(pc, "Indiv1"),
    regexp = "referenced"
  )
  expect_false("Indiv1" %in% names(pc$individuals))
})

test_that("project$addIndividual delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addIndividual(pc1, "Method", species = "Human", weight = 60)
  pc2$addIndividual("Method", species = "Human", weight = 60)
  expect_equal(pc1$individuals[["Method"]], pc2$individuals[["Method"]])
})

test_that("addIndividual survives saveProject/loadProject round-trip", {
  pc <- testProject()
  addIndividual(
    pc,
    "RTrip",
    species = "Human",
    weight = 65,
    height = 170,
    age = 40
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(pc, tmp)
  reloaded <- loadProject(tmp)
  entry <- reloaded$individuals[["RTrip"]]
  expect_equal(entry$species, "Human")
  expect_equal(entry$weight, 65)
  expect_equal(entry$height, 170)
  expect_equal(entry$age, 40)
})

test_that("addIndividualParameter adds an entry to a named individual", {
  pc <- testProject()
  addIndividual(pc, "I_param", species = "Human")
  pc$modified <- FALSE
  addIndividualParameter(
    pc,
    "I_param",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  expect_equal(
    pc$individuals[["I_param"]]$parameters$paths,
    "Organism|Liver|Volume"
  )
  expect_equal(pc$individuals[["I_param"]]$parameters$values, 1.8)
  expect_true(pc$modified)
})

test_that("addIndividualParameter errors on unknown individualId", {
  pc <- testProject()
  expect_error(
    addIndividualParameter(
      pc,
      "NoSuchIndiv_QQ",
      containerPath = "a",
      parameterName = "x",
      value = 1,
      units = "L"
    ),
    regexp = "not found|does not exist"
  )
})

test_that("addIndividualParameter preserves S3 class on the individual", {
  pc <- testProject()
  addIndividual(pc, "I_class", species = "Human")
  addIndividualParameter(
    pc,
    "I_class",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  expect_true(inherits(pc$individuals[["I_class"]], "Individual"))
})

test_that("removeIndividualParameter drops the entry", {
  pc <- testProject()
  addIndividual(pc, "I_rm", species = "Human")
  addIndividualParameter(
    pc,
    "I_rm",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  pc$modified <- FALSE
  removeIndividualParameter(
    pc,
    "I_rm",
    containerPath = "Organism|Liver",
    parameterName = "Volume"
  )
  expect_null(pc$individuals[["I_rm"]]$parameters)
  expect_true(pc$modified)
})

test_that("removeIndividualParameter warns when entry not found", {
  pc <- testProject()
  addIndividual(pc, "I_nf", species = "Human")
  expect_warning(
    removeIndividualParameter(
      pc,
      "I_nf",
      containerPath = "Organism|Liver",
      parameterName = "Volume"
    ),
    regexp = "not found"
  )
})

test_that("addIndividual accepts an inline parameters arg", {
  pc <- testProject()
  addIndividual(
    pc,
    "I_inline",
    species = "Human",
    parameters = list(
      list(
        containerPath = "Organism|Liver",
        parameterName = "Volume",
        value = 1.8,
        units = "L"
      ),
      list(
        containerPath = "Organism|Kidney",
        parameterName = "GFR",
        value = 90,
        units = "ml/min"
      )
    )
  )
  pset <- pc$individuals[["I_inline"]]$parameters
  expect_equal(pset$paths, c("Organism|Liver|Volume", "Organism|Kidney|GFR"))
  expect_equal(pset$values, c(1.8, 90))
})

test_that("project$addIndividualParameter delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addIndividual(pc1, "X", species = "Human")
  addIndividual(pc2, "X", species = "Human")
  addIndividualParameter(
    pc1,
    "X",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  pc2$addIndividualParameter(
    "X",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  expect_equal(
    pc1$individuals[["X"]]$parameters,
    pc2$individuals[["X"]]$parameters
  )
})
