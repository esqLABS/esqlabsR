# Round-trip / parsing ----

test_that("individual inline parameters round-trip through saveProject/loadProject", {
  project <- Project$new()
  project$modelFolder <- tempdir()
  addIndividual(
    project,
    "RT",
    species = "Human",
    weight = 70,
    height = 175,
    age = 30,
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
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  pset <- reloaded$individuals[["RT"]]$parameters
  expect_equal(pset$paths, c("Organism|Liver|Volume", "Organism|Kidney|GFR"))
  expect_equal(pset$values, c(1.8, 90))
  expect_equal(pset$units, c("L", "ml/min"))
})

test_that("individual without parameters round-trips with parameters = NULL", {
  project <- Project$new()
  project$modelFolder <- tempdir()
  addIndividual(project, "RT_NoParam", species = "Human")
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  expect_null(reloaded$individuals[["RT_NoParam"]]$parameters)
})

test_that("loaded JSON with individual parameters: array gets parsed correctly", {
  json <- '{
    "schemaVersion": "2.0",
    "esqlabsRVersion": "6.0.0",
    "individuals": [{
      "individualId": "JsonI",
      "species": "Human",
      "parameters": [
        {"containerPath": "Organism|Liver", "parameterName": "Volume",
         "value": 1.8, "units": "L"}
      ]
    }]
  }'
  tmp <- tempfile(fileext = ".json")
  writeLines(json, tmp)
  project <- loadProject(tmp)
  pset <- project$individuals[["JsonI"]]$parameters
  expect_equal(pset$paths, "Organism|Liver|Volume")
  expect_equal(pset$values, 1.8)
  expect_equal(pset$units, "L")
})

test_that("saveProject produces round-trip fidelity for individuals", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(project$individuals), names(pc2$individuals))
  indiv1 <- project$individuals[["Indiv1"]]
  indiv2 <- pc2$individuals[["Indiv1"]]
  expect_equal(indiv1$species, indiv2$species)
  expect_equal(indiv1$weight, indiv2$weight)
})

# Public CRUD: individuals ----

test_that("addIndividual adds entry with required fields", {
  project <- testProject()
  initial <- length(project$individuals)
  addIndividual(project, "NewIndiv", species = "Human")
  expect_equal(length(project$individuals), initial + 1)
  expect_equal(project$individuals[["NewIndiv"]]$species, "Human")
  expect_true(project$modified)
})

test_that("addIndividual accepts optional fields via ...", {
  project <- testProject()
  addIndividual(
    project,
    "Indiv2",
    species = "Human",
    population = "European_ICRP_2002",
    gender = "Male",
    weight = 70,
    height = 175,
    age = 30
  )
  entry <- project$individuals[["Indiv2"]]
  expect_equal(entry$weight, 70)
  expect_equal(entry$height, 175)
  expect_equal(entry$age, 30)
  expect_equal(entry$gender, "Male")
})

test_that("addIndividual coerces numeric fields", {
  project <- testProject()
  addIndividual(project, "I3", species = "Human", weight = "70", height = "175")
  expect_identical(project$individuals[["I3"]]$weight, 70)
  expect_identical(project$individuals[["I3"]]$height, 175)
})

test_that("addIndividual errors on duplicate id", {
  project <- testProject()
  existing <- names(project$individuals)[[1]]
  expect_error(
    addIndividual(project, existing, species = "Human"),
    regexp = "already exists"
  )
})

test_that("addIndividual errors on unknown ... fields", {
  project <- testProject()
  expect_error(
    addIndividual(project, "IBad", species = "Human", banana = 1, kiwi = 2),
    regexp = "banana.*kiwi|kiwi.*banana"
  )
})

test_that("addIndividual errors on empty species", {
  project <- testProject()
  expect_error(
    addIndividual(project, "IEmpty", species = ""),
    regexp = "species"
  )
})

test_that("addIndividual returns project invisibly", {
  project <- testProject()
  out <- withVisible(addIndividual(project, "IRet", species = "Human"))
  expect_false(out$visible)
  expect_identical(out$value, project)
})

test_that("removeIndividual removes entry and sets modified", {
  project <- testProject()
  addIndividual(project, "ToGo", species = "Human")
  project$.markSaved()
  removeIndividual(project, "ToGo")
  expect_false("ToGo" %in% names(project$individuals))
  expect_true(project$modified)
})

test_that("removeIndividual warns on missing id and does not change project", {
  project <- testProject()
  before <- project$individuals
  expect_warning(
    removeIndividual(project, "NoSuchIndiv_QQ"),
    regexp = "not found"
  )
  expect_identical(project$individuals, before)
})

test_that("removeIndividual warns when referenced by a scenario", {
  project <- testProject()
  # Indiv1 is referenced by scenarios in TestProject
  expect_warning(
    removeIndividual(project, "Indiv1"),
    regexp = "referenced"
  )
  expect_false("Indiv1" %in% names(project$individuals))
})

test_that("project$addIndividual delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addIndividual(pc1, "Method", species = "Human", weight = 60)
  pc2$addIndividual("Method", species = "Human", weight = 60)
  expect_equal(pc1$individuals[["Method"]], pc2$individuals[["Method"]])
})

test_that("addIndividual survives saveProject/loadProject round-trip", {
  project <- testProject()
  addIndividual(
    project,
    "RTrip",
    species = "Human",
    weight = 65,
    height = 170,
    age = 40
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  entry <- reloaded$individuals[["RTrip"]]
  expect_equal(entry$species, "Human")
  expect_equal(entry$weight, 65)
  expect_equal(entry$height, 170)
  expect_equal(entry$age, 40)
})

test_that("addIndividualParameter adds an entry to a named individual", {
  project <- testProject()
  addIndividual(project, "I_param", species = "Human")
  project$.markSaved()
  addIndividualParameter(
    project,
    "I_param",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  expect_equal(
    project$individuals[["I_param"]]$parameters$paths,
    "Organism|Liver|Volume"
  )
  expect_equal(project$individuals[["I_param"]]$parameters$values, 1.8)
  expect_true(project$modified)
})

test_that("addIndividualParameter errors on unknown individualId", {
  project <- testProject()
  expect_error(
    addIndividualParameter(
      project,
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
  project <- testProject()
  addIndividual(project, "I_class", species = "Human")
  addIndividualParameter(
    project,
    "I_class",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  expect_true(inherits(project$individuals[["I_class"]], "Individual"))
})

test_that("removeIndividualParameter drops the entry", {
  project <- testProject()
  addIndividual(project, "I_rm", species = "Human")
  addIndividualParameter(
    project,
    "I_rm",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  project$.markSaved()
  removeIndividualParameter(
    project,
    "I_rm",
    containerPath = "Organism|Liver",
    parameterName = "Volume"
  )
  expect_null(project$individuals[["I_rm"]]$parameters)
  expect_true(project$modified)
})

test_that("removeIndividualParameter warns when entry not found", {
  project <- testProject()
  addIndividual(project, "I_nf", species = "Human")
  expect_warning(
    removeIndividualParameter(
      project,
      "I_nf",
      containerPath = "Organism|Liver",
      parameterName = "Volume"
    ),
    regexp = "not found"
  )
})

test_that("addIndividual accepts an inline parameters arg", {
  project <- testProject()
  addIndividual(
    project,
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
  pset <- project$individuals[["I_inline"]]$parameters
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
