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

      # ospsuite 13's refined absorption model adds parameters to the
      # individual: 117 rows, and a concentration unit among them.
      expect_equal(dim(df), c(117L, 4L))
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
          "min",
          "µmol/l"
        )
      )
    }
  )
})

# Species scaling ----

# Human example model, re-exported with PK-Sim 13
.loadExampleAciclovirSimulation <- function() {
  loadSimulation(
    file.path(
      .exampleDirectory("TestProject"),
      "Models",
      "Simulations",
      "Aciclovir.pkml"
    ),
    loadFromCache = FALSE
  )
}

test_that("`applyIndividualParameters()` scales a human model to a rat", {
  simulation <- .loadExampleAciclovirSimulation()
  ratCharacteristics <- createIndividualCharacteristics(species = Species$Rat)

  expect_no_warning(applyIndividualParameters(ratCharacteristics, simulation))

  # Expected values are those of a rat individual created in PK-Sim 13. They
  # cover species constants that `createIndividual()` does not return, the
  # colon bile salt concentration that is new in PK-Sim 13, lumen geometry
  # that is a formula in the human model, and the derived body weight.
  expectedValues <- c(
    "Organism|Liver|EHC continuous fraction" = 1,
    "Organism|Lumen|Stomach|Basal pH in fasted state" = 3.9,
    "Organism|Liver|Vf (neutral lipid)-PT" = 0.0138,
    "Organism|Kidney|Fraction vascular" = 0.105,
    "Organism|Bone|Allometric scale factor" = 0.75,
    "Organism|Muscle|Vf (water)-PT" = 0.756,
    "Organism|Lumen|Duodenum|Length" = 1,
    "Organism|Lumen|ColonAscendens|Bile Salt concentration" = 5000,
    "Organism|Liver|Volume" = 0.0103,
    "Organism|Weight" = 0.227777
  )
  values <- vapply(
    names(expectedValues),
    function(path) getParameter(path, simulation)$value,
    FUN.VALUE = numeric(1)
  )
  expect_equal(values, expectedValues, tolerance = 1e-6)
})

test_that("`applyIndividualParameters()` follows the body weight of a rat", {
  simulation <- .loadExampleAciclovirSimulation()
  ratCharacteristics <- createIndividualCharacteristics(
    species = Species$Rat,
    weight = 0.4
  )

  applyIndividualParameters(ratCharacteristics, simulation)

  # Organ volumes of a 0.4 kg rat created in PK-Sim 13; species constants do
  # not depend on the weight.
  expectedValues <- c(
    "Organism|Weight" = 0.4,
    "Organism|Liver|Volume" = 0.0180878666414959,
    "Organism|Kidney|Volume" = 0.0040390381820816,
    "Organism|Liver|Vf (neutral lipid)-PT" = 0.0138
  )
  values <- vapply(
    names(expectedValues),
    function(path) getParameter(path, simulation)$value,
    FUN.VALUE = numeric(1)
  )
  expect_equal(values, expectedValues, tolerance = 1e-6)
})

test_that("`applyIndividualParameters()` leaves the species constants of a human model", {
  simulation <- .loadExampleAciclovirSimulation()
  humanCharacteristics <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    gender = Gender$Male,
    weight = 73,
    height = 176,
    age = 30
  )

  applyIndividualParameters(humanCharacteristics, simulation)

  expect_equal(
    getParameter("Organism|Liver|Vf (neutral lipid)-PT", simulation)$value,
    0.0348
  )
  expect_equal(
    getParameter("Organism|Liver|EHC continuous fraction", simulation)$value,
    0
  )
  # Lumen geometry stays a formula of the body height
  expect_true(
    getParameter("Organism|Lumen|Duodenum|Length", simulation)$isFormula
  )
})

test_that("`applyIndividualParameters()` scales between two non-human species", {
  # A model already scaled to a rat and then scaled to a mouse must equal the
  # human model scaled to a mouse directly.
  simulationViaRat <- .loadExampleAciclovirSimulation()
  applyIndividualParameters(
    createIndividualCharacteristics(species = Species$Rat),
    simulationViaRat
  )
  applyIndividualParameters(
    createIndividualCharacteristics(species = Species$Mouse),
    simulationViaRat
  )
  simulationDirect <- .loadExampleAciclovirSimulation()
  applyIndividualParameters(
    createIndividualCharacteristics(species = Species$Mouse),
    simulationDirect
  )

  organismValues <- function(simulation) {
    paths <- getAllParameterPathsIn(simulation)
    parameters <- getAllParametersMatching(
      paths[startsWith(paths, "Organism|")],
      simulation
    )
    values <- vapply(
      parameters,
      function(parameter) parameter$value,
      numeric(1)
    )
    names(values) <- vapply(
      parameters,
      function(parameter) parameter$path,
      character(1)
    )
    values[order(names(values))]
  }
  expect_equal(
    organismValues(simulationViaRat),
    organismValues(simulationDirect)
  )
})

# ospsuite's test snapshot holds a mouse project. Loading it takes about ten
# seconds, so the simulation is loaded once for this file.
.mouseSimulation <- local({
  simulation <- NULL
  function() {
    if (is.null(simulation)) {
      simulation <<- ospsuite::loadSimulationsFromSnapshot(
        system.file("extdata", "test_snapshot.json", package = "ospsuite")
      )[[1]]
    }
    simulation
  }
})

.humanCharacteristics <- function() {
  createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    gender = Gender$Male,
    weight = 73,
    height = 176,
    age = 30
  )
}

test_that("`applyIndividualParameters()` refuses a human individual on a non-human model", {
  mouseSimulation <- .mouseSimulation()
  weightBefore <- getParameter("Organism|Weight", mouseSimulation)$value

  expect_error(
    applyIndividualParameters(.humanCharacteristics(), mouseSimulation),
    "Mouse"
  )
  # The model is untouched
  expect_equal(
    getParameter("Organism|Weight", mouseSimulation)$value,
    weightBefore
  )

  # Another non-human species is still applied
  expect_no_error(
    applyIndividualParameters(
      createIndividualCharacteristics(species = Species$Rat),
      mouseSimulation
    )
  )
  expect_equal(
    getParameter(
      "Organism|Lumen|Stomach|Basal pH in fasted state",
      mouseSimulation
    )$value,
    3.9
  )
})

test_that("`.simulationSpecies()` reads the stored individual, then the parameters", {
  humanSimulation <- .loadExampleAciclovirSimulation()
  expect_identical(.simulationSpecies(humanSimulation), Species$Human)
  expect_identical(.simulationSpecies(.mouseSimulation()), Species$Mouse)

  # A MoBi model without a PK-Sim organism cannot be told
  mobiSimulation <- loadSimulation(
    system.file("extdata", "simple.pkml", package = "ospsuite"),
    loadFromCache = FALSE
  )
  expect_null(.simulationSpecies(mobiSimulation))

  # Without a stored individual, as in exports older than OSP version 12, the
  # human-only parameters decide
  local_mocked_bindings(.storedIndividualSpecies = function(simulation) NULL)
  expect_identical(.simulationSpecies(humanSimulation), Species$Human)
  expect_identical(.simulationSpecies(.mouseSimulation()), NA_character_)
  expect_error(
    applyIndividualParameters(.humanCharacteristics(), .mouseSimulation()),
    "not built for a human individual"
  )
  expect_no_error(applyIndividualParameters(
    .humanCharacteristics(),
    humanSimulation
  ))
})
