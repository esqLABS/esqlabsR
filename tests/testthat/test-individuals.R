test_that(".parseIndividuals passes through unknown fields", {
  raw <- list(
    list(
      individualId = "I1",
      species = "Human",
      weight = 70,
      futureField = "kept"
    )
  )
  result <- .parseIndividuals(raw)

  expect_identical(result[["I1"]]$species, "Human")
  expect_identical(result[["I1"]]$weight, 70)
  expect_identical(result[["I1"]]$futureField, "kept")
})

test_that("an individual carrying an unknown field round-trips through serialization", {
  raw <- list(
    list(
      individualId = "I1",
      species = "Human",
      gender = "MALE",
      futureField = "kept"
    )
  )
  individuals <- .parseIndividuals(raw)
  project <- .fakeProject(individuals = individuals)

  out <- .individualsToJson(project)[[1L]]
  expect_identical(out$individualId, "I1")
  expect_identical(out$futureField, "kept")
  # individualId leads; the remaining fields pass through in record order.
  expect_identical(
    names(out),
    c("individualId", "species", "gender", "futureField")
  )
})

test_that("addIndividual + removeIndividual round-trip leaves the section unchanged", {
  project <- testProject()
  before <- project$definitions$individuals

  addIndividual(project, "newi", species = "Human", gender = "MALE")
  expect_true("newi" %in% names(project$definitions$individuals))

  removeIndividual(project, "newi")
  expect_identical(project$definitions$individuals, before)
})

test_that("addIndividual aborts when individualId already exists", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addIndividual(project, "indiv1", species = "Human", gender = "MALE")
  )
})

test_that("addIndividual overwrite = TRUE replaces an existing individual", {
  project <- testProject()
  before <- length(project$definitions$individuals)
  addIndividual(
    project,
    "indiv1",
    species = "Human",
    gender = "FEMALE",
    overwrite = TRUE
  )
  expect_length(project$definitions$individuals, before)
  expect_identical(project$definitions$individuals[["indiv1"]]$gender, "FEMALE")
})

test_that("addIndividual rejects a non-logical overwrite passed through ...", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addIndividual(project, "newi", species = "Human", overwrite = "TRUE")
  )
})

test_that("addIndividual defaults an absent gender to UNKNOWN", {
  project <- testProject()
  addIndividual(project, "newi", species = "Dog")
  expect_identical(project$definitions$individuals$newi$gender, "UNKNOWN")
})

test_that(".parseIndividuals defaults an absent gender to UNKNOWN", {
  # JSON is the primary authoring path: an individual hand-authored in
  # definitions/individuals/*.json without a gender key must get the same
  # UNKNOWN default the authoring API and the Excel importer apply.
  parsed <- .parseIndividuals(list(
    list(individualId = "beagle", species = "Dog", weight = 10),
    list(individualId = "adult", species = "Human", gender = "FEMALE")
  ))
  expect_identical(parsed$beagle$gender, "UNKNOWN")
  expect_identical(parsed$adult$gender, "FEMALE")
})

test_that("addIndividual aborts when gender is not a valid GenderInt token", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addIndividual(project, "newi", species = "Human", gender = "banana")
  )
  expect_false("newi" %in% names(project$definitions$individuals))
})

test_that("addIndividual accepts a valid GenderInt token", {
  project <- testProject()
  addIndividual(project, "newi", species = "Human", gender = "FEMALE")
  expect_identical(project$definitions$individuals$newi$gender, "FEMALE")
})

test_that("addIndividual rejects a non-numeric weight/height/age", {
  project <- testProject()
  # "80kg" would silently coerce to NA via as.double(); it must abort instead.
  expect_error(
    addIndividual(
      project,
      "bad",
      species = "Human",
      gender = "MALE",
      weight = "80kg"
    ),
    "weight must be a single finite number"
  )
  expect_false("bad" %in% names(project$definitions$individuals))
})

test_that("removeIndividual warns when referenced by a scenario", {
  project <- testProject()
  referenced <- "indiv1"
  expect_warning(removeIndividual(project, referenced), "referenced")
  expect_false(referenced %in% names(project$definitions$individuals))
})

# setIndividual ----

test_that("setIndividual changes a field in memory and persists on save", {
  project <- testProject()
  setIndividual(project, "indiv1", weight = 80)
  expect_equal(project$definitions$individuals[["indiv1"]]$weight, 80)

  # The edit reaches disk on save: a throwaway reload sees the new value.
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_equal(reloaded$definitions$individuals[["indiv1"]]$weight, 80)
})

test_that("setIndividual coerces numeric fields like addIndividual", {
  project <- testProject()
  setIndividual(project, "indiv1", age = "45")
  expect_identical(project$definitions$individuals[["indiv1"]]$age, 45)
})

test_that("setIndividual partial update leaves other fields untouched", {
  project <- testProject()
  before <- project$definitions$individuals[["indiv1"]]
  setIndividual(project, "indiv1", height = 180)

  after <- project$definitions$individuals[["indiv1"]]
  expect_equal(after$height, 180)
  for (f in setdiff(names(before), "height")) {
    expect_equal(after[[f]], before[[f]])
  }
})

test_that("setIndividual clears a numeric field passed NULL", {
  # A NULL clears (removes) the optional field: the key must be ABSENT, not
  # present as numeric(0). The indiv1 fixture carries weight/height/age, so
  # each removal is observable.
  for (field in c("weight", "height", "age")) {
    project <- testProject()
    before <- project$definitions$individuals[["indiv1"]]

    do.call(
      setIndividual,
      c(list(project, "indiv1"), stats::setNames(list(NULL), field))
    )

    after <- project$definitions$individuals[["indiv1"]]
    expect_false(field %in% names(after))
    expect_null(after[[field]])
    # No other field changed, and no unexpected key was added.
    expect_setequal(names(after), setdiff(names(before), field))
    for (f in setdiff(names(before), field)) {
      expect_equal(after[[f]], before[[f]])
    }
  }
})

test_that("setIndividual clears the validation flag", {
  project <- testProject()
  .markValidated(project)
  setIndividual(project, "indiv1", weight = 80)
  expect_false(.isValidated(project))
})

test_that("setIndividual aborts on a non-existent individual", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    setIndividual(project, "Ghost", weight = 80)
  )
})

test_that("setIndividual rejects an empty gender like addIndividual", {
  project <- testProject()
  before <- project$definitions$individuals[["indiv1"]]
  expect_snapshot(
    error = TRUE,
    setIndividual(project, "indiv1", gender = "")
  )
  # Memory unchanged after the rejected write.
  expect_equal(project$definitions$individuals[["indiv1"]], before)
})

test_that("setIndividual rejects a gender that is not a valid GenderInt token", {
  project <- testProject()
  before <- project$definitions$individuals[["indiv1"]]
  expect_snapshot(
    error = TRUE,
    setIndividual(project, "indiv1", gender = "banana")
  )
  # Memory unchanged after the rejected write.
  expect_equal(project$definitions$individuals[["indiv1"]], before)
})

test_that("setIndividual rejects a non-numeric weight like addIndividual", {
  project <- testProject()
  before <- project$definitions$individuals[["indiv1"]]
  # "80kg" would silently coerce to NA via as.double(); it must abort instead,
  # mirroring the add-path guard.
  expect_snapshot(
    error = TRUE,
    setIndividual(project, "indiv1", weight = "80kg")
  )
  # Memory unchanged after the rejected write.
  expect_equal(project$definitions$individuals[["indiv1"]], before)
})

test_that("setIndividual accepts a valid GenderInt token", {
  project <- testProject()
  setIndividual(project, "indiv1", gender = "FEMALE")
  expect_identical(project$definitions$individuals[["indiv1"]]$gender, "FEMALE")
})

test_that("setIndividual rejects parameterSets that do not resolve", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    setIndividual(project, "indiv1", parameterSets = "Ghost")
  )
})

test_that("setIndividual stays in memory until saveProject()", {
  source <- testProject()
  before <- source$definitions$individuals[["indiv1"]]
  setIndividual(source, "indiv1", weight = 99)

  expect_equal(source$definitions$individuals[["indiv1"]]$weight, 99)
  # The edit must not reach the on-disk tree before a save: a fresh load still
  # sees the original value.
  reloaded <- loadProject(source$info$projectFilePath)
  expect_equal(reloaded$definitions$individuals[["indiv1"]], before)
})

# setIndividual parameterSets replacement ----

test_that("setIndividual replaces the parameter-set refs and persists on save", {
  project <- testProject()
  # The fixture individual references "indiv1_default"; point it at another
  # existing set instead.
  setIndividual(project, "indiv1", parameterSets = "global")
  expect_identical(
    project$definitions$individuals[["indiv1"]]$parameterSets,
    "global"
  )

  # The edit reaches disk on save.
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(
    reloaded$definitions$individuals[["indiv1"]]$parameterSets,
    "global"
  )
})

test_that("setIndividual aborts on an undefined parameter set", {
  project <- testProject()
  before <- project$definitions$individuals[["indiv1"]]
  expect_snapshot(
    error = TRUE,
    setIndividual(project, "indiv1", parameterSets = "Ghost")
  )
  expect_identical(project$definitions$individuals[["indiv1"]], before)
})

# Vectorized authoring ----

test_that("addIndividual adds N individuals in one call equal to N scalar adds", {
  vectorized <- testProject()
  addIndividual(
    vectorized,
    c("adult_female", "adult_male"),
    species = "Human",
    gender = c("FEMALE", "MALE"),
    weight = 60,
    height = 165,
    age = 35,
    parameterSets = "global"
  )

  scalar <- testProject()
  addIndividual(
    scalar,
    "adult_female",
    species = "Human",
    gender = "FEMALE",
    weight = 60,
    height = 165,
    age = 35,
    parameterSets = "global"
  )
  addIndividual(
    scalar,
    "adult_male",
    species = "Human",
    gender = "MALE",
    weight = 60,
    height = 165,
    age = 35,
    parameterSets = "global"
  )

  expect_identical(
    vectorized$definitions$individuals[c("adult_female", "adult_male")],
    scalar$definitions$individuals[c("adult_female", "adult_male")]
  )
})

test_that("addIndividual recycles scalar fields and applies parameterSets whole", {
  project <- testProject()
  addIndividual(
    project,
    c("a", "b"),
    species = "Human",
    gender = c("FEMALE", "MALE"),
    weight = 60,
    parameterSets = c("global", "aciclovir")
  )

  expect_identical(project$definitions$individuals$a$species, "Human")
  expect_identical(project$definitions$individuals$a$gender, "FEMALE")
  expect_identical(project$definitions$individuals$b$gender, "MALE")
  expect_identical(project$definitions$individuals$a$weight, 60)
  expect_identical(project$definitions$individuals$b$weight, 60)
  # parameterSets applied whole to both.
  expect_identical(
    project$definitions$individuals$a$parameterSets,
    c("global", "aciclovir")
  )
  expect_identical(
    project$definitions$individuals$b$parameterSets,
    c("global", "aciclovir")
  )
})

test_that("addIndividual applies an ontogeny vector whole and stores it as a vector", {
  # One entry per ontogeny: the vector belongs to the individual as a whole and
  # is never split across ids, and it survives the trip through JSON (where an
  # array would otherwise read back as a list).
  project <- testProject()
  ontogenies <- c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  addIndividual(
    project,
    c("a", "b"),
    species = "Human",
    proteinOntogenies = ontogenies
  )
  expect_identical(
    project$definitions$individuals$a$proteinOntogenies,
    ontogenies
  )
  expect_identical(
    project$definitions$individuals$b$proteinOntogenies,
    ontogenies
  )

  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(
    reloaded$definitions$individuals$a$proteinOntogenies,
    ontogenies
  )
})

test_that("setIndividual refuses an ontogeny value it cannot store", {
  # An `ospsuite::MoleculeOntogeny` object stored unchecked reached
  # `saveProject()` as an R6 object the JSON writer cannot serialize, which left
  # the whole project unsaved.
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    setIndividual(
      project,
      "indiv1",
      proteinOntogenies = ospsuite::MoleculeOntogeny$new(
        molecule = "CYP3A4",
        ontogeny = ospsuite::StandardOntogeny$CYP3A4
      )
    )
  )
  expect_no_error(saveProject(project))
})

test_that("addIndividual persists all N to disk in one saveProject()", {
  project <- testProject()
  addIndividual(
    project,
    c("a", "b"),
    species = "Human",
    gender = "MALE"
  )
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_true(all(c("a", "b") %in% names(reloaded$definitions$individuals)))
})

test_that("addIndividual aborts the whole batch and writes nothing on one bad entry", {
  project <- testProject()
  before <- names(project$definitions$individuals)
  expect_error(
    addIndividual(
      project,
      c("a", "b"),
      species = "Human",
      # b has no gender -> the whole batch must abort.
      gender = c("MALE", "")
    )
  )
  # Neither memory nor disk gained any individual.
  expect_identical(names(project$definitions$individuals), before)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(names(reloaded$definitions$individuals), before)
})

test_that("addIndividual aborts on a mismatched scalar field length", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addIndividual(
      project,
      c("a", "b", "c"),
      species = "Human",
      gender = c("MALE", "FEMALE")
    )
  )
})

test_that("addIndividual aborts on a duplicate id in the batch", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addIndividual(project, c("a", "a"), species = "Human", gender = "MALE")
  )
})

test_that("setIndividual vectorizes a partial update across N ids", {
  project <- testProject()
  addIndividual(project, c("a", "b"), species = "Human", gender = "MALE")
  setIndividual(project, c("a", "b"), weight = c(80, 90), age = 40)

  expect_identical(project$definitions$individuals$a$weight, 80)
  expect_identical(project$definitions$individuals$b$weight, 90)
  expect_identical(project$definitions$individuals$a$age, 40)
  expect_identical(project$definitions$individuals$b$age, 40)
  # An unsupplied field is untouched.
  expect_identical(project$definitions$individuals$a$gender, "MALE")
})

test_that("removeIndividual removes a vector of ids in one write-through", {
  project <- testProject()
  addIndividual(project, c("a", "b"), species = "Human", gender = "MALE")
  removeIndividual(project, c("a", "b"))
  expect_false(any(c("a", "b") %in% names(project$definitions$individuals)))
  reloaded <- loadProject(project$info$projectFilePath)
  expect_false(any(c("a", "b") %in% names(reloaded$definitions$individuals)))
})

test_that("setIndividual parameterSets vectorizes whole across N ids", {
  project <- testProject()
  addIndividual(project, c("a", "b"), species = "Human", gender = "MALE")
  setIndividual(project, c("a", "b"), parameterSets = c("global", "aciclovir"))
  expect_identical(
    project$definitions$individuals$a$parameterSets,
    c("global", "aciclovir")
  )
  expect_identical(
    project$definitions$individuals$b$parameterSets,
    c("global", "aciclovir")
  )
})

# Print method ----

test_that("print.Individual renders the configured fields", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$individuals[["indiv1"]]))
})

test_that("print.Individual renders a minimal individual", {
  project <- testProject()
  addIndividual(project, "minimal", species = "Human", gender = "MALE")
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$individuals[["minimal"]]))
})

test_that("a classed Individual still behaves as a list", {
  project <- testProject()
  indiv <- project$definitions$individuals[["indiv1"]]
  expect_type(indiv, "list")
  expect_identical(indiv[["species"]], "Human")
  expect_true("gender" %in% names(indiv))
})
