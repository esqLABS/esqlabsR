test_that(".assertIdVector accepts a non-empty character vector", {
  expect_identical(.assertIdVector(c("a", "b")), c("a", "b"))
})

test_that(".assertIdVector rejects empty, NA, or non-character ids", {
  expect_snapshot(error = TRUE, .assertIdVector(character(0)))
  expect_snapshot(error = TRUE, .assertIdVector(c("a", NA)))
  expect_snapshot(error = TRUE, .assertIdVector(c("a", "")))
  expect_snapshot(error = TRUE, .assertIdVector(1:3))
})

test_that(".recycleField recycles a length-1 value to all N", {
  expect_identical(
    .recycleField("x", 3L, "f"),
    list("x", "x", "x")
  )
})

test_that(".recycleField aligns a length-N value by position", {
  expect_identical(
    .recycleField(c("a", "b", "c"), 3L, "f"),
    list("a", "b", "c")
  )
})

test_that(".recycleField passes NULL through as N NULLs", {
  expect_identical(.recycleField(NULL, 2L, "f"), list(NULL, NULL))
})

test_that(".recycleField aborts on a length that is neither 1 nor N", {
  expect_snapshot(error = TRUE, .recycleField(c("a", "b"), 3L, "weight"))
})

test_that(".wholeField applies a vector whole to every definition", {
  expect_identical(
    .wholeField(c("global", "def"), 2L),
    list(c("global", "def"), c("global", "def"))
  )
})

test_that(".wholeField aligns a length-N list of vectors per definition", {
  expect_identical(
    .wholeField(list(c("a", "b"), "c"), 2L),
    list(c("a", "b"), "c")
  )
})

test_that(".alignAuthoringArgs builds N per-definition field sets", {
  out <- .alignAuthoringArgs(
    id = c("a", "b"),
    scalarFields = list(species = "Human", gender = c("FEMALE", "MALE")),
    wholeFields = list(parameterSets = c("global", "def"))
  )
  expect_length(out, 2L)
  expect_identical(
    out[[1]],
    list(
      species = "Human",
      gender = "FEMALE",
      parameterSets = c("global", "def")
    )
  )
  expect_identical(
    out[[2]],
    list(species = "Human", gender = "MALE", parameterSets = c("global", "def"))
  )
})

test_that(".alignAuthoringArgs preserves NULL fields as present-but-NULL", {
  out <- .alignAuthoringArgs(
    id = "a",
    scalarFields = list(species = "Human", population = NULL)
  )
  expect_true("population" %in% names(out[[1]]))
  expect_null(out[[1]]$population)
})

test_that(".alignAuthoringArgs propagates a length error naming the field", {
  expect_snapshot(
    error = TRUE,
    .alignAuthoringArgs(
      id = c("a", "b", "c"),
      scalarFields = list(weight = c(60, 70))
    )
  )
})

test_that(".assertAuthoringFieldsNamed passes a fully named field list", {
  fields <- list(species = "Rat", weight = 0.25)
  expect_identical(.assertAuthoringFieldsNamed(fields), fields)
  expect_identical(.assertAuthoringFieldsNamed(list()), list())
})

test_that(".assertAuthoringFieldsNamed names the unnamed positions", {
  # Every field is looked up by name downstream, so an unnamed one used to
  # vanish without a word. It arrives that way when a `set*()` field is passed
  # positionally.
  expect_snapshot(error = TRUE, .assertAuthoringFieldsNamed(list("Rat")))
  expect_snapshot(
    error = TRUE,
    .assertAuthoringFieldsNamed(list(species = "Rat", "UNKNOWN"))
  )
})

test_that("setIndividual() rejects a positionally passed field", {
  project <- .fakeProject(
    individuals = list(indiv1 = list(species = "Human"))
  )
  expect_error(setIndividual(project, "indiv1", "Rat"), "must be named")
  # The silent-drop version left the individual untouched and said nothing.
  expect_identical(project$definitions$individuals$indiv1$species, "Human")
})

test_that("setPopulation() rejects a positionally passed field", {
  project <- .fakeProject(
    populations = list(pop1 = list(species = "Human"))
  )
  expect_error(setPopulation(project, "pop1", "Rat"), "must be named")
})

test_that("the unnamed-field hint points at no one set*() function", {
  # `setScenario()`, `setIndividual()` and `setPopulation()` all raise it, and
  # they share no field, so an example written for one misdirects the other two.
  # The abort header already names the function the caller used.
  project <- .fakeProject(
    populations = list(pop1 = list(species = "Human"))
  )
  err <- expect_error(setPopulation(project, "pop1", "Rat"))
  expect_no_match(
    conditionMessage(err),
    "setScenario|setIndividual|setPopulation"
  )
})

test_that(".coerceNumericField returns NULL for NULL and as.double otherwise", {
  # A NULL passes through as NULL so the set-path loops delete the key (clearing
  # the optional field); any other value coerces with as.double().
  expect_null(.coerceNumericField(NULL))
  expect_identical(.coerceNumericField(45), 45)
  expect_identical(.coerceNumericField("45"), 45)
})

test_that(".isUnsetNumericField treats NA as unset but NaN as a value", {
  # An empty workbook cell arrives as NA, so NA means "field not set".
  expect_true(.isUnsetNumericField(NA))
  expect_true(.isUnsetNumericField(NA_real_))
  expect_true(.isUnsetNumericField(NA_character_))
  # NaN satisfies is.na() but never comes from an empty cell: it comes from a
  # calculation that went wrong, so it is a value, and an invalid one.
  expect_false(.isUnsetNumericField(NaN))
  expect_false(.isUnsetNumericField(45))
  expect_false(.isUnsetNumericField(c(NA, NA)))
})

test_that(".isInvalidNumericField rejects NaN and anything not coercing to a number", {
  expect_true(.isInvalidNumericField(NaN))
  expect_true(.isInvalidNumericField(Inf))
  expect_true(.isInvalidNumericField("80kg"))
  expect_true(.isInvalidNumericField(c(1, 2)))
  # Not set, so not invalid.
  expect_false(.isInvalidNumericField(NULL))
  expect_false(.isInvalidNumericField(NA))
  # Set and usable, including a numeric-like string from a workbook.
  expect_false(.isInvalidNumericField(45))
  expect_false(.isInvalidNumericField("45"))
})
