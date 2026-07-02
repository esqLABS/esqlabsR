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

test_that(".wholeField applies a vector whole to every entity", {
  expect_identical(
    .wholeField(c("global", "def"), 2L),
    list(c("global", "def"), c("global", "def"))
  )
})

test_that(".wholeField aligns a length-N list of vectors per entity", {
  expect_identical(
    .wholeField(list(c("a", "b"), "c"), 2L),
    list(c("a", "b"), "c")
  )
})

test_that(".alignAuthoringArgs builds N per-entity field sets", {
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
