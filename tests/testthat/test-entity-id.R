# .canonicalizeId ----

# The result-only assertions below suppress the (intentional) "canonicalized"
# warning; the warning behavior itself is asserted in its own snapshot test.

test_that(".canonicalizeId lowercases its input", {
  expect_identical(suppressWarnings(.canonicalizeId("ID")), "id")
  expect_identical(
    suppressWarnings(.canonicalizeId("MyScenario")),
    "myscenario"
  )
})

test_that(".canonicalizeId leaves an already-safe id unchanged", {
  expect_identical(.canonicalizeId("aciclovir_iv"), "aciclovir_iv")
  expect_identical(.canonicalizeId("global"), "global")
})

test_that(".canonicalizeId replaces forbidden characters with underscore", {
  expect_identical(suppressWarnings(.canonicalizeId("a/b")), "a_b")
  expect_identical(suppressWarnings(.canonicalizeId("a\\b")), "a_b")
  expect_identical(
    suppressWarnings(.canonicalizeId("a:b*c?d\"e<f>g|h")),
    "a_b_c_d_e_f_g_h"
  )
})

test_that(".canonicalizeId strips control characters", {
  expect_identical(suppressWarnings(.canonicalizeId("a\tb\nc")), "a_b_c")
})

test_that(".canonicalizeId trims leading and trailing dots and spaces", {
  expect_identical(suppressWarnings(.canonicalizeId(" id ")), "id")
  expect_identical(suppressWarnings(.canonicalizeId(".id.")), "id")
  expect_identical(suppressWarnings(.canonicalizeId("..id..")), "id")
})

test_that(".canonicalizeId maps an empty or all-trimmed id to underscore", {
  expect_identical(suppressWarnings(.canonicalizeId("")), "_")
  expect_identical(suppressWarnings(.canonicalizeId("   ")), "_")
  expect_identical(suppressWarnings(.canonicalizeId("...")), "_")
})

test_that(".canonicalizeId suffixes a Windows reserved basename", {
  expect_identical(suppressWarnings(.canonicalizeId("CON")), "con_")
  expect_identical(suppressWarnings(.canonicalizeId("con")), "con_")
  expect_identical(suppressWarnings(.canonicalizeId("NUL")), "nul_")
  expect_identical(suppressWarnings(.canonicalizeId("COM1")), "com1_")
  expect_identical(suppressWarnings(.canonicalizeId("LPT9")), "lpt9_")
  # A name that merely starts with a reserved word is fine.
  expect_identical(.canonicalizeId("console"), "console")
  expect_identical(.canonicalizeId("com10"), "com10")
})

test_that(".canonicalizeId is vectorized", {
  expect_identical(
    suppressWarnings(.canonicalizeId(c("A", "b/c", "CON"))),
    c("a", "b_c", "con_")
  )
})

test_that(".canonicalizeId warns naming each changed id", {
  expect_snapshot(out <- .canonicalizeId("My ID*"))
  expect_identical(out, "my id_")
})

test_that(".canonicalizeId does not warn when nothing changes", {
  expect_no_warning(.canonicalizeId("already_safe"))
})

test_that(".canonicalizeId errors on a post-canonicalization collision", {
  expect_snapshot(error = TRUE, .canonicalizeId(c("ID", "id")))
  expect_snapshot(error = TRUE, .canonicalizeId(c("a/b", "a:b")))
})

# The collision guard must fire through the public authoring API for a
# case-differing pair (not just literal duplicates): the two ids are distinct
# on input but canonicalize to the same safe id, and the whole batch aborts
# writing nothing.
test_that("a public authoring call aborts on a case-differing id collision", {
  project <- testProject()
  before <- names(project$individuals)
  expect_snapshot(
    error = TRUE,
    addIndividual(project, c("Foo", "foo"), species = "Human", gender = "MALE")
  )
  expect_identical(names(project$individuals), before)
})

# An id over the filesystem single-component byte limit becomes an unwritable
# filename; bound it up front with a clear message rather than letting the
# eventual file write fail with an opaque `cannot open the connection`.
test_that(".canonicalizeId errors on an id too long to be a filename", {
  longId <- strrep("a", 300L)
  expect_snapshot(error = TRUE, .canonicalizeId(longId))
})

test_that(".canonicalizeId accepts an id at the byte limit", {
  atLimit <- strrep("a", 250L)
  expect_identical(.canonicalizeId(atLimit), atLimit)
})

# .nearestMatch ----

test_that(".nearestMatch returns the closest candidate within threshold", {
  expect_identical(
    .nearestMatch("indiv1", c("Indiv1", "Pop1")),
    "Indiv1"
  )
})

test_that(".nearestMatch is case-insensitive", {
  expect_identical(
    .nearestMatch("GLOBAL", c("Global", "somethingFarOff")),
    "Global"
  )
})

test_that(".nearestMatch returns up to three matches ordered by distance", {
  out <- .nearestMatch(
    "aci",
    c("aci1", "aci2", "aci3", "aci4", "totallyDifferent")
  )
  expect_length(out, 3L)
})

test_that(".nearestMatch returns empty when there are no candidates", {
  expect_identical(.nearestMatch("x", character(0)), character(0))
})

test_that(".nearestMatch returns empty when nothing is within threshold", {
  expect_identical(
    .nearestMatch("abc", c("zzzzzzzzz", "qqqqqqqqq")),
    character(0)
  )
})

# .suggestSuffix ----

test_that(".suggestSuffix builds a 'did you mean' suffix for a near id", {
  expect_match(
    .suggestSuffix("indiv1", c("Indiv1", "Pop1")),
    "did you mean.*Indiv1",
    ignore.case = TRUE
  )
})

test_that(".suggestSuffix is empty when there is no close candidate", {
  expect_identical(.suggestSuffix("xyz", c("aaaaaaaa", "bbbbbbbb")), "")
})
