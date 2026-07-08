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

test_that(".canonicalizeOneId suffixes a reserved basename even with an extension", {
  # A Windows device name is reserved regardless of any extension, so the base
  # segment (before the first dot) is what decides it.
  expect_identical(.canonicalizeOneId("con.txt"), "con.txt_")
  expect_identical(.canonicalizeOneId("com1.log"), "com1.log_")
  expect_identical(.canonicalizeOneId("LPT9.dat"), "lpt9.dat_")
  # A bare reserved name is still suffixed.
  expect_identical(.canonicalizeOneId("con"), "con_")
  # A non-reserved name with an extension is left untouched.
  expect_identical(.canonicalizeOneId("data.txt"), "data.txt")
  expect_identical(.canonicalizeOneId("console.txt"), "console.txt")
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

test_that(".canonicalizeId canonicalizes and warns on an id containing braces instead of aborting", {
  expect_snapshot(out <- .canonicalizeId("Conc{Organ}"))
  expect_identical(out, "conc{organ}")
})

test_that(".canonicalizeId does not evaluate brace content as R code", {
  # The quotes are forbidden characters and get replaced with `_`, so the id
  # does change (and warns); the real assertion is that this returns at all
  # rather than evaluating `stop("boom")`, which would raise "boom" instead.
  expect_identical(
    suppressWarnings(.canonicalizeId('x{stop("boom")}')),
    "x{stop(_boom_)}"
  )
})

test_that(".canonicalizeId does not warn when nothing changes", {
  expect_no_warning(.canonicalizeId("already_safe"))
})

test_that(".canonicalizeId errors on a post-canonicalization collision", {
  expect_snapshot(error = TRUE, .canonicalizeId(c("ID", "id")))
  expect_snapshot(error = TRUE, .canonicalizeId(c("a/b", "a:b")))
})

test_that(".canonicalizeId does not treat an identically repeated id as a collision", {
  # One distinct id supplied twice canonicalizes to a single value with a
  # single pre-image; that is not ambiguity, so it must not abort.
  expect_identical(
    suppressWarnings(.canonicalizeId(c("Foo", "Foo"))),
    c("foo", "foo")
  )
  expect_identical(
    .canonicalizeId(c("safe_id", "safe_id", "safe_id")),
    c("safe_id", "safe_id", "safe_id")
  )
})

test_that(".canonicalizeId still errors when distinct ids collide via characters", {
  # Two genuinely distinct inputs collapsing to one canonical id is ambiguity.
  expect_snapshot(error = TRUE, .canonicalizeId(c("a/b", "a_b")))
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

# .canonicalizeIdRef ----

test_that(".canonicalizeIdRef canonicalizes an empty string like the definition side", {
  # The definition side maps `""` to `"_"`; a reference of `""` must follow the
  # same transform so both sides land on the same canonical id.
  expect_identical(
    suppressWarnings(.canonicalizeIdRef("")),
    .canonicalizeOneId("")
  )
  expect_identical(suppressWarnings(.canonicalizeIdRef("")), "_")
})

test_that(".canonicalizeIdRef passes NA through unchanged", {
  expect_identical(.canonicalizeIdRef(NA_character_), NA_character_)
  expect_identical(
    suppressWarnings(.canonicalizeIdRef(c("A", NA, ""))),
    c("a", NA, "_")
  )
})

test_that(".canonicalizeIdRef canonicalizes a normal reference", {
  expect_identical(suppressWarnings(.canonicalizeIdRef("Indiv1")), "indiv1")
  expect_identical(.canonicalizeIdRef("already_safe"), "already_safe")
})

test_that(".canonicalizeIdRef canonicalizes and warns on a reference containing braces instead of aborting", {
  expect_snapshot(out <- .canonicalizeIdRef("Ind{Organ}"))
  expect_identical(out, "ind{organ}")
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
