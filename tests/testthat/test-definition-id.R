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

test_that(".canonicalizeId replaces interior commas and spaces with underscore", {
  # A comma or an interior space is legal on disk but makes a fragile id and
  # breaks the comma-separated reference lists the Excel bridge parses, so both
  # are canonicalized out (#1158). One character maps to one underscore.
  expect_identical(suppressWarnings(.canonicalizeId("a,b")), "a_b")
  expect_identical(suppressWarnings(.canonicalizeId("mg kg")), "mg_kg")
  expect_identical(
    suppressWarnings(.canonicalizeId("Sheet, with comma")),
    "sheet__with_comma"
  )
  # A leading/trailing space is still trimmed away, not turned into an
  # underscore (the trim runs before the replacement).
  expect_identical(suppressWarnings(.canonicalizeId(" id ")), "id")
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
  expect_identical(out, "my_id_")
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
  before <- names(project$definitions$individuals)
  expect_snapshot(
    error = TRUE,
    addIndividual(project, c("Foo", "foo"), species = "Human", gender = "MALE")
  )
  expect_identical(names(project$definitions$individuals), before)
})

# The point of the collector: however many ids one authoring call rewrites, the
# user hears about it once. Without it an `add*()` over a project of
# non-canonical ids emitted one warning for the definition's own id and another
# for each batch of references, which is the warning storm authoring over a
# migrated project used to produce.
test_that("an authoring call reports every id it canonicalizes in one warning", {
  project <- testProject()
  warnings <- character()
  withCallingHandlers(
    addScenario(
      project,
      id = "New Scenario",
      modelFile = "Aciclovir.pkml",
      individual = "Indiv1",
      outputPaths = "Aciclovir PVB"
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  # Precision is kept: the one warning names each rewritten value, the
  # definition's own id and both references alike.
  for (rewritten in c("New Scenario", "Indiv1", "Aciclovir PVB")) {
    expect_match(warnings[[1L]], rewritten, fixed = TRUE)
  }
})

test_that("a canonicalization done only to compare or re-key stays silent", {
  # `.silentlyCanonicalized()` covers the callers that canonicalize as an
  # internal step; the sink must drop those pairs rather than hold them for a
  # later flush, which is what plain `suppressWarnings()` fails to do.
  expect_no_warning(
    .collectCanonicalizedRefs(.silentlyCanonicalized(.canonicalizeId("Quiet")))
  )
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

# Invisible Unicode (#1213 item 16) ----

# `.canonicalizeOneId()` maps whitespace to `_` through `[[:space:]]`, which
# matches neither U+00A0 (no-break space) nor U+200B (zero-width space): under
# both TRE and PCRE that class is ASCII whitespace. So an id carrying one of them
# comes back carrying it, and becomes a definition filename carrying it.
#
# Pinned here as well as through the importer because this is the single shared
# chokepoint every id goes through, from either entrypoint. Live data rather than
# a synthetic probe: one migrated project carried 12 real ids containing U+00A0.
test_that(".canonicalizeOneId leaves a no-break or zero-width space in the id", {
  nbsp <- "\u00a0"
  zwsp <- "\u200b"

  # An ordinary space becomes `_`; these do not.
  expect_identical(.canonicalizeOneId("Out Path"), "out_path")
  expect_identical(
    .canonicalizeOneId(paste0("Out", nbsp, "Path")),
    paste0("out", nbsp, "path")
  )
  expect_identical(
    .canonicalizeOneId(paste0("Out", zwsp, "Path")),
    paste0("out", zwsp, "path")
  )

  # So two ids that render identically canonicalize to two different ids, and
  # the warning a caller would print about the rewrite reads as a no-op.
  expect_false(identical(
    .canonicalizeOneId("OutPath"),
    .canonicalizeOneId(paste0("Out", zwsp, "Path"))
  ))
})

# The same class admits the other invisible formatting characters, so a fix that
# only special-cases U+00A0 and U+200B would leave these behind.
test_that(".canonicalizeOneId leaves the other invisible format characters in the id", {
  for (ch in c("\u2060", "\u200d", "\ufeff")) {
    expect_identical(
      .canonicalizeOneId(paste0("a", ch, "b")),
      paste0("a", ch, "b")
    )
  }
})

# #1213 item 25: the distance threshold is
# `max(1, min(3, ceiling(nchar(x) / 3)))`, so a candidate that diverges by more
# than 3 characters is never suggested however long the id is. The two shapes that
# occur in practice, a per-analyte suffix and a `_mean` sibling, both diverge by
# more than that, so the hint stays silent in exactly the cases it was added for
# while firing on a one-character typo.
test_that(".nearestMatch cannot reach a candidate diverging by a real suffix", {
  # A `_mean` sibling: 5 characters, over the cap of 3.
  expect_identical(
    .nearestMatch("aciclovir_pvb_mean", "aciclovir_pvb"),
    character(0)
  )
  # A per-analyte variant: 4 characters, also over the cap.
  expect_identical(
    .nearestMatch("aciclovir_pvb_m1og", "aciclovir_pvb"),
    character(0)
  )
  # Meanwhile a single-character divergence is suggested, however long the id.
  expect_identical(
    .nearestMatch("aciclovir_pvc", "aciclovir_pvb"),
    "aciclovir_pvb"
  )
})
