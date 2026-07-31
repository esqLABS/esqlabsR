# Definition id canonicalization + suggestion ----
#
# Every definition in a Project (a scenario, parameter set, individual,
# population, application, output path, plot, ...) is referenced by its
# **id**, and for the definition-file tree the id equals its on-disk filename.
# Ids therefore have to be safe single path segments on every target
# filesystem.
#
# Rather than rejecting an unsafe id, the authoring API runs it through a
# deterministic canonicalizer (`.canonicalizeId()`) the moment it enters the
# system: both when a definition is created (the `id` argument of an `add*`
# / `set*` function) and when an id is referenced (a foreign-key argument
# such as a scenario's `individualId` or its parameter-set list). Because the
# transform is deterministic and applied identically on both sides, a
# definition and a reference made from the same typed string always land on
# the same canonical id, which is what makes lossy sanitization safe: a
# reference still resolves even if the user types the un-sanitized form.
#
# A reference that has no matching definition (after canonicalization) is a
# genuine referential problem; `.nearestMatch()` / `.suggestSuffix()` turn it
# into a "did you mean '...'?" hint, surfaced by the cross-reference
# validator.

# Canonicalize one or more definition ids into safe, lowercase, single
# path-segment ids. Vectorized over `ids`.
#
# Rules (intersection of Windows + macOS + Linux filename rules):
#   - lowercase
#   - trim leading/trailing dots and spaces
#   - replace each of `/ \ : * ? " < > |`, commas, whitespace (spaces, tabs),
#     and control characters with `_`
#   - an id that is empty or trims to nothing becomes `_`
#   - a Windows reserved basename (CON, PRN, AUX, NUL, COM1-9, LPT1-9,
#     case-insensitive) gets a `_` suffix so it is never a bare reserved name
#
# Warns (once per call) naming each `input -> canonical` pair that changed,
# so the user knows the id that was actually used. Errors when two distinct
# inputs in the same call canonicalize to the same id (real ambiguity, not a
# fixable character issue).
#
# @keywords internal
# @noRd
.canonicalizeId <- function(ids) {
  if (!is.character(ids)) {
    cli::cli_abort("{.arg id} must be a character vector.")
  }
  if (length(ids) == 0L) {
    return(ids)
  }

  canonical <- vapply(ids, .canonicalizeOneId, character(1), USE.NAMES = FALSE)

  changed <- !is.na(ids) & ids != canonical
  if (any(changed)) {
    rendered <- .canonicalizedIdBullets(ids[changed], canonical[changed])
    cli::cli_warn(
      c(
        "Canonicalized {sum(changed)} id{?s} to a safe form:",
        rendered$bullets
      ),
      .envir = rendered$envir
    )
  }

  # Two DISTINCT inputs that collapse to one canonical id are real ambiguity.
  # A canonical id reached from a single distinct pre-image (the same id
  # supplied twice) is not a collision, so keep only canonicals with more than
  # one distinct offender.
  clashing <- unique(canonical[duplicated(canonical)])
  clashing <- clashing[vapply(
    clashing,
    function(c) length(unique(ids[canonical == c])) > 1L,
    logical(1)
  )]
  if (length(clashing) > 0L) {
    bullets <- vapply(
      clashing,
      function(c) {
        offenders <- unique(ids[canonical == c])
        # Interpolate the variables (so cli quotes each safely) rather than
        # inlining user text into the glue expression itself.
        cli::format_inline("{.val {offenders}} -> {.val {c}}")
      },
      character(1)
    )
    cli::cli_abort(c(
      "Ids collide after canonicalization:",
      stats::setNames(bullets, rep("x", length(bullets))),
      "i" = "Two distinct ids that canonicalize to the same id are ambiguous; \\
      rename so they differ by more than case or forbidden characters."
    ))
  }

  canonical
}

# Build one `input -> canonical` bullet template per pair for the changed-id
# / changed-reference warnings, quoting each value safely rather than
# inlining raw user text into a cli glue template (which would evaluate
# `{...}` content in the text as an R expression). Returns the bullet
# templates (still unglued) together with the environment binding their
# variables; the caller passes both straight to a single `cli_warn()` /
# `cli_abort()` call (`.envir = rendered$envir`) so the templates are
# glue-parsed exactly once. Pre-rendering each bullet with
# `cli::format_inline()` and handing the *rendered* strings to a second
# `cli_warn()` call is not actually safe: the rendered text still contains
# the value's literal `{`/`}` characters, and cli glue-parses that text again
# when the outer call formats it, evaluating the very content this is meant
# to guard against. Binding each pair's `input`/`canonical` under
# bullet-indexed variable names avoids that second pass entirely. The
# returned environment's parent is the caller of this function, so the
# caller's own glue expressions elsewhere in the same message (e.g.
# `{sum(changed)}` in a summary line) still resolve normally.
#
# @keywords internal
# @noRd
.canonicalizedIdBullets <- function(inputs, canonicals) {
  envir <- new.env(parent = parent.frame())
  bullets <- vapply(
    seq_along(inputs),
    function(i) {
      inputVar <- paste0("input", i)
      canonVar <- paste0("canon", i)
      assign(inputVar, inputs[i], envir = envir)
      assign(canonVar, canonicals[i], envir = envir)
      sprintf("{.val {%s}} -> {.val {%s}}", inputVar, canonVar)
    },
    character(1)
  )
  list(
    bullets = stats::setNames(bullets, rep("*", length(bullets))),
    envir = envir
  )
}

# Canonicalize a foreign-key reference argument (e.g. a scenario's
# `individualId`, or its `modelParameterSets` / `outputPathIds` vector) the
# same way `.canonicalizeId()` canonicalizes a definition's id, so a
# reference made from the same typed string as the definition resolves to
# it. `NULL` passes through unchanged (the reference is absent), and so does
# `NA` (the FK validators reject NA with their own clearer message). Unlike
# `.canonicalizeId()`, this does not error on within-vector collisions: a
# reference list is deduplicated by the lookup anyway.
#
# @keywords internal
# @noRd
.canonicalizeIdRef <- function(ref) {
  if (is.null(ref) || !is.character(ref) || length(ref) == 0L) {
    return(ref)
  }
  # Canonicalize `""` too, not just non-empty refs: the definition side
  # (`.canonicalizeOneId("")`) maps `""` to `"_"`, so a reference of `""` must
  # follow the same transform or it would never resolve to a definition made
  # from `""`. Only `NA` passes through untouched (the FK validators reject it
  # with a clearer message).
  keep <- !is.na(ref)
  if (!any(keep)) {
    return(ref)
  }
  out <- ref
  canon <- vapply(
    ref[keep],
    .canonicalizeOneId,
    character(1),
    USE.NAMES = FALSE
  )
  changed <- ref[keep] != canon
  if (any(changed)) {
    # When a batch authoring call is collecting reference canonicalizations
    # (`.collectCanonicalizedRefs()`), record each changed pair and stay
    # silent so the caller emits one consolidated warning per call instead of
    # one per definition. Outside a collector (a standalone call) warn immediately.
    inputs <- ref[keep][changed]
    canonicals <- canon[changed]
    if (.canonRefSink$depth > 0L) {
      .canonRefSink$inputs <- c(.canonRefSink$inputs, inputs)
      .canonRefSink$canonicals <- c(.canonRefSink$canonicals, canonicals)
    } else {
      .warnCanonicalizedRefs(inputs, canonicals)
    }
  }
  out[keep] <- canon
  out
}

# Canonicalize a vector-valued foreign-key argument (a scenario's
# `parameterSets` / `initialConditions` / `outputPaths`) as it comes out of a
# definition file, so a scenario's own written fields can be handed straight back
# to an authoring function. Two normalizations get it there:
#
#   * A list of one-element strings flattens to the character vector the
#     reference list is. The package reads a definition file with
#     `jsonlite::fromJSON(simplifyVector = FALSE)`, which turns a JSON array of
#     ids into `list("a", "b")`; the FK validators want the character vector.
#     A list holding anything else is left alone for them to reject.
#   * A zero-length value becomes `NULL`. A definition file carries `[]` for a
#     scenario that references none, and `character(0)` (or `list()`) means
#     exactly what `NULL` means here: there are none. That also keeps the record
#     shape identical to the one `.parseScenarios()` builds from the same `[]`.
#
# @keywords internal
# @noRd
.canonicalizeVectorIdRef <- function(ref) {
  if (is.list(ref) && all(vapply(ref, .isScalarString, logical(1)))) {
    ref <- unlist(ref)
  }
  ref <- .canonicalizeIdRef(ref)
  if (length(ref) == 0L) {
    return(NULL)
  }
  ref
}

# Is `x` a single string? Used to decide whether a list of reference ids can be
# flattened to a character vector.
#
# @keywords internal
# @noRd
.isScalarString <- function(x) {
  is.character(x) && length(x) == 1L
}

# Sink for collecting reference-canonicalization changes across the per-definition
# builds of one vectorized authoring call, so the whole call emits a single
# warning naming each `input -> canonical` change rather than one warning per
# definition. `depth` guards re-entrancy; the inputs/canonicals accumulate the
# changed pairs.
#
# @keywords internal
# @noRd
.canonRefSink <- new.env(parent = emptyenv())
.canonRefSink$depth <- 0L
.canonRefSink$inputs <- character()
.canonRefSink$canonicals <- character()

# Run `expr` while collecting every reference canonicalization it triggers
# (via `.canonicalizeIdRef()`), then emit ONE consolidated warning naming each
# unique `input -> canonical` change. The consolidated warning is flushed on
# normal completion and, via a calling handler, before an error propagates, so
# a batch that aborts partway still surfaces the canonicalizations that
# happened before the abort (preserving the warning-then-error order the
# per-definition path had). Re-entrant collectors share the outermost sink; only
# the outermost one flushes.
#
# @keywords internal
# @noRd
.collectCanonicalizedRefs <- function(expr) {
  outermost <- .canonRefSink$depth == 0L
  if (outermost) {
    .canonRefSink$inputs <- character()
    .canonRefSink$canonicals <- character()
  }
  .canonRefSink$depth <- .canonRefSink$depth + 1L
  flushed <- FALSE
  flush <- function() {
    if (flushed || !outermost) {
      return(invisible(NULL))
    }
    flushed <<- TRUE
    inputs <- .canonRefSink$inputs
    canonicals <- .canonRefSink$canonicals
    if (length(inputs) > 0L) {
      # Deduplicate by pair, keeping first-seen order, so a reference repeated
      # across several definitions in the batch is named once.
      key <- paste(inputs, canonicals, sep = "\r")
      firstSeen <- !duplicated(key)
      .warnCanonicalizedRefs(inputs[firstSeen], canonicals[firstSeen])
    }
  }
  on.exit(
    {
      .canonRefSink$depth <- .canonRefSink$depth - 1L
    },
    add = TRUE
  )
  withCallingHandlers(
    {
      result <- force(expr)
      flush()
      result
    },
    error = function(cnd) {
      # Flush the collected canonicalizations before the error unwinds the
      # stack; the handler does not catch the error, so it propagates unchanged.
      flush()
    }
  )
}

# Render the consolidated reference-canonicalization warning. Shared by the
# standalone `.canonicalizeIdRef()` path and the batch collector so both emit
# byte-identical text.
#
# @keywords internal
# @noRd
.warnCanonicalizedRefs <- function(inputs, canonicals) {
  # Same fix as `.canonicalizeId()`'s changed-id warning: build the bullet
  # templates and their binding environment via `.canonicalizedIdBullets()`
  # and glue-parse them in a single outer `cli_warn()` call.
  rendered <- .canonicalizedIdBullets(inputs, canonicals)
  cli::cli_warn(
    c(
      "Canonicalized {length(inputs)} referenced id{?s} to a safe form:",
      rendered$bullets
    ),
    .envir = rendered$envir
  )
}

# Reserved Windows device basenames (case-insensitive), never allowed as a
# bare filename on Windows.
.windowsReservedBasenames <- c(
  "con",
  "prn",
  "aux",
  "nul",
  paste0("com", 1:9),
  paste0("lpt", 1:9)
)

# Canonicalize a single id (scalar). NA passes through unchanged so the
# vectorized caller can flag it the same way a malformed value is flagged
# upstream.
#
# @keywords internal
# @noRd
.canonicalizeOneId <- function(id) {
  if (is.na(id)) {
    return(NA_character_)
  }
  # An id becomes a filename (`<id>.json`), so it must fit the filesystem's
  # per-component byte limit. Bound it before the transform so an over-long id
  # aborts with a clear message naming the id and the limit, rather than the
  # opaque `cannot open the connection` the eventual `write_json` would raise.
  # 255 bytes is the common single-component cap (ext4, APFS, NTFS); leave room
  # for the `.json` suffix.
  limit <- .maxDefinitionIdBytes
  nbytes <- nchar(id, type = "bytes")
  if (nbytes > limit) {
    cli::cli_abort(c(
      "Definition id is too long to be a safe filename: {nbytes} bytes \\
      (limit {limit}).",
      "x" = "{.val {id}}",
      "i" = "An id becomes the file {.file <id>.json}; shorten it to at most \\
      {limit} bytes."
    ))
  }
  out <- tolower(id)
  # Trim leading/trailing dots and spaces first (illegal as a trailing segment
  # on Windows, and a leading dot hides the file on Unix), so an edge space is
  # dropped rather than turned into an underscore by the replacement below.
  out <- gsub("^[. ]+|[. ]+$", "", out)
  # Forbidden characters, control characters, and any interior comma or space
  # -> underscore. A comma or space is legal on disk but not a safe id: it makes
  # a fragile filename and breaks the comma-separated reference lists the Excel
  # bridge parses, so canonicalize them out here at the single shared chokepoint.
  out <- gsub("[/\\:*?\"<>|,[:space:][:cntrl:]]", "_", out)
  if (nchar(out) == 0L) {
    return("_")
  }
  # A Windows device name is reserved regardless of any extension, so the
  # segment before the first dot decides it: `con.txt` is as unwritable as
  # `con`. Test that base segment, and suffix the whole id so the reserved
  # base is disarmed while any extension is preserved (`con.txt` -> `con.txt_`).
  if (sub("\\..*$", "", out) %in% .windowsReservedBasenames) {
    out <- paste0(out, "_")
  }
  out
}

# Maximum byte length of a definition id, so `<id>.json` fits the common
# single-path-component filesystem cap (255 bytes on ext4 / APFS / NTFS) with
# room for the `.json` suffix.
.maxDefinitionIdBytes <- 250L

# Find the candidate ids closest to `x` (typo-tolerant). Mirrors ESQmrg's
# `nearest_match`: `utils::adist(ignore.case = TRUE)`, a distance threshold of
# `max(1, min(3, ceiling(nchar(x) / 3)))`, returning at most the `n` closest.
# Returns `character(0)` when there are no candidates or nothing is within
# threshold.
#
# @keywords internal
# @noRd
.nearestMatch <- function(x, candidates, n = 3L) {
  # A non-scalar or NA `x` (e.g. a NULL / length-2 reference that reached an
  # error message builder) has no single nearest match; suggest nothing rather
  # than indexing an empty or multi-row distance matrix.
  if (
    length(candidates) == 0L || !is.character(x) || length(x) != 1L || is.na(x)
  ) {
    return(character(0))
  }
  d <- utils::adist(x, candidates, ignore.case = TRUE)[1, ]
  ord <- order(d)
  thr <- max(1L, min(3L, ceiling(nchar(x) / 3)))
  keep <- ord[d[ord] <= thr]
  candidates[utils::head(keep, n)]
}

# Build a "did you mean '...'?" suffix for a dangling reference `x` against
# the existing `candidates`, or `""` when no candidate is close enough.
# Appended to cross-reference validation messages.
#
# @keywords internal
# @noRd
.suggestSuffix <- function(x, candidates) {
  near <- .nearestMatch(x, candidates)
  if (length(near) == 0L) {
    return("")
  }
  paste0(" (did you mean ", paste0("'", near, "'", collapse = ", "), "?)")
}

# Build a single "did you mean ...?" suffix for a set of dangling references
# `xs` against the existing `candidates`: collects the closest candidate for
# each dangling id, deduplicates, and renders one combined suffix (or `""`).
#
# @keywords internal
# @noRd
.suggestSuffixMulti <- function(xs, candidates) {
  near <- unique(unlist(lapply(xs, function(x) .nearestMatch(x, candidates))))
  if (length(near) == 0L) {
    return("")
  }
  paste0(" (did you mean ", paste0("'", near, "'", collapse = ", "), "?)")
}
