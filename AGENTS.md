# esqlabsR — agent & contributor guide

Guidance for working in this repository with coding agents (Claude Code, Copilot, CodeRabbit, …). Tool-neutral; `CLAUDE.md` points here.

## File naming

Follows the [OSPSuite-R](https://github.com/Open-Systems-Pharmacology/OSPSuite-R) pattern:

- All files in `R/` use **kebab-case**: `scenario-execution.R`, `sensitivity-calculation.R`, `parameter-identification.R`.
- Files are organized by domain/functionality, not by class hierarchy.
- Helper collections use a `-utils` suffix: `data-utils.R`, `file-utils.R`, `plots-utils.R`.
- Tests mirror their source file: `R/scenario-execution.R` → `tests/testthat/test-scenario-execution.R`.

## OSP ecosystem dependencies

esqlabsR builds on the Open Systems Pharmacology ecosystem. When unsure about an API, inspect the source code of the dependency and its official vignettes instead of guessing:

- [`ospsuite`](https://github.com/Open-Systems-Pharmacology/OSPSuite-R) — core PBPK simulation interface
- [`ospsuite.utils`](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils) — validation and utility helpers
- [`ospsuite.plots`](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots) — plotting. Version 6 needs `ospsuite` 13 or later and no longer uses `tlf`.
- [`ospsuite.parameteridentification`](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification) — parameter identification

## Coding conventions

- Internal (non-exported) functions are named with a leading dot: `.validateParametersStructure()`, `.getEsqlabsColors()`. The dot marks a function as internal at every call site, so a reader never has to check `NAMESPACE` to know whether they are looking at part of the package's interface.
- Type checking: use `ospsuite.utils::validateIsOfType()` / `ospsuite.utils::isOfType()` instead of native `inherits()` checks.
- Raise user-facing errors, warnings, and messages with `cli::cli_abort()`, `cli::cli_warn()`, and `cli::cli_inform()`, never with base `stop()`, `warning()`, or `message()`. Keep the text in the `R/messages.R` catalog and use cli markup (`{.val {x}}`, `{.arg {name}}`); the header of `R/messages.R` says how to build an entry.
- Do not join values with `paste()` when the function that receives them already does it. cli interpolation lists a vector by itself: `{.val {x}}` gives `"a", "b", and "c"`, and `{.or {.val {x}}}` gives `"a", "b", or "c"`, while `{.val {paste(x, collapse = ", ")}}` quotes the whole list as one value. `ospsuite.utils::cliFormat()` and `cli::cli_abort()` already join their arguments into lines. cli shortens a vector of more than 20 values with `…`; to show every value, pass `cli::cli_vec(x, list("vec-trunc" = Inf))`.
- Use explicit `package::function()` for functions from other packages, also for `ospsuite` and `ospsuite.utils` functions that `NAMESPACE` imports (`ospsuite.utils::validateIsOfType()`, not `validateIsOfType()`). Never use `library()` or `require()` in package code. In tests, call esqlabsR's own functions directly, without a namespace prefix.
- Use vectorized operations for simple vector math. For complex operations, prefer explicit `for` loops over `apply()`/`purrr::map()`.

## Testing

- Test whole structures in one assertion: `expect_equal(result, expected)` over multiple partial checks.
- Compare against independently constructed expected values — never re-use the output of the function under test as the oracle.
- Prefer `expect_snapshot(x)` over `expect_identical(x, <hand-written value>)` when `x` prints readably (a data frame, a list of records, a printed object). The snapshot also checks identity, and the `_snaps/` file shows reviewers the content under test; a change shows up in the PR diff. Read a new snapshot before you accept it: it is the expected value. A printed snapshot checks only what `print()` shows: it rounds doubles to 7 significant digits, and a `print()` method can leave out fields. For exact numbers or the full structure, use `expect_snapshot_value(x, style = "deparse")` or keep `expect_identical()` / `expect_equal()`.
- Test errors and warnings against the specific message, preferably with `expect_snapshot(error = TRUE)`.
- To mock a function from another package with `testthat::local_mocked_bindings()`, pass `.package = "<package>"`. The code calls it as `<package>::fn()`, so a mock in the esqlabsR namespace is never used.
- Do not add tests that cannot fail for a real defect; avoid trivial assertions.
- Use meaningful, domain-specific test data; avoid unexplained literals like `42` or `"foo"`.

## User-visible documentation

The target reader of all user-facing documentation (roxygen `@description`, `@param`, `@details`, vignettes, messages) is a **modeling expert, not a coder or data scientist**. They know PBPK/QSP modeling, scenarios, individuals, and populations; they do not know software-engineering vocabulary.

- Do **not** use developer jargon: "reconcile", "authoring", "scaffold", "orphan", "idempotent", "in-memory vs. on-disk tree", "bound/unbound", "no-op", "side-car", "mutate", "sweep".
- Describe behavior by **what the user sees and does**, not by mechanism. Prefer "Only files with actual changes are re-written, so `git diff` shows exactly the definitions you edited" over "write-if-different reconciliation".
- Say things plainly: "idempotent no-op" → "saving repeatedly is always safe"; "orphan deletion" → "if you removed a scenario, its file is deleted".
- State what a function does *not* do and what to call instead (e.g. saving does not update the Excel files; use `exportProjectToExcel()`).
- Document important APIs in the vignettes, with a description of what they do and when to use them.
- Internal comments and `@keywords internal` docs may stay technical — this rule is about what package users read.

## Pull requests

- Open new PRs as **draft (WIP)**.
- `NEWS.md` describes the difference between the last release and the current state, not the history of the development cycle.
- Every PR that changes what a user of the last release sees needs at least one `NEWS.md` bullet, written for the end user. Add **one bullet per user-visible change**, not one per PR: a PR that changes three things visible to users gets three bullets. Do not combine unrelated changes into a single bullet.
- When a PR changes something added in the current development cycle (already in the development section), edit its existing bullet instead of adding a new one, and only if the bullet is now wrong.
- Skip `NEWS.md` when the PR has no user-facing change compared to the last release: internal refactoring, tests, CI, agent guidance, or a fix for a bug added after the last release.
- Each bullet names the affected function as `functionName()`, says what the user now sees or can do, and ends with the issue number in parentheses, e.g. `(#1056)`. File it under one of the development section's headings: `## Breaking changes`, `## Major changes`, `## New features`, `## Deprecations`, or `## Minor improvements and bug fixes`.

## Documentation / Rd files

- Exported functions need complete roxygen2 docs: a title, `@param` (with types and defaults), `@returns`, `@examples`. The title is the first line of the roxygen block; an explicit `@title` tag is not needed.
- Internal functions need minimal documentation plus `@keywords internal` and `@noRd`.
- When updating roxygen documentation, do **not** automatically re-generate the `man/*.Rd` files (`devtools::document()`). Rd generation is done once at the end of the work, before the PR is marked ready for review.
