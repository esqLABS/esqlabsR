# esqlabsR — agent & contributor guide

Guidance for working in this repository with coding agents (Claude Code,
Copilot, CodeRabbit, …). Tool-neutral; `CLAUDE.md` points here.

## File naming

Follows the
[OSPSuite-R](https://github.com/Open-Systems-Pharmacology/OSPSuite-R)
pattern:

- All files in `R/` use **kebab-case**: `scenario-configuration.R`,
  `sensitivity-calculation.R`, `validation-scenarios.R`.
- Files are organized by domain/functionality, not by class hierarchy.
- Helper collections use a `utilities-` prefix: `utilities-data.R`,
  `utilities-file.R`, `utilities-scenarios.R`.
- Tests mirror their source file: `R/scenario-configuration.R` →
  `tests/testthat/test-scenario-configuration.R`.

## OSP ecosystem dependencies

esqlabsR builds on the Open Systems Pharmacology ecosystem. When unsure
about an API, inspect the source code of the dependency and its official
vignettes instead of guessing:

- [`ospsuite`](https://github.com/Open-Systems-Pharmacology/OSPSuite-R)
  — core PBPK simulation interface
- [`ospsuite.utils`](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils)
  — validation and utility helpers
- [`tlf`](https://github.com/Open-Systems-Pharmacology/TLF-Library) —
  plotting
- [`ospsuite.parameteridentification`](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification)
  — parameter identification

## Coding conventions

- Internal (non-exported) functions are named with a leading dot:
  [`.validateParametersStructure()`](https://esqlabs.github.io/esqlabsR/dev/reference/dot-validateParametersStructure.md),
  `.getEsqlabsColors()`. The dot marks a function as internal at every
  call site, so a reader never has to check `NAMESPACE` to know whether
  they are looking at part of the package’s interface.
- Type checking: use
  [`ospsuite.utils::validateIsOfType()`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/validateIsOfType.html)
  /
  [`ospsuite.utils::isOfType()`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/isOfType.html)
  instead of native [`inherits()`](https://rdrr.io/r/base/class.html)
  checks.
- All user-facing messages, warnings, and errors live in `R/messages.R`
  as dedicated functions using
  [`ospsuite.utils::cliFormat()`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/cliFormat.html)
  with cli markup (`{.val {x}}`, `{.arg {name}}`). Never inline message
  strings or build them with
  [`paste()`](https://rdrr.io/r/base/paste.html).
- Use explicit `package::function()` for functions from other packages.
  Never use [`library()`](https://rdrr.io/r/base/library.html) or
  [`require()`](https://rdrr.io/r/base/library.html) in package code. In
  tests, call esqlabsR’s own functions directly, without a namespace
  prefix.
- Use vectorized operations for simple vector math. For complex
  operations, prefer explicit `for` loops over
  [`apply()`](https://rdrr.io/r/base/apply.html)/[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html).

## Testing

- Test whole structures in one assertion:
  `expect_equal(result, expected)` over multiple partial checks.
- Compare against independently constructed expected values — never
  re-use the output of the function under test as the oracle.
- Test errors and warnings against the specific message, preferably with
  `expect_snapshot(error = TRUE)`.
- Do not add tests that cannot fail for a real defect; avoid trivial
  assertions.
- Use meaningful, domain-specific test data; avoid unexplained literals
  like `42` or `"foo"`.

## User-visible documentation

The target reader of all user-facing documentation (roxygen
`@description`, `@param`, `@details`, vignettes, messages) is a
**modeling expert, not a coder or data scientist**. They know PBPK/QSP
modeling, scenarios, individuals, and populations; they do not know
software-engineering vocabulary.

- Do **not** use developer jargon: “reconcile”, “authoring”, “scaffold”,
  “orphan”, “idempotent”, “in-memory vs. on-disk tree”, “bound/unbound”,
  “no-op”, “side-car”, “mutate”.
- Describe behavior by **what the user sees and does**, not by
  mechanism. Prefer “Only files with actual changes are re-written, so
  `git diff` shows exactly the definitions you edited” over
  “write-if-different reconciliation”.
- Say things plainly: “idempotent no-op” → “saving repeatedly is always
  safe”; “orphan deletion” → “if you removed a scenario, its file is
  deleted”.
- State what a function does *not* do and what to call instead
  (e.g. saving does not update the Excel files; use
  `exportProjectToExcel()`).
- Document important APIs in the vignettes, with a description of what
  they do and when to use them.
- Internal comments and `@keywords internal` docs may stay technical —
  this rule is about what package users read.

## Pull requests

- Open new PRs as **draft (WIP)**.
- Add one `NEWS.md` bullet per PR with user-facing changes, summarizing
  them for the end user. Skip the bullet when a PR has none.

## Documentation / Rd files

- Exported functions need complete roxygen2 docs: `@title`, `@param`
  (with types and defaults), `@return`, `@examples`.
- Internal functions need minimal documentation plus
  `@keywords internal` and `@noRd`.
- When updating roxygen documentation, do **not** automatically
  re-generate the `man/*.Rd` files
  ([`devtools::document()`](https://devtools.r-lib.org/reference/document.html)).
  Rd generation is done once at the end of the work, before the PR is
  marked ready for review.
