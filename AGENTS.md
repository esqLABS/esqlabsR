# esqlabsR — agent & contributor guide

Guidance for working in this repository with coding agents (Claude Code, Copilot, CodeRabbit, …). Tool-neutral; `CLAUDE.md` points here.

## File naming

Follows the [OSPSuite-R](https://github.com/Open-Systems-Pharmacology/OSPSuite-R) pattern:

- All files in `R/` use **kebab-case**: `scenario-configuration.R`, `sensitivity-calculation.R`, `validation-scenarios.R`.
- Files are organized by domain/functionality, not by class hierarchy.
- Helper collections use a `utilities-` prefix: `utilities-data.R`, `utilities-file.R`, `utilities-scenarios.R`.
- Tests mirror their source file: `R/scenario-configuration.R` → `tests/testthat/test-scenario-configuration.R`.

## User-visible documentation

The target reader of all user-facing documentation (roxygen `@description`, `@param`, `@details`, vignettes, messages) is a **modeling expert, not a coder or data scientist**. They know PBPK/QSP modeling, scenarios, individuals, and populations; they do not know software-engineering vocabulary.

- Do **not** use developer jargon: "reconcile", "authoring", "orphan", "idempotent", "in-memory vs. on-disk tree", "bound/unbound", "no-op", "side-car", "mutate".
- Describe behavior by **what the user sees and does**, not by mechanism. Prefer "Only files with actual changes are re-written, so `git diff` shows exactly the definitions you edited" over "write-if-different reconciliation".
- Say things plainly: "idempotent no-op" → "saving repeatedly is always safe"; "orphan deletion" → "if you removed a scenario, its file is deleted".
- State what a function does *not* do and what to call instead (e.g. saving does not update the Excel files; use `exportProjectToExcel()`).
- Internal comments and `@keywords internal` docs may stay technical — this rule is about what package users read.

## Pull requests

- Open new PRs as **draft (WIP)**.

## Documentation / Rd files

- When updating roxygen documentation, do **not** automatically re-generate the `man/*.Rd` files (`devtools::document()`). Rd generation is done once at the end of the work, before the PR is marked ready for review.
