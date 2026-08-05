# Improve Code Quality Plan

## Context

Started 2026-08-04 on branch `felixmil/infallible-curie-fcd278` (86 commits ahead of `main`), during the 6.0.0 JSON-project migration described in `REFACTORING.md`.

- **What the package does.** esqlabsR builds, runs, and reports PBPK simulation projects on top of the OSP Suite. Users are PBPK modelers, not software engineers. The worst failure is silent: a scenario that runs but applies the wrong parameters, or a `saveProject()` that drops a definition a modeler authored.
- **Stack.** R package. R6 `Project` object, a `definitions/` tree of one-per-definition JSON files as the source of truth, Excel as a secondary import/export format. Dependencies: `ospsuite`, `ospsuite.utils`, `ospsuite.parameteridentification`, `jsonlite`, `readxl`, `writexl`, `cli`, `ggplot2`, `tlf`. No database, no network calls, no server process.
- **Size.** 41 files and 31,322 lines under `R/`; 39 files and 27,139 lines under `tests/testthat/`; 790 function definitions, 116 exported.
- **Test status at intake.** `devtools::test()` is green: `FAIL 0 | WARN 49 | SKIP 1 | PASS 3344`.
- **Highest-churn current files** (last 300 commits): `messages.R` (88), `project.R` (29), `project-lifecycle.R` (23).
- **Scope chosen.** Whole package, findings ranked, applied in approved batches. Sensitivity-analysis files stay read-only per `REFACTORING.md`.

Artifacts live in `quality/`, not `docs/`: `docs/` is pkgdown's build output here, listed in both `.gitignore` and `.Rbuildignore`, so anything written there would be untracked and overwritten by `pkgdown::build_site()`.

## Phase Status

| Phase | Skill | Status | Artifact | Date |
|---|---|---|---|---|
| 1 — Build the safety net | working-with-legacy-code | done | TESTING.md + TECH-DEBT.md (GATE) | 2026-08-04 |
| 2 — Make the code readable | clean-code | done | TECH-DEBT.md | 2026-08-04 |
| 3 — Apply named refactorings | refactoring-patterns | done | TECH-DEBT.md | 2026-08-04 |
| 4 — Reduce complexity | software-design-philosophy | done | TECH-DEBT.md | 2026-08-04 |
| 5 — Draw the architecture boundary | clean-architecture | done | TECH-DEBT.md (no ARCHITECTURE.md, see below) | 2026-08-04 |
| 6 — Lock in the habits | pragmatic-programmer | done | TECH-DEBT.md | 2026-08-04 |
| 7 — Make it survive production | release-it | skipped: no runtime service, no outbound calls | — | |
| 8 — Size for real load | system-design | skipped: library, no request load | — | |
| 9 — Get the data layer right | ddia-systems | skipped: no database, no concurrency | — | |
| 10 — Test-suite quality (added) | — | done | TESTING.md | 2026-08-04 |
| Optional — Domain language | domain-driven-design | skipped: domain language already used throughout | — | |

Statuses: pending · in-progress · awaiting-evidence · done · deferred: reason · skipped: reason

Every phase's P1 findings are applied; the P2 and P3 rows that remain are tracked in the Debt Ledger, not left implicit.

Phase 5 produced no Dependency Rule violations to track, so no `ARCHITECTURE.md` was created; its two findings are Debt Ledger rows instead.

## What was applied

| Commit | Change | Net lines |
|---|---|---|
| `a4900ec0` | B2, B3 — shared individual field constants and one numeric-field check | −60 |
| `11cb3121` | B5 — `addScenario` forwards through `...`, three signatures become two | −30 |
| `6edfe95d` | B1 — 26 inline string guards routed through `.isNonEmptyString()` | −72 |
| `9fb9b281` | T1, T4, T5, T6 — shared PI test fixtures, `.localSnapshotOptions()` moved, snapshot test renamed | −150 |
| `c63f3dfc` | B6, B13 — the Excel section table extracted, history comments rewritten | −0 (moved) |
| `11f078a1` | B10, B12, B17 — one PI-task lookup, one parameter-set merge loop, and the illegal `:::` call removed | −45 |
| `624b406b` | B8 (part) — the duplicated parameter-set reference check shared | −20 |

`R CMD check` was run once the code changes were in: `Status: 1 NOTE`, no errors and no warnings. The NOTE is B18, a pre-existing one about long snapshot file paths.

## Key Decisions

| Date | Phase | Decision | Rationale |
|---|---|---|---|
| 2026-08-04 | intake | Skip phases 7-9 | They assume a running service with outbound calls, request load, and a database. This is an R package: none of the three exist. |
| 2026-08-04 | intake | Add a phase 10 for test-suite quality | The nine listed phases measure test *coverage* but never test *bloat*, which is one of the three problems named at intake. |
| 2026-08-04 | intake | Artifacts go in `quality/`, not `docs/` | `docs/` is pkgdown output, gitignored and rebuilt. |
| 2026-08-04 | 1 | Phase 1 passes as a gate without writing new characterization tests | 3344 tests pass with zero failures and every module that later phases touch already has a mirrored test file. The gate exists to guarantee changes are verifiable; that guarantee already holds. |
| 2026-08-04 | 1 | Pin, do not fix, any wrong behavior found while auditing | Callers may depend on a quirk. Anything suspicious becomes a Debt Ledger row, not a silent correction. |
| 2026-08-04 | 5 | Do not abstract `ospsuite` behind an owned interface | It is the domain library the package exists to extend, not a swappable vendor. Wrapping it would add an indirection layer with one implementation. |
| 2026-08-04 | 3 | Leave `R/sensitivity-*.R` duplication unfixed, logged only | `REFACTORING.md` marks those files do-not-touch. |

## Next Actions

- [ ] Review the ranked Debt Ledger in `quality/TECH-DEBT.md` and approve batch 1 (Felix)
- [ ] Apply batch 1 as single-purpose commits, suite green between each (Claude)
- [ ] Re-run `devtools::test()` and `devtools::check()` after each batch (Claude)
