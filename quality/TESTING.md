# Testing

## Test Strategy

`testthat` edition 3, run with `NOT_CRAN=true Rscript -e 'devtools::test()'`. One test file per source file, mirroring `R/`. Shared fixtures live in `tests/testthat/helpers.R` (27 builder functions) and session-wide setup in `tests/testthat/setup.R`. Real PK-Sim model files and a bundled test project under `tests/testthat/data` back the integration-level tests; `vdiffr` snapshots back the plotting tests.

Baseline at 2026-08-04: `FAIL 0 | WARN 49 | SKIP 1 | PASS 3344`.

## Safety Net Map

The safety net is already in place: every source file that later phases touch has a mirrored test file and the suite is green. The gate is satisfied by the existing suite, not by newly written characterization tests.

| Area | Source | Pinned behaviors | Test files | Gaps |
|---|---|---|---|---|
| Project object and lifecycle | `project.R`, `project-lifecycle.R`, `project-snapshot.R` | Load from folder or file, explicit save, dirty tracking, reload, snapshot round trip | `test-project.R` (1318), `test-project-lifecycle.R`, `test-snapshot.R` | `project-field-group.R` has no test file and is never named in any test |
| Definition persistence | `definition-files.R`, `definition-id.R`, `definition-list.R` | Per-definition file tree read/write, id canonicalization, stale-file removal | `test-definition-files.R`, `test-definition-files-sections.R`, `test-definition-id.R` | `definition-list.R` has no test file; exercised only indirectly through `test-project*.R` |
| Authoring API | `scenarios.R`, `individuals.R`, `populations.R`, `parameters.R`, `observed-data.R`, `output-paths.R`, `applications.R`, `plots.R` | add / set / remove / rename / duplicate per section, vectorized ids, overwrite semantics | one `test-<domain>.R` each | none |
| Validation | `validation.R` plus per-section validators | Section validators, cross-reference phase, critical-vs-warning classification | `test-validation.R` (1390) | none |
| Excel bridge | `project-excel.R` (4400), `import-legacy-snapshot.R` | Import legacy workbook, export, round trip, sync detection | `test-project-excel.R` (3938), `test-import-legacy-snapshot.R` | none |
| Simulation and results | `scenario-execution.R`, `scenario-results.R`, `simulation.R`, `parallel.R` | Scenario build, run, result collection, failure handling | `test-scenario-execution.R`, `test-scenario-results.R`, `test-simulation.R`, `test-parallel.R` | none |
| Parameter identification | `parameter-identification.R` (2029) | PI task build, run, output mappings, bounds and units | `test-parameter-identification.R` (3352) | none |
| Plotting | `create-plots.R`, `plots.R`, `plots-utils.R`, `data-combined.R` | Plot and grid construction, axis handling, `DataCombined` assembly | `test-create-plots.R`, `test-plots.R`, `test-plots-utils.R`, `test-data-combined.R` | none |
| Sensitivity analysis | `sensitivity-*.R` | Calculation, spider / tornado / time-profile plots | four `test-sensitivity-*.R` files | do-not-touch area per `REFACTORING.md`; tests kept as-is |
| Message catalog | `messages.R` (1955) | — | none directly | All 190 catalog entries are referenced from `R/`; wording is pinned indirectly by `expect_snapshot()` assertions across the suite |

## Characterization Backlog

- [ ] `R/definition-list.R` — the read-only `DefinitionList` wrapper (147 lines): `format`, `print`, and subsetting behavior are unpinned (low risk, low effort)
- [ ] `R/project-field-group.R` — the field-group accessor and its read-only error (132 lines): never referenced from any test (low risk, low effort)

## Test-Suite Quality Findings (Phase 10)

Ranked. These are about test *bloat and weakness*, not coverage.

| # | Finding | Location | Cost | Fix |
|---|---|---|---|---|
| T1 | **Fixed.** A ~15-line `PITask(...)` fixture was rebuilt verbatim in 12 tests, each differing by one field. | `test-parameter-identification.R`, throughout | Adding a `PITask` field meant editing 12 blocks | `testPITask()`, `testPIParameter()`, `testPIOutputMapping()` and `testObservedDataId` now live in `helpers.R`; each test passes only what it varies. The file went from 3352 to 3163 lines. |
| T2 | 49 warnings are raised during the run and never asserted. Tests tolerate them instead of pinning them, so a regression that stops warning passes silently. | whole suite; mostly id-canonicalization and cross-reference warnings | Warning behavior is unprotected | Wrap the intended ones in `expect_snapshot()` / `expect_warning()`; silence the incidental ones at the call site |
| T3 | The Excel test file repeats a 12-line project-setup block 4 times and a 10-line block 3 times, among others. | `test-project-excel.R` 34-52 / 105-116 / 207-218 / 242-255; 1721-1730 / 1768-1778 / 1935-1945; 2648-2659 / 2673-2684 / 2700-2708 | 3938-line file is hard to navigate | Extract the repeated setups into local builders in `helpers.R` |
| T4 | **Partly fixed.** `.localSnapshotOptions()` was byte-identical in three sensitivity test files and now lives in `helpers.R`. The `sensFixture` / `sensFixtureMultiple` memoized fixtures around it stay duplicated on purpose. | `test-sensitivity-calculation.R`, `test-sensitivity-spider-plot.R`, `test-sensitivity-time-profiles.R` | 45 lines removed | Each file's `sensFixture` caches a PK-Sim native session deliberately per file, and its comments say so. Sharing one cache across files would change that isolation for a saving of a few lines, so it stays. |
| T5 | **Fixed.** `test-snapshot.R` did not mirror its source file name `R/project-snapshot.R`, and its header pointed at the wrong file. | `tests/testthat/test-project-snapshot.R` | Broke the 1:1 naming rule; `devtools::test_active_file("R/project-snapshot.R")` could not find it | Renamed, with its `_snaps/` file, and the header corrected |
| T6 | **Fixed.** Comments in tests narrated history rather than describing the current check. | `tests/testthat/setup.R`, `test-project.R` | Contradicted the repo's own comment rule in `REFACTORING.md` | Rewritten to state the current constraint |

`test-definition-files-sections.R` has no matching source file, but it is a deliberate second file for the 1377-line `R/definition-files.R`. Left as is.

## CI Gates

`devtools::test()` green and `devtools::check()` clean before pushing, per the repo's R conventions. No coverage threshold is enforced today; none is proposed here.
