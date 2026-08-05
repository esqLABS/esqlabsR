# Testing

## Test Strategy

`testthat` edition 3, run with `NOT_CRAN=true Rscript -e 'devtools::test()'`. One test file per source file, mirroring `R/`. Shared fixtures live in `tests/testthat/helpers.R` (34 builder functions) and session-wide setup in `tests/testthat/setup.R`. Real PK-Sim model files and a bundled test project under `tests/testthat/data` back the integration-level tests; `vdiffr` snapshots back the plotting tests.

Baseline at 2026-08-04: `FAIL 0 | WARN 49 | SKIP 1 | PASS 3344`. Now: `FAIL 0 | WARN 11 | SKIP 1 | PASS 3401`, with all 11 remaining warnings coming from dependencies.

The full suite gets killed partway when run in one invocation in this environment, so it is verified in three `filter =` chunks whose counts are summed.

## Safety Net Map

Every source file has a mirrored test file, bar the three noted below, and the suite is green.

| Area | Source | Pinned behaviors | Test files | Gaps |
|---|---|---|---|---|
| Project object and lifecycle | `project.R`, `project-lifecycle.R`, `project-snapshot.R` | Load from folder or file, explicit save, dirty tracking, reload, snapshot round trip | `test-project.R`, `test-project-lifecycle.R`, `test-project-snapshot.R`, `test-project-field-group.R` | none |
| Definition persistence | `definition-files.R`, `definition-id.R`, `definition-list.R` | Per-definition file tree read/write, id canonicalization, stale-file removal | `test-definition-files.R`, `test-definition-files-sections.R`, `test-definition-id.R`, `test-definition-list.R` | none |
| Authoring API | `scenarios.R`, `individuals.R`, `populations.R`, `parameters.R`, `observed-data.R`, `output-paths.R`, `applications.R`, `plots.R` | add / set / remove / rename / duplicate per section, vectorized ids, overwrite semantics | one `test-<domain>.R` each | none |
| Validation | `validation.R` plus per-section validators | Section validators, cross-reference phase, critical-vs-warning classification | `test-validation.R` | none |
| Excel bridge | `project-excel.R`, `import-legacy-snapshot.R` | Import legacy workbook, export, round trip, sync detection | `test-project-excel.R`, `test-import-legacy-snapshot.R` | none |
| Simulation and results | `scenario-execution.R`, `scenario-results.R`, `simulation.R`, `parallel.R` | Scenario build, run, result collection, failure handling | `test-scenario-execution.R`, `test-scenario-results.R`, `test-simulation.R`, `test-parallel.R` | none |
| Parameter identification | `parameter-identification.R` | PI task build, run, output mappings, bounds and units | `test-parameter-identification.R` | none |
| Plotting | `create-plots.R`, `plots.R`, `plots-utils.R`, `data-combined.R` | Plot and grid construction, axis handling, `DataCombined` assembly | `test-create-plots.R`, `test-plots.R`, `test-plots-utils.R`, `test-data-combined.R` | none |
| Sensitivity analysis | `sensitivity-*.R` | Calculation, spider / tornado / time-profile plots | four `test-sensitivity-*.R` files | do-not-touch area per `REFACTORING.md`; tests kept as-is |
| Message catalog | `messages.R` | — | none directly | All 190 catalog entries are referenced from `R/`; wording is pinned indirectly by `expect_snapshot()` assertions across the suite |

## Characterization Backlog

- [x] `R/definition-list.R` — pinned by `test-definition-list.R`: wrapping and unwrapping, read transparency, the pluralized print header, and the abort on `[[<-` / `$<-` / `[<-`
- [x] `R/project-field-group.R` — pinned by `test-project-field-group.R`: reads and writes through the field closures, a handle writing through live state, per-field closure capture, and both read-only handlers

Both are done. `R/globals.R`, `R/messages.R` and `R/zzz.R` have no test file of their own by design: the first two are data, and the catalog's wording is pinned indirectly by the `expect_snapshot()` assertions across the suite.

## Test-Suite Quality Findings (Phase 10)

Ranked. These are about test *bloat and weakness*, not coverage.

| # | Finding | Location | Cost | Fix |
|---|---|---|---|---|
| T1 | **Fixed.** A ~15-line `PITask(...)` fixture was rebuilt verbatim in 12 tests, each differing by one field. | `test-parameter-identification.R`, throughout | Adding a `PITask` field meant editing 12 blocks | `testPITask()`, `testPIParameter()`, `testPIOutputMapping()` and `testObservedDataId` now live in `helpers.R`; each test passes only what it varies. The file went from 3352 to 3163 lines. |
| T2 | **Fixed.** 49 warnings were raised during the run and never asserted, so a regression that stopped warning would have passed silently. Now 11, every one of them from a dependency rather than from esqlabsR. | whole suite | Warning behaviour was unprotected, and the noise hid a test that asserted the wrong warning (see below) | Two fixes, applied per site. Where the non-canonical id was incidental, it is now written the way the project stores it, so no warning fires and nothing is lost. Where the warning is the point of the test, it is asserted with `expect_warning()`. |
| T3 | **Fixed.** The Excel test file repeated the same setup 45 times: a three-line copy of the legacy Excel fixture (28x), a three-line load of the example project (11x, reimplementing the existing `exampleProject()` helper), an eight-line export-and-reimport round trip (6x), and a 15-column scenario-sheet data frame (3x). | `test-project-excel.R` | 3948-line file, 165 lines of it copied setup | `localExcelProjectDir()`, `excelRoundTrip()` and `scenarioSheetRow()` added to `helpers.R`; the example-project sites now call the helper that already existed. File down to 3781 lines. |
| T4 | **Partly fixed.** `.localSnapshotOptions()` was byte-identical in three sensitivity test files and now lives in `helpers.R`. The `sensFixture` / `sensFixtureMultiple` memoized fixtures around it stay duplicated on purpose. | `test-sensitivity-calculation.R`, `test-sensitivity-spider-plot.R`, `test-sensitivity-time-profiles.R` | 45 lines removed | Each file's `sensFixture` caches a PK-Sim native session deliberately per file, and its comments say so. Sharing one cache across files would change that isolation for a saving of a few lines, so it stays. |
| T5 | **Fixed.** `test-snapshot.R` did not mirror its source file name `R/project-snapshot.R`, and its header pointed at the wrong file. | `tests/testthat/test-project-snapshot.R` | Broke the 1:1 naming rule; `devtools::test_active_file("R/project-snapshot.R")` could not find it | Renamed, with its `_snaps/` file, and the header corrected |
| T6 | **Fixed.** Comments in tests narrated history rather than describing the current check. | `tests/testthat/setup.R`, `test-project.R` | Contradicted the repo's own comment rule in `REFACTORING.md` | Rewritten to state the current constraint |

`test-definition-files-sections.R` has no matching source file, but it is a deliberate second file for the 1377-line `R/definition-files.R`. Left as is.

### What the warning noise was hiding

One test was passing for the wrong reason. `test-parameter-identification.R`, "removeOutputPath() warns when the path is referenced only by a PI mapping", added the output path as `pionlypath` but then wrote `PIOnlyPath` in the mapping and in the call under test, and asserted `expect_warning(..., "PIOnlyPath")`. That string appears only in the *canonicalization* warning (`"PIOnlyPath" -> "pionlypath"`), never in the "still referenced by 1 parameter identification task" warning the test is named for. The check the test existed to make was therefore never made: had the PI branch of `.warnIfReferenced()` stopped working entirely, the test would still have gone green. It now uses the canonical spelling throughout and asserts the real message.

This is the argument for keeping a suite warning-free. The one genuinely wrong assertion was indistinguishable from 48 harmless ones until the harmless ones were gone.

### The 11 remaining warnings

None originate in esqlabsR, and none are suppressed, because suppressing them would hide information that belongs to somebody.

- **2 x `plotIndividualTimeProfile()` was deprecated in ospsuite 12.4.2** (`test-create-plots.R:91`, `test-plots-utils.R:93`). ospsuite will remove the function in 14.0 and the replacement is not a drop-in. Tracked as B19 in `quality/TECH-DEBT.md`; left visible on purpose, since the warning is the only thing carrying the deadline.
- **9 base-R warnings** (`test-sensitivity-time-profiles.R:259` and `:338`): eight `NAs introduced by coercion` and one `longer object length is not a multiple of shorter object length`. All come from `ospsuite.utils::validateEnumValue()` / `enumGetKey()`, which compares a value against an enum with `==` without matching lengths first. Reached from `R/sensitivity-time-profiles.R`, which is a do-not-touch area, and the fix belongs upstream in any case.

## CI Gates

`devtools::test()` green and `devtools::check()` clean before pushing, per the repo's R conventions. No coverage threshold is enforced today; none is proposed here.
