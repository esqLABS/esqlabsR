# Technical Debt

Findings from the code-quality journey, ranked. Priority is P1 (do first) to P3 (log only). Effort is rough: S is under an hour, M is a few hours, L is a day or more.

## What the audit found clean

Worth recording, because these are the usual suspects and the package does not have them:

- **No dead code.** Every one of the 582 internal function definitions is referenced. The six that no scan finds by name (`print.DefinitionList`, `print.PIParameter`, `print.PIOutputMapping`, `print.PITask`, `format.DefinitionList`, `format.ProjectFieldGroup`) are S3 methods registered in `NAMESPACE` and reached by dispatch.
- **No unused message catalog entries.** All 190 `messages$*` builders in `R/messages.R` are referenced.
- **No boarded-up hacks.** Zero `TODO`, `FIXME`, `HACK`, or `XXX` comments in `R/` or `tests/`. No commented-out code.
- **Error handling is consistent.** User-facing text goes through the `messages` catalog and is raised with `cli::cli_abort()` / `cli_warn()` / `cli_inform()`, as the house rule requires. No bare `stop()` on inline literals.
- **Function signatures are small.** Median 2 arguments across 775 top-level functions; only 9 exceed 10, and those are domain record constructors (a scenario genuinely has 16 fields).

## Debt Ledger

| # | Item | Location | Type | Risk | Effort | Priority | Status |
|---|---|---|---|---|---|---|---|
| B1 | The same scalar non-empty-string guard is written inline 26 times across 8 files, while a function that does exactly this already exists as `.assertScenarioIdArg()`. The name is scenario-specific but the body is generic. | `definition-id.R:439`, `individuals.R:288,303,543,555`, `output-paths.R:178`, `parameter-identification.R:47,63,202,221,233,246,392,1371,1518`, `parameters.R:890,968,1031,1433,1491,1530,1544`, `populations.R:688,926`, `scenarios.R:1028,1954`; helper at `scenarios.R:1756` | duplicated knowledge | low | M | P1 | fixed |
| B2 | The list of allowed individual fields is written out twice, once in the add path and once in the set path, though `populations.R` already demonstrates the fix with its `.populationNumericFields` constant. | `individuals.R:266-274` and `individuals.R:522-530` | duplicated knowledge | medium (the two lists can silently drift apart) | S | P1 | fixed |
| B3 | The numeric-field coercion-and-validate loop is duplicated between individuals and populations; only the field list differs. | `individuals.R:329-337`, `populations.R:738-746` (and the matching set-path copies) | duplicated knowledge | low | S | P1 | fixed |
| B4 | `.runScenariosFromProject()` and `.buildSimulationsFromProject()` open with the same ~18 lines: identical preflight call, identical build call, identical unpacking. | `scenario-execution.R:747-765` and `scenario-execution.R:845-863` | duplicated knowledge | medium (a change to the build sequence must be made twice) | S | P1 | won't fix (see note below) |
| B5 | Adding a field to a scenario means editing the same 16-argument signature in three places. Most other authoring methods on `Project` already forward with `...`; `addScenario` / `setScenario` / `createScenariosFromPKML` are the exceptions. | `project.R:276-313`, `scenarios.R:894-939`, `scenarios.R:947-967` | duplicated knowledge | medium | S | P1 | fixed (now two places) |
| B6 | `importProjectFromExcel()` is 629 lines in one function, the longest in the package by 180 lines. | `project-excel.R:55` | long function | medium (hard to review, hard to test in parts) | L | P2 | fixed (629 -> 366) |
| B7 | `.createScenariosFromPKML_impl()` is 447 lines. | `scenario-from-pkml.R:230` | long function | medium | L | P2 | open |
| B8 | `.validateCrossReferences()` is 314 lines and encloses a 128-line `inScope()` function. | `validation.R:878` | long function | medium | M | P2 | open |
| B9 | 37 functions exceed 100 lines and 94 exceed 60, against a median of 13. | across `R/` | long function | low | L | P3 | open |
| B10 | The "PI task not found" lookup-and-abort, and the task write-back through the section seam, are each duplicated between the add and set paths. | `parameter-identification.R:1761-1769` / `1928-1936`, and `1846-1856` / `2018-2028` | duplicated knowledge | low | S | P2 | open |
| B11 | The `.appendParameterSets()` call block appears three times in the Excel importer. | `project-excel.R:278-286`, `387-395`, `~425` | duplicated knowledge | low | S | P2 | open |
| B12 | The `extendParameterStructure()` merge block appears three times in a row inside one function. | `scenario-execution.R:124-131`, `141-148`, `167-174` | duplicated knowledge | low | S | P2 | open |
| B13 | Comments narrate the code's history instead of describing what it does now, which the repo's own comment rule in `REFACTORING.md` forbids. | `definition-files.R:1235`, `project-excel.R:627`; in tests, `setup.R:3` and `test-project.R:1082` | comment rot | low | S | P2 | fixed |
| B14 | `readxl` is called directly from two files outside the Excel boundary, bypassing the `readExcel()` wrapper in `file-utils.R` (which does not cover `excel_sheets()`). | `simulation.R:180`, `parameters.R:310` | leaky boundary | low | S | P3 | open |
| B15 | The `Project` R6 class exposes 83 public methods. Each is a thin forwarder, so the class itself stays readable, but the interface a user faces is very wide. | `project.R:238-734` | wide interface | low | L | P3 | open (accept) |
| B16 | Roughly 40 lines are shared between the spider-plot and time-profile builders: split-by-`OutputPath`, log-scale axis handling, and `patchwork` assembly. | `sensitivity-spider-plot.R:198-210, 332-340, 411-426` and `sensitivity-time-profiles.R:190-202, 319-326, 408-423` | duplicated knowledge | low | M | P3 | won't fix (do-not-touch area) |

Test-suite findings T1-T6 live in `quality/TESTING.md` and are not repeated here.

### Why B4 was not fixed

The two functions do share 18 lines, but those 18 lines are two calls whose arguments differ (`opName`, and `stopIfFails` versus `canSkip = FALSE`), and the run path needs `simulationRunOptions` back out while the build path does not. A function wrapping them would take nine parameters and its whole body would be "call A, then call B": it hides no decision and adds an interface wider than the duplication it removes. That is the shallow module the deep-module rule warns against, so the duplication stays and this note explains why, rather than the fix being silently skipped.

## Smell Inventory

| Smell | Location | Refactoring | Status |
|---|---|---|---|
| Duplicated guard clause | B1 sites | Extract Method — `.isNonEmptyString()` in `R/utils.R` holds the rule; `.assertScenarioIdArg()` moved there as `.assertNonEmptyString()`. Both the aborting and the error-collecting sites call the predicate. | done |
| Duplicated literal list | B2 | Extract Constant — `.individualNumericFields` and `.individualFields`, beside the existing `.populationNumericFields` | done |
| Duplicated loop | B3 | Extract Method — `.isInvalidNumericField()` in `R/authoring-vectorize.R`, beside its two existing siblings | done |
| Duplicated call sequence | B10, B11, B12 | Extract Method per site | pending |
| Triplicated signature | B5 | Replace the spelled-out signature on the `Project` method with `...`, matching `addIndividual()`; the free function now passes every argument by name | done |
| Long function | B6 | Extract Method — the 260-line section-descriptor table moved to `.excelImportSections()` | done |
| Long function | B7, B8, B9 | Extract Method, guided by the existing section comments inside each function | pending |
| Comment rot | B13 | Rewrite in the present tense; the history belongs in the commit | done |
| Information leakage | B14 | Add an `excelSheetNames()` wrapper to `file-utils.R` and route both callers through it | pending |
| Wide interface | B15 | None proposed. Narrowing it means a breaking API change for a facade that is already thin. | accepted |

## Sprout / Wrap Register

Empty. No code was added beside untested code during this journey.

## Debt Budget and Broken-Windows Policy

- P1 items are fixed in this journey's batches. P2 items are fixed opportunistically when a change already touches the file. P3 items are recorded and left alone.
- No untracked hack: anything deliberately left broken gets a row here, never a bare `TODO`. The package currently has zero of either, and that is the state to hold.
- Structural and behavioral changes never share a commit. A refactoring commit leaves the test suite green and changes no behavior; a behavior change is its own commit.

## Adopted Conventions

Already established in the codebase and confirmed by this audit. Listed so new code matches.

- Authoring verbs are `add*` / `set*` / `remove*` / `rename*` / `duplicate*` / `create*`.
- Every authoring operation is a three-layer chain: exported free function, `Project` method, private `.<verb><Section>_impl`. The free function guards with `validateIsOfType(project, "Project")` and forwards; the method forwards through `private$.impl()`; the `_impl` holds the logic and calls `rlang::local_error_call(.call)` first.
- All section reads and writes go through `private$.getSection()` / `.setSection()`.
- Ids are canonicalized with `.canonicalizeId()`; vectorized arguments go through `.assertIdVector()`, `.assertNoDuplicateIds()`, `.recycleField()`, `.alignAuthoringArgs()`.
- User-facing text is a `messages` catalog entry raised through the `cli` wrappers, never a literal in a `stop()` or `warning()`.
- One plural domain file owns its section and pairs 1:1 with one test file.
- Comments explain why, and describe the code as it is now. No history.
- roxygen uses `@returns`; internal helpers pair `@keywords internal` with `@noRd`.
