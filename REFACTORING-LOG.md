# Refactoring log

One bullet per commit, newest at the bottom: `- YYYY-MM-DD HH:MM: plain-language one-liner`. See the "Log every commit" rule in `REFACTORING.md`.

- 2026-07-22 11:08: Added the refactoring brief (goal, rules, codebase conventions) and this commit log; renamed leftover "entity" wording to "definition" in a few comments.
- 2026-07-22 12:05: Documented that a script observed-data source runs arbitrary R (security note on loadObservedData and in the vignette).
- 2026-07-22 12:10: Warn once per session when a script observed-data source is sourced; moved rlang to a hard dependency.
- 2026-07-22 12:14: Warn (instead of dropping silently) when a programmatic observed-data source has no data behind it.
- 2026-07-22 12:18: On save, a programmatically added DataSet is now written to a PKML file so it survives a reload.
- 2026-07-22 12:22: Updated the observed-data vignette and NEWS for the round-trip and the script-vs-programmatic safety trade-off.
- 2026-07-22 12:35: Fixed a data-loss bug when saving a programmatic DataSet; the whole batch is validated before any PKML is written.
- 2026-07-22 13:10: Addressed PR review on the observed-data work: keep a session DataSet recoverable if a save aborts, guard name-less and unsafe-named sentinels, warn per project, and align the log format with the rule.
- 2026-07-22 15:00: Unified R6 method and helper casing to camelCase: validationResult methods, Project's parsing-era private methods, and the shared validation check helpers.
- 2026-07-22 15:30: Dropped the severity/kind prefixes from messages catalog entry names (error/stop/warning/validation/message), naming each by what it describes; kept a short context qualifier where two entries would otherwise collide.
- 2026-07-22 16:00: Normalized @family roxygen values to camelCase (spaced and kebab-case ones), leaving singular/plural as-is; updated the matching has_concept() entry in _pkgdown.yml.
- 2026-07-23 11:34: Removed three unused test helpers (testProjectExcelConfigurationsPath, executeWithTestFile, createValidPISheets) from tests/testthat/helpers.R.
- 2026-07-23 11:35: Removed orphan test fixtures unreferenced by any test (ObsDataAciclovir_1/2/3.pkml, ProjectConfiguration-V5.xlsx).
