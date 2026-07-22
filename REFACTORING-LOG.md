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
- 2026-07-22 14:30: Gave the parameter-set entry and parameter-identification `add*` functions a uniform overwrite policy: a duplicate now errors unless `overwrite = TRUE`.
- 2026-07-22 14:45: Extended the overwrite policy to `addOutputPath()`: an existing id errors unless `overwrite = TRUE`.
- 2026-07-22 15:10: Extended the overwrite policy to `addIndividual()`: an existing id errors unless `overwrite = TRUE`.
- 2026-07-22 15:20: Extended the overwrite policy to `addPopulation()`: an existing id errors unless `overwrite = TRUE`.
- 2026-07-22 15:30: Extended the overwrite policy to `addApplication()`: an existing id errors unless `overwrite = TRUE`.
- 2026-07-22 15:45: Extended the overwrite policy to `addParameterSet()` and `addInitialConditions()`: an existing set id errors unless `overwrite = TRUE`.
- 2026-07-22 16:00: Extended the overwrite policy to `addScenario()`: an existing id errors unless `overwrite = TRUE` (distinct from the `overwriteFormulasInSS` model option).
- 2026-07-22 16:20: Extended the overwrite policy to `addPlot()`, `addPlotGrid()`, and `addDataCombined()`: an existing id errors unless `overwrite = TRUE`.
- 2026-07-22 16:45: Extended the overwrite policy to `addObservedData()`: a source whose id (DataSet name or file basename) already exists errors unless `overwrite = TRUE`.
