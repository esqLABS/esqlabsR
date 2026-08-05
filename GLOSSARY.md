# Glossary

Some domain terms in the 6.0.0 project model carry more than one meaning: reading "scenario" or "parameters" in an unfamiliar file doesn't tell you which one you have. This table disambiguates them by pointing each meaning at the code name that actually carries it.

This is descriptive only. No renames are implied or scheduled by this document; see issue [#1226](https://github.com/esqLABS/esqlabsR/issues/1226) for the architecture audit that identified these overlaps.

| Term | Meaning | Code name |
|---|---|---|
| Scenario definition | The authored record a user means by "my IV scenario": a model file plus its individual/population/application and parameter-set/initial-condition/output-path references, plus timing. What `addScenario()` creates and `project$definitions$scenarios` holds. | `scenarios` project section; `addScenario()` / `setScenario()` / `removeScenario()` |
| Scenario object | The in-memory typed value a parsed scenario definition becomes. | `Scenario()` constructor, `as.list.Scenario()`, `print.Scenario()` — all in `R/scenarios.R` |
| Scenario execution unit | What a scenario definition resolves to at run time: a loaded, fully-parameterized simulation paired with its population, ready to hand to `ospsuite::simulate()`. | `.prepareScenario()`'s `list(simulation, population)` return (an `ospsuite::Simulation` + `ospsuite::Population`), in `R/scenario-execution.R` |
| Parameter set | A named, reusable bundle of model-parameter overrides, attached to a scenario, individual, or application. | `parameterSets` project section; `addParameterSet()` / `addParameterEntry()` in `R/parameters.R` |
| Initial condition | A named, reusable bundle of molecule start-value overrides — structurally parallel to a parameter set, but applied via `ospsuite::setQuantityValuesByPath()` instead of as a model parameter. | `initialConditions` project section; `addInitialConditions()` / `addInitialConditionEntry()` in `R/parameters.R` |
| PI parameter | One variable a parameter-identification task optimizes: an OSPS path plus its bounds and start value. | `PIParameter()` in `R/parameter-identification.R` |
| Parameter path | A raw `ospsuite`-notation string identifying one quantity in the model tree (e.g. `"Organism\|Liver\|Volume"`). No dedicated esqlabsR type — used directly wherever a path is expected. | plain character string ("OSPS notation") — e.g. a parameter-set entry's `containerPath` + `parameterName`, or a PI parameter's `path` |
