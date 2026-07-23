---
description: Test whether a legacy Excel esqlabsR project migrates cleanly to the 6.0.0 JSON model (importer + hand-authoring), and regression-check the known migration issues.
argument-hint: SOURCE=<local path or owner/repo> ESQLABSR_REF=<branch|tag|sha>
---

You are running a **migration test** of the esqlabsR JSON-based project model. Goal: recreate a legacy Excel-based esqlabsR project under the new 6.0.0 API two independent ways, compare them, run them, and report every limitation, bug, and friction point about the new API. Work in a fresh local repo; do not modify the source project.

## Inputs

Parse from `$ARGUMENTS` (both may be omitted; if so, ask for them before starting):

- `SOURCE` = a local legacy Excel project directory, OR a `owner/repo` GitHub repo containing one.
- `ESQLABSR_REF` = the esqlabsR ref to test (e.g. `json-based-project`, a tag, or a commit sha). Default `json-based-project`.

## Rules

- **Install esqlabsR exactly as a user would**: `remotes::install_github("esqLABS/esqlabsR@<ESQLABSR_REF>")` into a temp library, and record the resolved commit sha. Do not rely on whatever is already installed; a stale build invalidates the test. Anchor every finding to that sha.
- **Confidentiality**: the source project may be confidential. In every artifact you produce (issue text, shareable logs, summaries), never name the source project, its repo, its domain, or any project-specific id. For a reproducible example prefer esqlabsR's own bundled fixture `tests/testthat/data/TestProjectExcel/` (standard legacy layout) or a `file:line` reference, not the source data.
- Keep the source Excel project pristine; all outputs go under a new working dir.
- Confirm the runtime works before trusting run results: `ospsuite::loadSimulation()` on the project's `.pkml` must succeed (the .NET backend must be live). If it does not, say so and stop; never report simulated results you could not produce.

## Steps

1. **Scaffold.** Create a fresh working dir (git repo). If `SOURCE` is a GitHub repo, fetch its Excel config, model `.pkml`(s), data files, and population CSVs via `gh api` (use the raw `Accept: application/vnd.github.raw` header for files over ~1 MB; the base64 contents route truncates them). Lay them out as a legacy Excel project under `excel-source/`.

2. **Read what the Excel actually holds.** Dump every sheet of every `*.xlsx` with `readxl`. Enumerate the real content: scenarios and their references (individual/population/application/parameter-sets/output-paths), time grids, steady-state; individuals; populations; applications; model-parameter sheets; PI task; plots; observed-data references. Do not assume file names match content; some projects carry template content.

3. **Path A: automated import.** `importProjectFromExcel(<config>.xlsx, outputDir = "imported", overwrite = TRUE)`, then `loadProject()`, then `validateProject()`. Capture verbatim: every warning, abort, canonicalization, and each definition-section count. Flag anything dropped (section = 0 but Excel had content) or mangled.

4. **Path B: hand-author.** `initProject("authored", type = "minimal", createExcel = FALSE)`, `loadProject()`, then rebuild every definition from step 2 with the `add*()` API in dependency order (output paths -> parameter sets/entries -> individual(s) -> populations -> applications -> scenarios -> PI -> plots -> observed data). Copy the `.pkml`(s) into `Models/Simulations/` and data into `Data/`, `saveProject()`, `validateProject()`. Encode the correct intent where Path A produced something broken.

5. **Diff.** Normalize JSON key order and compare `imported/definitions/` vs `authored/definitions/`. Report files only-in-one and content differences; classify each as a Path A bug, a Path B choice, or an importer/authoring inconsistency.

6. **Run.** `runScenarios()` on the runnable scenarios of each tree; confirm equivalent results. Expect deliberately-broken scenarios to fail and say how each path reports it.

7. **Regression check (known issues from issues #1118 and #1157).** Confirm the status of each as fixed / still-broken, with the exact current message:
   - Importer builds an `Application` per protocol sheet in a one-sheet-per-protocol `Applications.xlsx` (not only from an `ApplicationProtocols` sheet). *(Was: applications = 0.)*
   - Importer parses the multi-sheet `PITaskName`-keyed parameter-identification layout (not only a `PITasks` sheet). *(Was: PI = 0.)*
   - Importer accepts a `Scenarios.xlsx` lacking `OverwriteFormulasInSS` (defaults it, does not abort). *(Was: hard abort.)*
   - Importer imports, or explicitly warns it skipped, the configured observed-data file. *(Was: observedData = 0, silent.)*
   - Importer links an individual to its individual-specific parameter set. *(Was: link dropped.)*
   - Parameter-set ids are canonicalized to filename-safe, not only lowercased. *(Was: comma/space kept in filename.)*
   - Quoted multi-value cells (`"A", "B", "C, with comma"`) parse correctly. *(Was: quotes kept, comma split.)*
   - `addParameterEntry(units=)` and the importer agree on "no unit"; `addPopulation()` accepts `proteinOntogenies`; `steadyState=FALSE` serializes the same from both paths. *(Round-trip encoding.)*
   - `addPITask()` can be created empty then filled; `PIOutputMapping(outputPath=)` naming is clear; `runScenarios(scenarios=)` accepts the authored (non-canonical) names or errors helpfully. *(Authoring ergonomics.)*

8. **Report.** Write `FRICTION-LOG.md` in the working dir: a ranked summary, then one entry per finding with severity (bug / limitation / friction / note), what happened, the exact message, and a `file:line` root cause where findable. Then a regression table for step 7 (fixed / still-broken). State the tested commit sha at the top. Keep it confidential-safe.

Finish with a short chat summary: tested sha, headline (does each path validate and run?), which of the known issues are now fixed, and any new findings.
