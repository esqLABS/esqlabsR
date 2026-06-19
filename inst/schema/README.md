# esqlabsR v6 project-snapshot JSON Schema (draft)

Single shared contract for the v6 `Project.json` snapshot, consumed by **ESQapp** (grid +
forms + validation), **Ralf** (LLM generation target), and **ESQdb** (programmatic
generation). Draft 2020-12.

Grounded in branch `excel-deprecation-and-reorg` (the current v6 WIP merge chain;
supersedes the stale `v6` branch by 43 commits). v5 baseline = the spreadsheet-mirror JSON.

## Contents

1. `DIFF_v5_to_v6.md` — per-object-type structure diff, v5 → v6.
2. `schema-faithful/` — schema describing the snapshot **exactly as v6 emits it today**.
   Validated against the real `Example` and `Blank` snapshots. Intended as **PR A**.
3. `schema-clean/` — proposed **normalised** contract (uniform arrays-with-id, structured
   `simulationTime`, real arrays/numbers, tighter enums) + `RATIONALE.md` for Felix Mil.
   Intended as **PR B**.
4. `ISSUE_additionalProperties.md` — paste-ready issue for the unresolved
   `additionalProperties` policy.
5. `OPEN_QUESTIONS.md` — ambiguities to resolve with the esqlabsR team.

(PRs/issue are provided as files, not opened — no push credentials in this environment.)

## Schema files (one per object type, both sets)

`project.schema.json` (top-level) plus `scenario`, `individual`, `population`,
`application`, `modelParameterSets`, `individualParameterSets`, `applicationParameterSets`,
`parameterModification`, `outputPaths`, `observedData`, `plots`,
`parameterIdentification`, `filePaths`. Cross-file 1:N relationships use relative `$ref`;
keep the files co-located when resolving.

## Versioning

- `x-snapshotSchemaVersion` / the snapshot's `schemaVersion` = **`2.0`** (v5 = `1.0`).
- `x-schemaPackageVersion` = **`0.1.0`** (semver of this schema artifact itself).
- Policy intent: additive content change → minor bump (2.1); breaking → major (3.0). Future
  content changes become **migrations**, not rewrites. Confirm policy via OPEN_QUESTIONS §11.

## Status / caveats

1. `additionalProperties` is **provisional `false`** pending the issue above.
2. Several enums are documented but not closed in v6 code (OPEN_QUESTIONS §3).
3. JSON Schema cannot enforce **referential integrity** (a referenced id existing); that
   needs a companion validation layer (OPEN_QUESTIONS §7).

## Validate locally

```bash
pip install jsonschema referencing
# see the validation snippet used to confirm Example/Blank against schema-faithful/
```
