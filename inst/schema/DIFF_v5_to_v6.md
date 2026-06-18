# esqlabsR snapshot: v5 → v6 structure diff

Baseline confirmed with you: the only v5 JSON is the **spreadsheet-mirror** snapshot
(`{ "column_names": [...], "rows": [ {col: val, ...} ] }` per Excel sheet, e.g.
`ProjectConfiguration.json` on `main`). v6 source examined: branch
`excel-deprecation-and-reorg` (the current WIP merge chain that supersedes the stale
`v6` branch by 43 commits), files `inst/extdata/projects/{Example,Blank}/Project.json`
and the `R/` readers/writers/validators.

## 0. The headline change (read this first)

This is **not** a "keys renamed / fields moved" diff. It is a change of representation
paradigm:

1. **v5** = one generic table-dump per Excel sheet. Every sheet becomes
   `{column_names, rows}`; every cell is a string; there is no domain meaning in the
   JSON itself. Relationships are implicit (a cell in one sheet happens to contain a
   name used in another).
2. **v6** = a single domain object graph in one `Project.json`. Top level is keyed by
   object type; fields are semantic and typed; relationships are explicit by id
   reference.
3. The **modelling information** carried is the same as v5 (same parameter paths, doses,
   demographics, etc.) — but the JSON *shape, keys, and types are essentially new*. So
   "contents stay the same, structure changes" is true only at the level of "the same
   facts are represented." The contract surface is new.

A second cross-cutting change: v6 mixes two collection conventions (see §11), and keeps
several Excel-era string encodings (see §12). Both are flagged as open questions.

## 1. Top-level / project configuration

1. **v5:** `ProjectConfiguration.json` = `{column_names:[Property,Value,Description],
   rows:[{Property, Value, Description}]}`. A two-column key/value table with a doc
   column.
2. **v6:** `filePaths` object with fixed, named keys (`modelFolder`,
   `configurationsFolder`, `modelParamsFile`, …). The `Description` column is dropped.
3. **New top-level metadata:** `schemaVersion` (now `"2.0"`; v5 is implicitly `"1.0"`)
   and `esqlabsRVersion`. These are the versioning hook — they already exist.
4. **Note:** the `*File` entries still point at `.xlsx` files even though JSON is now the
   source of truth — an Excel holdover (open question §1 in OPEN_QUESTIONS).

## 2. Scenarios

1. **v5:** rows of a `Scenarios` sheet; references to individual/population/application/
   output paths are bare strings in cells; simulation time is a string cell.
2. **v6:** `scenarios` is an **array of objects**, each with `name`, `individualId`,
   `populationId` (null when individual sim), `readPopulationFromCSV`,
   `modelParameterSets` (array of set names), `applicationProtocol`, `simulationTime`,
   `simulationTimeUnit`, `steadyState`, `steadyStateTime`, `steadyStateTimeUnit`,
   `overwriteFormulasInSS`, `modelFile`, `outputPathIds` (array).
3. **Relationships made explicit:** `individualId`→individuals, `populationId`→populations,
   `applicationProtocol`→applications, `modelParameterSets[]`→modelParameterSets,
   `outputPathIds[]`→outputPaths. Confirmed by `.warnIfReferenced()` in `R/validation.R`.
4. **Conditional rule:** if `steadyState=true`, `steadyStateTime`+`steadyStateTimeUnit`
   are required (the writer errors otherwise, for round-trip).

## 3. Individuals

1. **v5:** rows of an `Individuals` sheet (wide demographic columns); parameter overrides
   live in a separate area/sheet.
2. **v6:** `individuals` is an **array** of `{individualId, species, population, gender,
   weight, height, age, proteinOntogenies, parameterSets[]}`.
3. **Parameter overrides split out:** an individual no longer inlines its parameters; it
   holds `parameterSets` (array of names) that reference the **`individualParameterSets`
   map** (§7). 1:N.
4. **Required (from `R/individuals.R`):** `species`, `gender` (plus the id). Others
   optional/nullable.

## 4. Populations

1. **v5:** rows of a `Populations` sheet.
2. **v6:** `populations` is an **array** of `{populationId, species, population,
   numberOfIndividuals, proportionOfFemales, weight/height/age/BMI Min+Max+Unit,
   proteinOntogenies}`. Min/Max are nullable.

## 5. Parameter sets / parameter modifications

1. **v5:** parameter rows live in `ModelParameters` / individual / application sheets as
   `{containerPath, parameterName, value, units}`-ish columns, grouped by a "set" column.
2. **v6:** three **maps keyed by set name**: `modelParameterSets`, `individualParameterSets`,
   `applicationParameterSets`. Each value is an **array of parameter modifications**
   `{containerPath, parameterName, value, units}` (`units` nullable; `value` numeric).
3. **Relationship inverted vs v5:** the owning object (scenario/individual/application)
   now points *into* these named sets by name, rather than the rows pointing back. 1:N,
   and a set can be shared by multiple owners.

## 6. Applications

1. **v5:** rows of an `Applications` sheet.
2. **v6:** `applications` is a **map** of `applicationProtocolId → { parameterSets: [...] }`.
   The protocol's actual parameter values live in `applicationParameterSets` (§5),
   referenced by name.

## 7. Output paths / outputs

1. **v5:** an output-paths sheet (id ↔ model path).
2. **v6:** `outputPaths` is a **map** of `outputPathId → "pipe|delimited|model|path"`.
   Scenarios reference these ids via `outputPathIds[]`; plots reference the path string
   directly.

## 8. Plot (figure) definitions

1. **v5:** a `Plots` sheet (or several) of flat rows.
2. **v6:** `plots` is an **object** with three arrays:
   - `dataCombined[]`: `{name, simulated[], observed[]}`. A `simulated` curve references a
     `scenario` + `path`; an `observed` curve references a `dataSet` label. Both carry
     x/y offsets, offset units and scale factors (nullable).
   - `plotConfiguration[]`: `{plotID, DataCombinedName→dataCombined.name, plotType, title,
     subtitle, xUnit, yUnit, xAxisScale, yAxisScale, xValuesLimits, yValuesLimits,
     aggregation, quantiles, nsd, foldDistance}`.
   - `plotGrids[]`: `{name, plotIDs, title, subtitle}` where `plotIDs` references
     `plotConfiguration.plotID`.
3. **Closed enums (from `R/plots-utils.R`):** `xAxisScale`/`yAxisScale` ∈ {`lin`,`log`}.
   `plotType`/`aggregation` are **not** closed in code (pass-through to ospsuite/tlf) —
   open question §3.

## 9. Observed-data references

1. **v5:** a data sheet listing files + importer config.
2. **v6:** `observedData` is an **array** of `{type, file, importerConfiguration, sheets[]}`.
   `type` ∈ {`excel`,`pkml`,`script`,`programmatic`} (`R/observed-data.R`), and required
   sub-fields differ by type (excel needs `file`+`importerConfiguration`+`sheets`; pkml/
   script need `file`). Imported datasets get derived labels later referenced by plots and
   parameter identification.

## 10. Simulation time

1. **v5:** a string cell.
2. **v6:** still a **string** on `scenario.simulationTime`, but with a defined grammar
   (writer in `R/project-to-json.R`): one interval is `"start, end, resolution"`; multiple
   intervals are joined with `"; "`. E.g. `"0, 24, 60"` or `"0, 1, 0.1; 1, 24, 1"`.
   `simulationTimeUnit` is a sibling field. (Clean set models this as a real array — §12.)

## 11. Parameter identification (new as first-class)

1. **v6:** `parameterIdentification` is an **optional array** (absent in `Blank`) of
   `{id, scenarios[], parameters[], outputMappings[], configuration}`.
   - `parameters[]`: `{id, scenarios[], path, units, minValue, maxValue, startValue}`.
   - `outputMappings[]`: `{id, scenarios[], outputPathId→outputPaths, observedDataId→
     observed dataset, scaling∈{lin,log}, xOffset, yOffset, xFactor, yFactor}`.
   - `configuration`: `{algorithm, ciMethod, algorithmOptions?}` (algorithm/ciMethod are
     free strings in v6).

## 12. Two cross-cutting inconsistencies introduced in v6

1. **Collection convention is mixed.** Arrays-with-an-embedded-id for `scenarios`,
   `individuals`, `populations`, `observedData`, `parameterIdentification`; but
   name-keyed **maps** for `modelParameterSets`, `individualParameterSets`,
   `applicationParameterSets`, `applications`, `outputPaths`. Internally the R code keeps
   *everything* as named lists and the writer converts only some to arrays — so this is a
   serialization choice, not a semantic one. (Clean set unifies; see RATIONALE.)
2. **Several fields are still string-encoded** (Excel cell holdovers): `simulationTime`
   (`;`/`,` grammar), `xValuesLimits`/`yValuesLimits` (`"a, b"`), `plotIDs` (`","`),
   `proteinOntogenies` (`"Protein:Ontogeny[, ...]"`). The faithful schema models these as
   strings; the clean schema promotes them to arrays/objects.
