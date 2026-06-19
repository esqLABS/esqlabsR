# Open questions for the esqlabsR team

Numbered so you can reply by number. Each notes where it bites and a suggested default
(used provisionally in the schemas where one was needed).

1. **Excel-era `filePaths` entries.** With JSON as the v6 source of truth, are
   `modelParamsFile`, `individualsFile`, `populationsFile`, `scenariosFile`,
   `applicationsFile`, `plotsFile` still meaningful, or vestigial? They still name `.xlsx`
   files. *Bite:* ESQdb would have to invent values; Ralf would hallucinate them.
   *Default used:* kept and required (faithful), kept with a deprecation `$comment` (clean).

2. **Collection convention (maps vs arrays-with-id).** v6 mixes both (DIFF §12.1). Should
   the contract standardise — and on which? *Bite:* every consumer; biggest single
   structural decision. *Default used:* faithful keeps the mix; clean standardises on
   arrays-with-`id` (rationale in `schema-clean/RATIONALE.md`). Needs Felix's call.

3. **Which enums are actually closed?** Confirmed closed in code: `observedData.type`
   {excel,pkml,script,programmatic}, axis scales {lin,log}, PI `scaling` {lin,log}. **Not**
   closed in code but conventionally limited: `plotType`, `aggregation`, PI `algorithm`,
   PI `ciMethod`, `gender`, `species`, `population` (OSP db name), all `*Unit` fields.
   Please give the authoritative allowed-value lists. *Bite:* validation strictness and
   Ralf's output space. *Default used:* free strings with documented examples (faithful);
   `gender`∈{MALE,FEMALE} and `scaling`/scales tightened (clean).

4. **`additionalProperties`.** Tracked separately in `ISSUE_additionalProperties.md`.
   *Default used:* provisional `false`, flagged via `$comment`.

5. **String-encoded fields.** `simulationTime`, `xValuesLimits`/`yValuesLimits`, `plotIDs`,
   `proteinOntogenies` are strings (DIFF §12.2). Keep as strings, or promote to structured
   arrays/objects in v6? *Bite:* Ralf must reproduce delimiter grammar exactly if strings
   stay. *Default used:* strings (faithful) vs structured (clean).

6. **`simulationTime` grammar.** Is it definitively `start, end, resolution` per interval,
   `;`-separated, with `resolution` meaning points-per-unit? Confirm units of `resolution`.
   The faithful regex encodes triples-only; confirm 1- or 2-number forms are never valid.

7. **Referential integrity is out of scope for JSON Schema.** The schema can enforce that
   `scenario.individualId` is a string and that `individuals` is well-formed, but not that
   the id actually exists in `individuals`. Same for every reference in DIFF. Where should
   that live — a companion validator in esqlabsR (`validateProject()` already warns), in
   ESQapp, or a shared JS/Python validator generated from the schema? Recommend one shared
   layer so all three consumers agree.

8. **Identity vs display name.** `scenario.name` doubles as identity and label. If a user
   renames a scenario in ESQapp, every reference breaks. Should there be a separate stable
   `id` distinct from a mutable `name`? (Clean set uses `id` as the stable key.)

9. **Observed dataset labels as references.** Plots/PI reference observed data by a long
   derived label (e.g. `"Laskin 1982.Group A_Aciclovir_1_Human_MALE_..."`). That label is
   produced by the importer, not present as a declared id in the snapshot. *Bite:* Ralf and
   ESQdb cannot generate a valid reference without reproducing importer logic. Should the
   snapshot declare observed dataset ids explicitly?

10. **`type: "programmatic"` observed data.** Listed as valid in one place in
    `observed-data.R` but omitted in others; no required-field rule. Is it a real snapshot
    case, and if so what fields does it carry?

11. **`schemaVersion` semantics.** Confirm `"2.0"` is the intended v6 value and that the
    intended bump policy is: additive/back-compatible content change → minor (2.1);
    breaking → major (3.0). This drives the migration story.

12. **`quantiles` / `nsd` / `foldDistance` types.** In the example these are null. Are
    `quantiles` a list (e.g. `"0.05, 0.5, 0.95"`), `nsd` an integer count, `foldDistance` a
    number? Confirm so the schema can type them rather than leaving them nullable-any.
