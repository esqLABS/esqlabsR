# Why adopt the "clean" v6 schema (PR B)

Audience: Felix Mil (decider). This argues for normalising the v6 snapshot contract rather
than freezing the current in-review shape. PR A (faithful) ships regardless; this PR is the
proposal to converge toward. Decision is yours — reject any individual point without
rejecting the rest.

## TL;DR

The faithful schema is correct but encodes three transitional warts as if they were the
contract: (a) a mix of map vs array collections, (b) string-encoded structured fields, and
(c) inconsistent id field names. All three are cheap to fix now and expensive to fix once
ESQdb and Ralf depend on them. None of these change the *information* in the snapshot, so
they are migrations, not redesigns.

## 1. Standardise collections on arrays-with-`id`

Today: `scenarios`/`individuals`/`populations` are arrays-with-id, but
`modelParameterSets`/`individualParameterSets`/`applicationParameterSets`/`applications`/
`outputPaths` are name-keyed maps. Internally esqlabsR keeps *everything* as named lists —
the array-vs-map split is only in the serializer, so neither form is "more native."

Why arrays-with-`id` for all:

1. **ESQdb is row-oriented.** Arrays map 1:1 to result-set rows. Maps force key-pivoting on
   write and read.
2. **Ralf generates arrays more reliably than dynamic object keys.** "Emit a list of
   objects each with an `id`" is a far more robust instruction than "emit an object whose
   keys are themselves data," which LLMs routinely get wrong (duplicate keys, key/value
   confusion).
3. **JSON Schema validates array items well, map values poorly.** With a map you can only
   constrain `additionalProperties` and `propertyNames`; you cannot require/describe the key
   as a first-class field. With an array you validate `id` like any other field.
4. **ESQapp grid wants stable order.** JSON object key order is not a contract; array order
   is. The grid currently has to impose its own ordering on the map collections.
5. **One mental model.** Three consumers, one iteration pattern, one reference pattern.

Cost: the serializer already converts named-list → array for the other three collections;
extending that to the remaining five is a small, mechanical change with a clean round-trip.

## 2. Promote string-encoded fields to structured values

Today these are Excel-cell holdovers:

1. `simulationTime` = `"0, 24, 60"` / `"0, 1, 0.1; 1, 24, 1"` → array of
   `{start, end, resolution}`.
2. `xValuesLimits` / `yValuesLimits` = `"0, 24"` → `[min, max]` numbers.
3. `plotIDs` = `"P1"` / `"P1, P2"` → `["P1", "P2"]`.
4. `proteinOntogenies` = `"CYP3A4:CYP3A4"` → `[{protein, ontogeny}]`.

Why: every consumer currently has to parse and re-emit a bespoke delimiter grammar.
That is exactly the kind of thing a generator (ESQdb) or an LLM (Ralf) gets subtly wrong —
a stray space, a `,` vs `;`, a missing third number — producing a file that *validates as a
string* but is semantically broken. Structured values move those errors from silent runtime
failures to schema-validation failures, which is the whole point of having a schema.

## 3. Unify the identity field name to `id`

Today: `scenario.name`, `individual.individualId`, `population.populationId`, PI `id`. Same
concept, four spellings. Unify to `id`. Cross-references then read uniformly
(`individualId` → `individuals[].id`) and tooling stops special-casing per type.

Optional but recommended: separate the stable `id` from a mutable display `name`/`label`
so a rename in ESQapp does not break every reference (OPEN_QUESTIONS §8).

## 4. Mark `filePaths` Excel entries as deprecated

With JSON as the source of truth, `*File` entries pointing at `.xlsx` are vestigial. The
clean schema keeps them (round-trip safety) but `$comment`s them as deprecation candidates
so ESQdb/Ralf know not to treat them as meaningful. Removal is a later migration.

## What this does NOT change

Parameter paths, values, units, demographics, dose, model file names, plot semantics — all
identical. This is purely the encoding of structure. The `schemaVersion` bump that would
carry these changes is the mechanism the project already has for exactly this.

## Suggested rollout

1. Land PR A (faithful, `schemaVersion 2.0`) as the immediate contract for current data.
2. Review this PR (clean) as the proposed `2.x`/`3.0` target.
3. If accepted, the serializer changes are mechanical and the faithful→clean transform is a
   pure function (good migration-test material).
