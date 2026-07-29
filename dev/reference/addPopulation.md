# Add one or more populations to a Project

Add populations to `populations` definitions. Two forms:

- A **demographics spec**: pass `species` and `numberOfIndividuals`
  (plus optional `...` fields). This vectorizes over a vector of ids
  (see the recycling rule under Details); `species`,
  `numberOfIndividuals`, and the `...` fields are scalar-per-definition
  (recycle/align).

- An **injected object**: pass a single id and an
  [ospsuite::Population](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Population.html)
  object in the `species` position. The object is stored in the R
  session and the scenario runs against it directly (a mutated or
  programmatically built population). `numberOfIndividuals` and `...`
  fields are not accepted for this form. It survives to disk only after
  [`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
  which freezes the sampled population to a `<id>.csv` file under the
  populations folder; rerun the code that built it to reproduce it in a
  new session.

## Usage

``` r
addPopulation(project, id, species, numberOfIndividuals = NULL, ...)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of unique ids (the number of populations to add).
  Each is canonicalized to a safe, lowercase id (a warning names the
  result if it changed). For an injected
  [ospsuite::Population](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Population.html)
  object the id must be a single value.

- species:

  Character scalar (recycled) or the same length as `id`; or an
  [ospsuite::Population](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Population.html)
  object to inject (single id only).

- numberOfIndividuals:

  Positive number, scalar (recycled) or the same length as `id`. Omit
  when injecting a `Population` object.

- ...:

  Optional named fields. Accepted: `proportionOfFemales`, `weightMin`,
  `weightMax`, `heightMin`, `heightMax`, `ageMin`, `ageMax`, `BMIMin`,
  `BMIMax`, `gender`, `weightUnit`, `heightUnit`, `ageUnit`, `BMIUnit`,
  `population`, `diseaseState`, `proteinOntogenies`, and `overwrite`.
  Numeric range fields are coerced via
  [`as.double()`](https://rdrr.io/r/base/double.html). `overwrite` is a
  logical scalar (default `FALSE`): an id that already exists aborts
  unless `overwrite = TRUE`, which replaces it (last-write-wins).

## Value

The `project` object, invisibly.

## Details

The id argument sets `N`, the number of definitions to act on, and
cannot itself be recycled: when any scalar-per-definition field has
length greater than 1, the id vector must have that same length. A
length-1 id with all-scalar fields is the ordinary single-definition
call.

Each scalar-per-definition field is either length 1 (recycled to all `N`
definitions) or length `N` (aligned to the ids by position). Any other
length is an error naming the field and the lengths.

A vector-valued-per-definition field (an individual's or application's
`parameterSets`, a scenario's `outputPaths` and `parameterSets`) is
applied whole to every definition, never split positionally. To give a
different multi-valued list per definition, pass a list of the same
length as the id vector (one vector per definition).

The call is all-or-nothing: every definition is validated first, and if
any fails the whole call aborts and writes nothing. On success all
definitions are folded into the section and persisted in a single
write-through.

Two families of authoring functions sit outside this id-sets-`N` rule.
[`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
and
[`removeParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterEntry.md)
vectorize over parameter entries (parallel `containerPath` /
`parameterName` / `value` / `units` vectors) within a single named set,
a different axis than the id-sets-`N` rule described here.
[`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md),
[`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md),
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
and the per-task parameter-identification sub-definition helpers act on
a single definition per call.

## See also

Other population:
[`removePopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePopulation.md),
[`setPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/setPopulation.md)
