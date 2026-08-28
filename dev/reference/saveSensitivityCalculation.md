# Save Sensitivity Calculation Results

Saves the results of a sensitivity analysis to a specified directory,
including metadata and simulation output required for restoring or
sharing the analysis. The underlying simulation is written to
`simulation.pkml` in the same directory so the saved calculation is
self-contained and can be reloaded without access to the original
simulation file.

## Usage

``` r
saveSensitivityCalculation(
  sensitivityCalculation,
  outputDir,
  overwrite = FALSE
)
```

## Arguments

- sensitivityCalculation:

  A named list of class `SensitivityCalculation` as returned by
  [`sensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityCalculation.md),
  containing `simulationResults`, `outputPaths`, `parameterPaths`, and
  `pkData`.

- outputDir:

  A character string specifying the path to the directory where the
  results should be saved.

- overwrite:

  Logical. If `TRUE`, an existing directory at `outputDir` will be
  deleted and replaced. Default is `FALSE`.

## Value

Invisibly returns `NULL`. Results are saved to disk in the specified
folder.

## Examples

``` r
if (FALSE) { # \dontrun{
sensitivityCalculation <- sensitivityCalculation(
  simulation = mySim,
  outputPaths = "Organism|PeripheralVenousBlood|Drug|Plasma",
  parameterPaths = c("Drug|Lipophilicity", "Application|Dose")
)

saveSensitivityCalculation(
  sensitivityCalculation,
  outputDir = "output/my-sensitivity",
  overwrite = TRUE
)
} # }
```
