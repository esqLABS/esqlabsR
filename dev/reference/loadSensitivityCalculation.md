# Load Sensitivity Calculation Results

Restores a previously saved sensitivity calculation from a directory
created with
[`saveSensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveSensitivityCalculation.md).
If no simulation object is provided, the function attempts to load it
from the saved simulation file path.

## Usage

``` r
loadSensitivityCalculation(outputDir, simulation = NULL)
```

## Arguments

- outputDir:

  Path to the directory containing the saved sensitivity calculation
  files.

- simulation:

  Optional. A `Simulation` object. If not provided, the function will
  attempt to load the simulation from the path stored in the metadata.

## Value

A named list of class `SensitivityCalculation`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sensitivity analysis result from disk
sensitivityCalculation <- loadSensitivityCalculation("output/my-sensitivity")
} # }
```
