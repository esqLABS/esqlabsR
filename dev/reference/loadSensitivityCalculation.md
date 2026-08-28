# Load Sensitivity Calculation Results

Restores a previously saved sensitivity calculation from a directory
created with
[`saveSensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveSensitivityCalculation.md).
If no simulation object is provided, the function loads the
`simulation.pkml` bundled in the directory, falling back to the
simulation file path stored in the metadata for folders saved before the
pkml was bundled.

## Usage

``` r
loadSensitivityCalculation(outputDir, simulation = NULL)
```

## Arguments

- outputDir:

  Path to the directory containing the saved sensitivity calculation
  files.

- simulation:

  Optional. A `Simulation` object. If not provided, the function loads
  the `simulation.pkml` bundled in `outputDir`, or, if absent, the
  simulation stored at the source path recorded in the metadata.

## Value

A named list of class `SensitivityCalculation`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sensitivity analysis result from disk
sensitivityCalculation <- loadSensitivityCalculation("output/my-sensitivity")
} # }
```
