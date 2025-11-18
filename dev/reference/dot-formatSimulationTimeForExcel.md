# Format simulation time for Excel

Format simulation time for Excel

## Usage

``` r
.formatSimulationTimeForExcel(simulationTime)
```

## Arguments

- simulationTime:

  List of numeric vectors. Simulation time intervals (as parsed by
  ScenarioConfiguration) or character string containing time intervals.

## Value

Formatted string for Excel storage, or empty string if input is NULL.

## Details

Converts simulation time intervals from a list format to a string format
suitable for Excel storage. Each interval is formatted as "start, end,
resolution" and multiple intervals are separated by semicolons.
