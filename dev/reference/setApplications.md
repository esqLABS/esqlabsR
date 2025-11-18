# Set an application protocol in a `Simulation` from the Excel file

Set an application protocol in a `Simulation` from the Excel file

## Usage

``` r
setApplications(simulation, scenarioConfiguration)
```

## Arguments

- simulation:

  A `Simulation` object that will be modified.

- scenarioConfiguration:

  A `ScenarioConfiguration` object holding the name of the application
  protocol.

## Details

Sets the parameter values describing the application protocol defined in
the scenario configuration by reading from the Applications.xlsx file.
The function looks for a sheet named after the application protocol and
applies all parameter values found in that sheet to the simulation.

## Deprecation

This function is deprecated. Use `setParametersFromXLS` instead for
better parameter handling and more flexibility.
