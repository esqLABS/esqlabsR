# Add user defined variability on parameters to a population.

Add user defined variability on parameters to a population.

## Usage

``` r
extendPopulationByUserDefinedParams(
  population,
  parameterPaths,
  meanValues,
  sdValues,
  distributions = Distributions$Normal
)
```

## Arguments

- population:

  Object of type `Population`

- parameterPaths:

  Vector of parameter path for which the variability is to be added.

- meanValues:

  Vector of mean values of the parameters. Must have the same length as
  `parameterPaths`. The type of mean (arithmetic, geometric) depends on
  the selected `distribution`. The values must be in the base units of
  the parameters.

- sdValues:

  Vector of standard deviation values of the parameters. Must have the
  same length as `parameterPaths`. The type of standard deviation
  depends on the selected `distribution`.

- distributions:

  Type of distribution from which the random values will be sampled.
  Must have the same length as `parameterPaths`. A list of supported
  distributions is defined in `Distributions`. Default is `"Normal"`.
