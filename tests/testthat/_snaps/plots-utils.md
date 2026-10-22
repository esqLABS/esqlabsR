# .applyPlotConfiguration rejects a configuration that is not a named list

    Code
      .applyPlotConfiguration(defaultPlotConfiguration = c(titleSize = 10))
    Condition
      Error:
      ! `defaultPlotConfiguration` must be a named list, as returned by `createEsqlabsPlotConfiguration()`, not <numeric>.

# the sensitivity plots reject an unknown name in the plot configuration

    Code
      .applyPlotConfiguration(defaultPlotConfiguration = plotConfiguration)
    Condition
      Error:
      ! `defaultPlotConfiguration` has unknown name lineColour. See `vignette("reference-definition-fields")` for the plot configuration fields.

