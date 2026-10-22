# .applyPlotConfiguration rejects a configuration that is not a named list

    Code
      .applyPlotConfiguration(defaultPlotConfiguration = c(titleSize = 10))
    Condition
      Error:
      ! `defaultPlotConfiguration` must be a named list, as returned by `createEsqlabsPlotConfiguration()`, not <numeric>.

