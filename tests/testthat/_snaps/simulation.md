# `initializeSimulation()` stops on additionalParams values of another length

    Code
      initializeSimulation(simulation, additionalParams = list(paths = c(
        "Organism|Liver|Volume", "Organism|Volume"), values = c(2, 0.5, 9), units = "l"))
    Condition
      Error in `initializeSimulation()`:
      ! `additionalParams$values` must have length 1 or 2, one per path.
      x It has length 3.

# `initializeSimulation()` names every path the model lacks and applies none

    Code
      initializeSimulation(simulation, additionalParams = list(paths = c(
        "Organism|Liver|Volume", "Organism|Liverr|Volume"), values = c(2, 2), units = c(
        "l", "l")), additionalInitialConditions = list(paths = "Organism|Liver|Metformn",
        values = 1, units = "µmol"))
    Condition
      Error in `initializeSimulation()`:
      ! The model of simulation "Simple" has no parameter or molecule at these paths: "Organism|Liverr|Volume" and "Organism|Liver|Metformn".
      i Pass `stopIfParameterNotFound = FALSE` to skip them.

# `initializeSimulation()` names a single missing path in the singular

    Code
      initializeSimulation(simulation, additionalParams = list(paths = "Organism|Liverr|Volume",
        values = 2, units = "l"))
    Condition
      Error in `initializeSimulation()`:
      ! The model of simulation "Simple" has no parameter or molecule at this path: "Organism|Liverr|Volume".
      i Pass `stopIfParameterNotFound = FALSE` to skip it.

