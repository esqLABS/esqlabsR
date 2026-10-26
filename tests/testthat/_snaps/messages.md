# cliBullets() lets a value with braces through a cli_abort() as written

    Code
      cli::cli_abort(entry("Fit {old}"))
    Condition
      Error:
      x Scenario "Fit {old}" failed.
      i Value Fit {old}, literal `{x}`.

# a bullet message puts each bullet on its own line

    Code
      cli::cli_abort(messages$invalidSimulationRunOptions(4))
    Condition
      Error:
      ! `simulationRunOptions` must be an <ospsuite::SimulationRunOptions>, not a number.
      i Build one with `ospsuite::SimulationRunOptions$new(numberOfCores = 4)`.
      i To change how the solver behaves, pass `solverSettings` instead, as `list(relTol = 1e-6)`.

---

    Code
      cli::cli_warn(messages$scenariosSkipped(c("first", "second")))
    Condition
      Warning:
      ! 2 of the scenarios produced no results: "first" and "second".
      i Their entries carry `results = NULL`; select the ones that ran with `Filter(\(x) !is.null(x$results), results)`.

