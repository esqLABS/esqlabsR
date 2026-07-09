# removeApplication warns when still referenced by a scenario, removes anyway

    Code
      removeApplication(project, "aciclovir_iv_250mg")
    Condition
      Warning:
      Removed application "aciclovir_iv_250mg" is still referenced by 4 scenarios:
      * populationscenario, populationscenariofromcsv, testscenario, and testscenario_steadystate
      i These scenarios now have a dangling reference. Update or remove them.

# setApplicationParameterSets aborts on an undefined parameter set

    Code
      setApplicationParameterSets(project, "aciclovir_iv_250mg", "Ghost")
    Condition
      Warning:
      Canonicalized 1 referenced id to a safe form:
      * "Ghost" -> "ghost"
      Error in `setApplicationParameterSets()`:
      ! `parameterSets` references undefined parameter sets:
      x "ghost"

# addApplication aborts on a duplicate id in the batch

    Code
      addApplication(project, c("p1", "p1"))
    Condition
      Error in `addApplication()`:
      ! duplicate application id in the batch: "p1"

# print.Application renders its parameter-set references

    Code
      print(project$applications[["aciclovir_iv_250mg"]])
    Output
      <Application>
        * Parameter Sets: aciclovir_iv_250mg_default

# print.Application renders an empty protocol

    Code
      print(project$applications[["empty"]])
    Output
      <Application>
        * Parameter Sets: <empty string>

