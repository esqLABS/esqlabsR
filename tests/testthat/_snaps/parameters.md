# It overwrites the value if the path is present in multiple sheets

    Code
      params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)
    Condition
      Warning:
      Duplicate parameter path(s) in parameters file 'data/Parameters.xlsx': "Path1|Param1, Applications|Glucose_iv_infusion|Active". Only the last value defined for each path is used.

# `readParametersFromXLS()` errors on a non-numeric Value cell

    Code
      readParametersFromXLS(paramsXLSpath = paramsXLSpath)
    Condition
      Error in `readParametersFromXLS()`:
      ! Missing or non-numeric values in parameters file 'Parameters.xlsx' for parameter(s): "Path1|Param1". A numeric value must be specified for all parameters.

# `readParametersFromXLS()` warns and keeps the last value for a duplicate path

    Code
      params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath)
    Condition
      Warning:
      Duplicate parameter path(s) in parameters file 'Parameters.xlsx': "Path1|Param1". Only the last value defined for each path is used.

# addParameterSet canonicalizes its id

    Code
      addParameterSet(project, "New Set")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "New Set" -> "new set"

# addParameterSet aborts on a duplicate id

    Code
      addParameterSet(project, "global")
    Condition
      Error in `addParameterSet()`:
      ! parameter set "global" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addParameterSet aborts on a duplicate id in the batch

    Code
      addParameterSet(project, c("a", "a"))
    Condition
      Error in `addParameterSet()`:
      ! duplicate parameter set id in the batch: "a"

# addParameterEntry creates the set on demand and appends entries

    Code
      addParameterEntry(project, "tempset", "Organism|A", "K", 1.5, "1/h")
    Message
      Created parameter set "tempset" on demand to hold the new entry.

# addParameterEntry aborts on an in-batch duplicate by default

    Code
      suppressMessages(addParameterEntry(project, "dupset", containerPath = c(
        "Organism|A", "Organism|A"), parameterName = c("K", "K"), value = c(1, 9),
      units = c("1/h", "1/min")))
    Condition
      Error in `addParameterEntry()`:
      ! parameter "Organism|A|K" already exists in the set.
      i Pass `overwrite = TRUE` to replace it.

# addParameterEntry aborts on mismatched vector lengths

    Code
      addParameterEntry(project, "set", containerPath = c("Organism|A", "Organism|B"),
      parameterName = "K", value = c(1, 2), units = "1/h")
    Condition
      Error in `.assertParameterEntryVectorLengths()`:
      ! `containerPath`, `parameterName`, `value`, and `units` must be vectors of the same length.
      x Got lengths 2, 1, 2, and 1.

# removeParameterEntry rejects an empty or NA id

    Code
      removeParameterEntry(project, "", "Organism|A", "K")
    Condition
      Error in `removeParameterEntry()`:
      ! `id` must be a non-empty string

---

    Code
      removeParameterEntry(project, NA_character_, "Organism|A", "K")
    Condition
      Error in `removeParameterEntry()`:
      ! `id` must be a non-empty string

# removeInitialConditionEntry rejects an empty or NA id

    Code
      removeInitialConditionEntry(project, "", "Organism|A")
    Condition
      Error in `removeInitialConditionEntry()`:
      ! `id` must be a non-empty string

---

    Code
      removeInitialConditionEntry(project, NA_character_, "Organism|A")
    Condition
      Error in `removeInitialConditionEntry()`:
      ! `id` must be a non-empty string

# removeParameterSet warns when still referenced by a scenario, removes anyway

    Code
      removeParameterSet(project, "global")
    Condition
      Warning:
      Removed parameterSet "global" is still referenced by 4 definitions:
      * scenario 'populationscenario', scenario 'populationscenariofromcsv', scenario 'testscenario', and scenario 'testscenario_steadystate'
      i These now have a dangling reference. Update or remove them.

# removeParameterSet warns when still referenced by an individual, removes anyway

    Code
      removeParameterSet(project, "indiv1_default")
    Condition
      Warning:
      Removed parameterSet "indiv1_default" is still referenced by 1 definition:
      * individual 'indiv1'
      i These now have a dangling reference. Update or remove them.

# print.ParameterSet renders the entry count and a compact table

    Code
      print(project$definitions$parameterSets[["global"]])
    Output
      <ParameterSet>
        * Number of Entries: 1
        * Organism|Liver|EHC continuous fraction = 1

# print.ParameterSet renders an empty set

    Code
      print(project$definitions$parameterSets[["emptyset"]])
    Output
      <ParameterSet>
        * Number of Entries: 0

# addInitialConditions canonicalizes its id

    Code
      addInitialConditions(project, "New Set")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "New Set" -> "new set"

# addInitialConditions aborts on a duplicate id

    Code
      addInitialConditions(project, "dupset")
    Condition
      Error in `addInitialConditions()`:
      ! initial-condition set "dupset" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addInitialConditions aborts on a duplicate id in the batch

    Code
      addInitialConditions(project, c("a", "a"))
    Condition
      Error in `addInitialConditions()`:
      ! duplicate initial-condition set id in the batch: "a"

# addInitialConditionEntry creates the set on demand and appends

    Code
      addInitialConditionEntry(project, "tempset", "Organism|A", 1.5, "mg/l")
    Message
      Created initial-condition set "tempset" on demand to hold the new entry.

# addInitialConditionEntry aborts on an in-batch duplicate by default

    Code
      suppressMessages(addInitialConditionEntry(project, "dset", path = c(
        "Organism|A", "Organism|A"), value = c(1, 9), unit = c("mg/l", "mg/l")))
    Condition
      Error in `addInitialConditionEntry()`:
      ! initial condition "Organism|A" already exists in the set.
      i Pass `overwrite = TRUE` to replace it.

# addInitialConditionEntry aborts on mismatched vector lengths

    Code
      addInitialConditionEntry(project, "set", path = c("Organism|A", "Organism|B"),
      value = 1, unit = "mg/l")
    Condition
      Error in `.assertInitialConditionEntryVectorLengths()`:
      ! `path`, `value`, and `unit` must be vectors of the same length.
      x Got lengths 2, 1, and 1.

# addInitialConditionEntry aborts on a blank unit (units are mandatory)

    Code
      addInitialConditionEntry(project, "set", "Organism|A", 1.5, "")
    Message
      Created initial-condition set "set" on demand to hold the new entry.
    Condition
      Error in `addInitialConditionEntry()`:
      ! Invalid initial-condition entry:
      x unit must be a non-empty string

# removeInitialConditions warns when still referenced by a scenario, removes anyway

    Code
      removeInitialConditions(project, "refset")
    Condition
      Warning:
      Removed initialConditions "refset" is still referenced by 1 scenario:
      * scenario 'testscenario'
      i These now have a dangling reference. Update or remove them.

# print.InitialConditionSet renders the entry count and a compact table

    Code
      print(project$definitions$initialConditions[["printset"]])
    Output
      <InitialConditionSet>
        * Number of Entries: 2
        * Organism|A = 1.5 [mg/l]
        * Organism|B = 0.5 [µmol/l]

# print.InitialConditionSet renders a unit-less entry

    Code
      print(set)
    Output
      <InitialConditionSet>
        * Number of Entries: 1
        * Organism|A = 1.5

# print.InitialConditionSet renders an empty set

    Code
      print(project$definitions$initialConditions[["emptyset"]])
    Output
      <InitialConditionSet>
        * Number of Entries: 0

# `readInitialConditionsFromXLS()` warns and overwrites a path repeated across sheets

    Code
      initialValues <- readInitialConditionsFromXLS(filePath = initialConditionsXLSpath,
        sheets = c("ValidSheet", "SecondSheet"))
    Condition
      Warning in `readInitialConditionsFromXLS()`:
      ! Duplicate molecule path(s) in initial values file 'data/InitialConditions.xlsx': "Organism|Liver|A". Only the last value defined for each path is used.

# `readInitialConditionsFromXLS()` errors when units are missing for a present molecule

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "MissingUnits")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Missing units in initial values file 'data/InitialConditions.xlsx' for molecule(s): "Organism|Liver|A". Units must be specified for all molecule initial values.

# `readInitialConditionsFromXLS()` errors when a value is missing for a present molecule

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "MissingValue")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Missing or non-numeric values in initial values file 'data/InitialConditions.xlsx' for molecule(s): "Organism|Liver|A". A numeric value must be specified for all present molecules.

# `readInitialConditionsFromXLS()` errors on a non-logical 'Is Present' value

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "BadIsPresent")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Invalid 'Is Present' values in initial values file 'data/InitialConditions.xlsx' for molecule(s): "Organism|Liver|A". 'Is Present' must be a logical value (TRUE/FALSE), numeric 1/0 (present/not present), or empty.

# `readInitialConditionsFromXLS()` warns and keeps the last value for a duplicate path

    Code
      initialValues <- readInitialConditionsFromXLS(filePath = initialConditionsXLSpath,
        sheets = "DuplicatePath")
    Condition
      Warning in `readInitialConditionsFromXLS()`:
      ! Duplicate molecule path(s) in initial values file 'data/InitialConditions.xlsx': "Organism|Liver|A". Only the last value defined for each path is used.

# `readInitialConditionsFromXLS()` errors when a present row has a blank container path

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "BlankPath")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Missing Container Path or Molecule Name in initial values file 'data/InitialConditions.xlsx', sheet "BlankPath", data row(s): "1".

# `readInitialConditionsFromXLS()` errors when a sheet has the wrong structure

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "InvalidSheet")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Loading from XLS failed, the file 'data/InitialConditions.xlsx' has wrong structure!  The file should contain columns "Container Path, Molecule Name, Is Present, Value, Units, Scale Divisor, Neg. Values Allowed".

# setParameterValuesByPathWithCondition aborts on a values length mismatch

    Code
      setParameterValuesByPathWithCondition(parameterPaths = c(
        "Organism|Liver|Volume", "Organism|Volume"), values = c(1, 2, 3), simulation = NULL)
    Condition
      Error in `setParameterValuesByPathWithCondition()`:
      ! `values` must be a scalar or have the same length as `parameterPaths`.
      x Got lengths 3 and 2.

# setParameterValuesByPathWithCondition aborts on a units length mismatch

    Code
      setParameterValuesByPathWithCondition(parameterPaths = c(
        "Organism|Liver|Volume", "Organism|Volume"), values = c(1, 2), simulation = NULL,
      units = c("l", "l", "l"))
    Condition
      Error in `setParameterValuesByPathWithCondition()`:
      ! `units` must be `NULL`, a scalar, or have the same length as `parameterPaths`.
      x Got lengths 3 and 2.

# .splitParameterPathIntoContainerAndName aborts on a separator-less path

    Code
      esqlabsR:::.splitParameterPathIntoContainerAndName("Volume")
    Condition
      Error in `esqlabsR:::.splitParameterPathIntoContainerAndName()`:
      ! parameter path "Volume" must contain a container path and a parameter name separated by "|".

