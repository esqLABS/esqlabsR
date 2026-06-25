# `readInitialConditionsFromXLS()` warns and overwrites a path repeated across sheets

    Code
      initialValues <- readInitialConditionsFromXLS(filePath = initialConditionsXLSpath,
        sheets = c("ValidSheet", "SecondSheet"))
    Condition
      Warning in `readInitialConditionsFromXLS()`:
      ! Duplicate molecule path(s) in initial values file 'data//InitialConditions.xlsx': "Organism|Liver|A". Only the last value defined for each path is used.

# `readInitialConditionsFromXLS()` errors when units are missing for a present molecule

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "MissingUnits")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Missing units in initial values file 'data//InitialConditions.xlsx' for molecule(s): "Organism|Liver|A". Units must be specified for all molecule initial values.

# `readInitialConditionsFromXLS()` errors when a value is missing for a present molecule

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "MissingValue")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Missing or non-numeric values in initial values file 'data//InitialConditions.xlsx' for molecule(s): "Organism|Liver|A". A numeric value must be specified for all present molecules.

# `readInitialConditionsFromXLS()` errors on a non-logical 'Is Present' value

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "BadIsPresent")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Invalid 'Is Present' values in initial values file 'data//InitialConditions.xlsx' for molecule(s): "Organism|Liver|A". 'Is Present' must be a logical value (TRUE/FALSE), numeric 1/0 (present/not present), or empty.

# `readInitialConditionsFromXLS()` warns and keeps the last value for a duplicate path

    Code
      initialValues <- readInitialConditionsFromXLS(filePath = initialConditionsXLSpath,
        sheets = "DuplicatePath")
    Condition
      Warning in `readInitialConditionsFromXLS()`:
      ! Duplicate molecule path(s) in initial values file 'data//InitialConditions.xlsx': "Organism|Liver|A". Only the last value defined for each path is used.

# `readInitialConditionsFromXLS()` errors when a present row has a blank container path

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "BlankPath")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Missing Container Path or Molecule Name in initial values file 'data//InitialConditions.xlsx', sheet "BlankPath", data row(s): "1".

# `readInitialConditionsFromXLS()` errors when a sheet has the wrong structure

    Code
      readInitialConditionsFromXLS(filePath = initialConditionsXLSpath, sheets = "InvalidSheet")
    Condition
      Error in `readInitialConditionsFromXLS()`:
      x Loading from XLS failed, the file 'data//InitialConditions.xlsx' has wrong structure!  The file should contain columns "Container Path, Molecule Name, Is Present, Value, Units, Scale Divisor, Neg. Values Allowed".

