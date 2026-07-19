# loadObservedData errors when an entry has invalid type

    Code
      loadObservedData(project)
    Condition
      Error in `.validateObservedDataEntry()`:
      ! x Invalid type "invalid_type" in `observedData` entry. i Must be one of: "excel", "pkml", "script", and "programmatic".

# loadObservedData errors when excel entry is missing required fields

    Code
      loadObservedData(project)
    Condition
      Error in `.validateObservedDataEntry()`:
      ! x `observedData` entry 1 (type "excel") is missing required field importerConfiguration.

# loadObservedData errors when dataFolder is not declared

    Code
      loadObservedData(project)
    Condition
      Error in `.resolveDataPath()`:
      ! dataFolder is not declared in `filePaths`; cannot resolve 'x.pkml'.

# addObservedData rejects an under-specified config entry

    Code
      addObservedData(project, list(type = "excel", file = "x.xlsx"))
    Condition
      Error in `.validateObservedDataEntry()`:
      ! x `observedData` entry 2 (type "excel") is missing required field importerConfiguration.

# addObservedData rejects a duplicate config entry file

    Code
      addObservedData(project, list(type = "excel", file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml", sheets = list(
          "Laskin 1982.Group A")))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with file "Aciclovir_TimeValuesData.xlsx" already exists

# observedData declarations sharing a basename fail the write-through

    Code
      project$.setSection("observedData", colliding)
    Condition
      Error in `.serializeObservedDataSet()`:
      ! Two observedData declarations map to the same definition file 'obs.pkml.json'.
      x The on-disk id is the file basename (or the programmatic name), so two sources sharing a basename collide.
      i Rename one source so the basenames differ.

# addObservedData leaves the runtime store untouched when the write-through aborts

    Code
      addObservedData(project, ds)
    Condition
      Error in `.serializeObservedDataSet()`:
      ! Two observedData declarations map to the same definition file 'Aciclovir_TimeValuesData.xlsx.json'.
      x The on-disk id is the file basename (or the programmatic name), so two sources sharing a basename collide.
      i Rename one source so the basenames differ.

# removeObservedData leaves the runtime store untouched when the write-through aborts

    Code
      removeObservedData(project, "myProgSet")
    Condition
      Error in `.serializeObservedDataSet()`:
      ! Two observedData declarations map to the same definition file 'obs.pkml.json'.
      x The on-disk id is the file basename (or the programmatic name), so two sources sharing a basename collide.
      i Rename one source so the basenames differ.

# print.ObservedDataSource renders the source declaration

    Code
      print(project$observedData[[1]])
    Output
      <ObservedDataSource>
        * Type: excel
        * File: Aciclovir_TimeValuesData.xlsx
        * Name: <empty string>
        * Importer Configuration: esqlabs_dataImporter_configuration.xml
        * Sheets: Laskin 1982.Group A

