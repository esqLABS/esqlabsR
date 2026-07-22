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

# observedData declarations sharing a basename fail saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.serializeObservedDataSet()`:
      ! Two observedData declarations map to the same definition file 'obs.pkml.json'.
      x The on-disk id is the file basename (or the programmatic name), so two sources sharing a basename collide.
      i Rename one source so the basenames differ.

# saveProject aborts on a programmatic-to-PKML basename collision

    Code
      saveProject(project)
    Condition
      Error in `.persistProgrammaticObservedData()`:
      ! x Saving a programmatic observed-data source would overwrite another source: 'Collide.pkml'. i A programmatic source is written to '<name>.pkml'; this clashes with an existing source filed under the same name. i Rename the <DataSet> (its name) so the file names differ.

# saveProject aborts persisting a programmatic DataSet with no dataFolder

    Code
      saveProject(project)
    Condition
      Error in `.persistProgrammaticObservedData()`:
      ! x Cannot save the programmatic observed-data source "NoFolderSet": dataFolder is not declared in `filePaths`. i A programmatic source is written to a PKML file under dataFolder on save. Declare dataFolder, then save again.

# removeObservedData mutates memory only; a surviving collision aborts saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.serializeObservedDataSet()`:
      ! Two observedData declarations map to the same definition file 'obs.pkml.json'.
      x The on-disk id is the file basename (or the programmatic name), so two sources sharing a basename collide.
      i Rename one source so the basenames differ.

# print.ObservedDataSource renders the source declaration

    Code
      print(project$definitions$observedData[[1]])
    Output
      <ObservedDataSource>
        * Type: excel
        * File: Aciclovir_TimeValuesData.xlsx
        * Name: <empty string>
        * Importer Configuration: esqlabs_dataImporter_configuration.xml
        * Sheets: Laskin 1982.Group A

