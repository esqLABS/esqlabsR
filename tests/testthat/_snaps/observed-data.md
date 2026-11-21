# loadObservedData errors when an entry has invalid type

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x Invalid type "invalid_type" in `observedData` entry.
      i Must be one of: "excel", "pkml", "script", and "programmatic".

# loadObservedData errors when excel entry is missing required fields

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      ! `observedData` entry 1 (type "excel") is missing required fields importerConfiguration and sheets.

# loadObservedData errors when dataFolder is not declared

    Code
      loadObservedData(project)
    Condition
      Error in `.resolveDataPath()`:
      ! dataFolder is not declared in `filePaths`; cannot resolve 'x.pkml'.

# addObservedData rejects an under-specified config entry, naming every gap at once

    Code
      addObservedData(project, list(type = "excel", file = "x.xlsx"))
    Condition
      Error in `addObservedData()`:
      ! `observedData` entry 2 (type "excel") is missing required fields importerConfiguration and sheets.

---

    Code
      addObservedData(project, list(type = "excel"))
    Condition
      Error in `addObservedData()`:
      ! `observedData` entry 2 (type "excel") is missing required fields file, importerConfiguration, and sheets.

# addObservedData rejects a duplicate config entry file

    Code
      addObservedData(project, list(type = "excel", file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml", sheets = list(
          "Laskin 1982.Group A")))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with id "Aciclovir_TimeValuesData.xlsx" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedData aborts on a duplicate DataSet name, replaces with overwrite

    Code
      addObservedData(project, ds2)
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "prog_ds" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedData with a DataSet runs no script source

    Code
      addObservedData(project, .plasmaDataSet("Scratch measurement"))
    Message
      i Added programmatic observed-data source "Scratch measurement". It lives in this session until you save.
      i On `saveProject()` it is written to 'Scratch measurement.pkml' under the data folder, so it survives a reload.

# addObservedData with a DataSet asks a project without a data folder to declare one

    Code
      addObservedData(project, .plasmaDataSet("Scratch measurement"))
    Message
      i Added programmatic observed-data source "Scratch measurement". It lives in this session until you save.
      i Declare a dataFolder in `filePaths` before saving: `saveProject()` writes it to a PKML file there so it survives a reload, and aborts if no data folder is declared.

# addObservedData aborts on a DataSet name an already loaded source holds

    Code
      addObservedData(project, .plasmaDataSet("Digitized plasma"))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "Digitized plasma" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedData aborts on the name of a DataSet added in this session

    Code
      addObservedData(project, .plasmaDataSet("Scratch measurement"))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "Scratch measurement" already exists.
      i Pass `overwrite = TRUE` to replace it.

# a DataSet clashes with a declared id without reading that source's file

    Code
      addObservedData(project, .plasmaDataSet("cohort_b"))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "cohort_b" already exists.
      i Pass `overwrite = TRUE` to replace it.

# a DataSet name equal to a data set inside a script source aborts the next load

    Code
      suppressWarnings(suppressMessages(loadObservedData(project)))
    Condition
      Error in `.mergeObservedDataSets()`:
      x Duplicate observed-data set name across sources: "Digitized plasma".
      i Each loaded <DataSet> must have a unique name; rename the source or the data set.

# observedData declarations sharing a basename fail saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.serializeObservedDataSet()`:
      ! Two observedData declarations map to the same definition file 'obs.pkml.json'.
      x The on-disk id is the declaration's id, or the file basename (the programmatic name) when it declares none, so two declarations sharing one collide.
      i Give them distinct ids, or rename one so the basenames differ.

# saveProject aborts on a programmatic-to-PKML basename collision

    Code
      saveProject(project)
    Condition
      Error in `.persistProgrammaticObservedData()`:
      x Saving a programmatic observed-data source would overwrite another source: 'Collide.pkml'.
      i A programmatic source is written to '<name>.pkml'; this clashes with an existing source filed under the same name.
      i Rename the <DataSet> (its name) so the file names differ.

# saveProject aborts persisting a programmatic DataSet with no dataFolder

    Code
      saveProject(project)
    Condition
      Error in `.persistProgrammaticObservedData()`:
      x Cannot save the programmatic observed-data source "NoFolderSet": dataFolder is not declared in `filePaths`.
      i A programmatic source is written to a PKML file under dataFolder on save. Declare dataFolder, then save again.

# addObservedData rejects a duplicate declared id

    Code
      addObservedData(project, list(id = "obs", type = "pkml", file = "b.pkml"))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with id "obs" already exists.
      i Pass `overwrite = TRUE` to replace it.

# a config entry cannot overwrite a live programmatic source

    Code
      addObservedData(project, list(id = "prog_src", type = "pkml", file = "x.pkml"),
      overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      ! observedData entry with id "prog_src" is a programmatic source holding a <DataSet> in this session, so it cannot be overwritten with a configuration entry.
      i Remove it first with `removeObservedData()`, then add the configuration entry.

# removeObservedData mutates memory only; a surviving collision aborts saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.serializeObservedDataSet()`:
      ! Two observedData declarations map to the same definition file 'obs.pkml.json'.
      x The on-disk id is the declaration's id, or the file basename (the programmatic name) when it declares none, so two declarations sharing one collide.
      i Give them distinct ids, or rename one so the basenames differ.

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

# printing the observedData section lists the declaration ids

    Code
      print(project$definitions$observedData)
    Output
      <DefinitionList>
      observedData (2 definitions):
        * Aciclovir_TimeValuesData.xlsx
        * simulated_plasma

