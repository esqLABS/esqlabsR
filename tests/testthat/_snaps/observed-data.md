# loadObservedData errors when an entry has invalid type

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x Invalid type "invalid_type" in `observedData` entry.
      i Must be one of: "excel", "pkml", or "programmatic".

# loadObservedData errors when excel entry is missing required fields

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      ! `observedData` entry 1 (type "excel") is missing required fields importerConfiguration and sheets.

# addObservedData rejects a script entry

    Code
      addObservedData(project, list(type = "script", file = "make_dataset.R"))
    Condition
      Error in `addObservedData()`:
      x Invalid type "script" in `observedData` entry.
      i Must be one of: "excel" or "pkml".

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

# addObservedData with a DataSet reads no other source's file

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Scratch measurement"))
    Message
      i Added the <DataSet> "Scratch measurement" as an observed-data source. It lives in this session until you save.
      i Declare a dataFolder in `filePaths` before saving: `saveProject()` writes it to a PKML file there so it survives a reload, and aborts if no data folder is declared.

# addObservedData checks a DataSet name against the names already loaded

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Digitized plasma"))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "Digitized plasma" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedData aborts on the name of a DataSet added in this session

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Scratch measurement"))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "Scratch measurement" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedData refuses a DataSet saved earlier under its name

    Code
      addObservedData(project, scratch)
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "Scratch measurement" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedData(project, scratch, overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "Scratch measurement" is a file-based source, not a programmatic one, so it cannot be overwritten with a <DataSet>.
      i Remove it first with `removeObservedData()` (by its id, or its file basename), then add the <DataSet>.

# addObservedData refuses a DataSet whose PKML file another source reads

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Cohort B"))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "Cohort B" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Cohort B"), overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "Cohort B" is a file-based source, not a programmatic one, so it cannot be overwritten with a <DataSet>.
      i Remove it first with `removeObservedData()` (by its id, or its file basename), then add the <DataSet>.

# a DataSet name inside a PKML source not loaded yet aborts the next load

    Code
      suppressWarnings(suppressMessages(loadObservedData(project)))
    Condition
      Error in `loadObservedData()`:
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
      Error in `saveProject()`:
      x Saving a <DataSet> added in this session would give it the same id as another source: "Collide.pkml".
      i A <DataSet> is saved under the id '<name>.pkml'. Another source already has that id, either declared or taken from its file name.
      i Before saving, remove the <DataSet> with `removeObservedData()` by its name, rename it, and add it again with `addObservedData()`.

# saveProject keeps the data file of a source filed under a declared id

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Saving a <DataSet> added in this session would overwrite the data file of another source: 'Cohort B.pkml'.
      i The <DataSet> would be written to '<name>.pkml' in the data folder, a file another source already reads or another <DataSet> is written to. File names that differ only in case are the same file on Windows and macOS.
      i Before saving, remove the <DataSet> with `removeObservedData()` by its name, rename it, and add it again with `addObservedData()`.

# saveProject refuses two DataSets whose names differ only in case

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Saving a <DataSet> added in this session would overwrite the data file of another source: 'Cohort B.pkml' and 'cohort b.pkml'.
      i The <DataSet> would be written to '<name>.pkml' in the data folder, a file another source already reads or another <DataSet> is written to. File names that differ only in case are the same file on Windows and macOS.
      i Before saving, remove the <DataSet> with `removeObservedData()` by its name, rename it, and add it again with `addObservedData()`.

# saveProject aborts persisting a programmatic DataSet with no dataFolder

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Cannot save the <DataSet> "NoFolderSet" added in this session: dataFolder is not declared in `filePaths`.
      i A <DataSet> added with `addObservedData()` is written to a PKML file under dataFolder on save. Declare dataFolder, then save again.

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

# a DataSet clashes with an existing declaration's declared id

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "cohort_b"))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with name "cohort_b" already exists.
      i Pass `overwrite = TRUE` to replace it.

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

