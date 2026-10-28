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

# Error type ASD under a geometric configuration aborts

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x The importer configuration 'importer.xml' reads the Error column as a geometric standard deviation, but a data set in 'data.xlsx' has the Error type "ASD":
      * "B_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_" (sheet "B")
      i That configuration has already stored every error value below 1 as missing and the others without a unit, so they cannot be read back as arithmetic standard deviations.
      i Use an importer configuration that reads arithmetic standard deviations, such as the one in the example project, and mark the geometric data sets with "GSD".

# different Error type values inside one data set abort

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x The rows of a data set in 'data.xlsx' have different values in the Error type column:
      * "A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_" (sheet "A"): "GSD" and "ASD"
      i Write the same value, "ASD" or "GSD", in every row of a data set, with the same letter case and no extra spaces, or leave the column empty in every row of it.

# an unknown Error type value aborts

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x Unknown Error type value in 'data.xlsx':
      * "Geometric Standard Deviation" for "A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_" (sheet "A")
      i Use "ASD" (arithmetic standard deviation) or "GSD" (geometric standard deviation), in any letter case but the same in every row of a data set, or leave the cell empty to use the error type of the importer configuration.

# a configuration that reads Error type aborts when only some sheets have the column

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x The importer configuration 'importer.xml' reads an Error type column, but sheet "B" of 'data.xlsx' has no such column.
      i Add an Error type column to this sheet (its cells may stay empty), or remove the Error type line from the importer configuration.

# an Error type header spelled differently aborts instead of being ignored

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x The importer configuration 'importer.xml' reads an Error type column, but sheet "A" of 'data.xlsx' has no such column.
      i Found "Error Type": the header must read exactly "Error type".
      i Add an Error type column to this sheet (its cells may stay empty), or remove the Error type line from the importer configuration.

# a GSD data set with more than one measurement unit aborts

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x A data set with the Error type "GSD" in 'data.xlsx' uses more than one unit in the Measurement unit column:
      * "A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_" (sheet "A")
      i A geometric standard deviation is a factor without a unit, and a change of unit inside a data set would rescale some of the factors.
      i Use one unit in every row of a "GSD" data set.

