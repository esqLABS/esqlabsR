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

# removeObservedData warns about the DataCombined and PI task using a removed Excel source

    Code
      removeObservedData(project, "Aciclovir_TimeValuesData.xlsx")
    Condition
      Warning:
      DataSet "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_" of removed observedData "Aciclovir_TimeValuesData.xlsx" is still referenced by 1 dataCombined entry:
      * aciclovir_individual
      i Update or remove this dataCombined entry.
      Warning:
      DataSet "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_" of removed observedData "Aciclovir_TimeValuesData.xlsx" is still referenced by 1 parameter identification task:
      * aciclovirsimple
      i Update the output mappings of this task.

# removeObservedData names only the DataSets of a removed source that are still used

    Code
      removeObservedData(project, "Aciclovir_TimeValuesData.xlsx")
    Condition
      Warning:
      DataSet "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_" of removed observedData "Aciclovir_TimeValuesData.xlsx" is still referenced by 1 parameter identification task:
      * aciclovirsimple
      i Update the output mappings of this task.

# removeObservedData warns about a PI task fitting an unsaved programmatic DataSet

    Code
      removeObservedData(project, "Aciclovir plasma")
    Condition
      Warning:
      DataSet "Aciclovir plasma" of removed observedData "Aciclovir plasma" is still referenced by 1 parameter identification task:
      * aciclovirsimple
      i Update the output mappings of this task.

# removeObservedData reports each removed source of a batch in the order given

    Code
      removeObservedData(project, ids)
    Condition
      Warning:
      DataSet "Aciclovir plasma" of removed observedData "Aciclovir plasma.pkml" is still referenced by 1 dataCombined entry:
      * aciclovir_plasma
      i Update or remove this dataCombined entry.
      Warning:
      DataSet "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_" of removed observedData "Aciclovir_TimeValuesData.xlsx" is still referenced by 1 parameter identification task:
      * aciclovirsimple
      i Update the output mappings of this task.

# the removeObservedData warnings agree in number with the DataSets they name

    Code
      cli::cli_warn(messages$observedDataRemovedUsedByDataCombined("cohorts.xlsx", c(
        "Cohort A", "Cohort B"), c("dc_a", "dc_b")))
    Condition
      Warning:
      DataSets "Cohort A" and "Cohort B" of removed observedData "cohorts.xlsx" are still referenced by 2 dataCombined entries:
      * dc_a and dc_b
      i Update or remove these dataCombined entries.
    Code
      cli::cli_warn(messages$observedDataRemovedUsedByPITasks("cohorts.xlsx", c(
        "Cohort A", "Cohort B"), c("fit_a", "fit_b")))
    Condition
      Warning:
      DataSets "Cohort A" and "Cohort B" of removed observedData "cohorts.xlsx" are still referenced by 2 parameter identification tasks:
      * fit_a and fit_b
      i Update the output mappings of these tasks.

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

