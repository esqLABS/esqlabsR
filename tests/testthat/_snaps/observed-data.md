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
      ! observed-data source "aciclovir_timevaluesdata" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedData aborts on a duplicate DataSet name, replaces with overwrite

    Code
      addObservedData(project, ds2)
    Condition
      Error in `addObservedData()`:
      ! observed-data source "prog_ds" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedData with a DataSet reads no other source's file

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Scratch measurement"))
    Message
      i Added the <DataSet> "Scratch measurement" as observed-data source "scratch_measurement". It lives in this session until you save.
      i Declare a dataFolder in `filePaths` before saving: `saveProject()` writes it to a PKML file there so it survives a reload, and aborts if no data folder is declared.

# addObservedData checks a DataSet name against the names already loaded

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Digitized plasma"), id = "digitized_plasma_rerun")
    Condition
      Error in `addObservedData()`:
      x Another observed-data source already provides a data set named "Digitized plasma".
      i Each data set needs a unique name. Rename the <DataSet>, or remove the other source with `removeObservedData()`.

# addObservedData aborts on the name of a DataSet added in this session

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Scratch measurement"),
      id = "scratch_rerun")
    Condition
      Error in `addObservedData()`:
      x Another observed-data source already provides a data set named "Scratch measurement".
      i Each data set needs a unique name. Rename the <DataSet>, or remove the other source with `removeObservedData()`.

# a DataSet name known under another id aborts, also with overwrite

    Code
      addObservedData(project, rerun, id = "main")
    Condition
      Error in `addObservedData()`:
      x Another observed-data source already provides a data set named "Plasma cohort A".
      i Each data set needs a unique name. Rename the <DataSet>, or remove the other source with `removeObservedData()`.
    Code
      addObservedData(project, rerun, id = "main", overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      x Another observed-data source already provides a data set named "Plasma cohort A".
      i Each data set needs a unique name. Rename the <DataSet>, or remove the other source with `removeObservedData()`.

# addObservedData refuses a DataSet saved earlier under its id

    Code
      addObservedData(project, scratch)
    Condition
      Error in `addObservedData()`:
      ! observed-data source "scratch_measurement" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedData(project, scratch, overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      x The observed-data source "scratch_measurement" reads its data from a file, so a <DataSet> cannot replace it.
      i Remove it first with `removeObservedData()`, or add the <DataSet> under another `id`.

# addObservedData refuses a DataSet whose PKML file another source reads

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Cohort B"))
    Condition
      Error in `addObservedData()`:
      x The <DataSet> would be saved to 'cohort_b.pkml', the data file of another source.
      i Pass another `id`.
    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Cohort B"), overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      x The <DataSet> would be saved to 'cohort_b.pkml', the data file of another source.
      i Pass another `id`.

# a DataSet name inside a PKML source not loaded yet aborts the next load

    Code
      suppressWarnings(suppressMessages(loadObservedData(project)))
    Condition
      Error in `loadObservedData()`:
      x Duplicate observed-data set name across sources: "Digitized plasma".
      i Each loaded <DataSet> must have a unique name; rename the source or the data set.

# sources whose files share a name take one id, and the second add aborts

    Code
      addObservedData(project, list(type = "pkml", file = "dirB/obs.pkml"))
    Condition
      Error in `addObservedData()`:
      ! observed-data source "obs" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedData(project, list(type = "pkml", file = "data.pkml"))
    Condition
      Error in `addObservedData()`:
      ! observed-data source "data" already exists.
      i Pass `overwrite = TRUE` to replace it.

# saveProject aborts on a programmatic-to-PKML file collision

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Saving a <DataSet> added in this session would overwrite the data file of another source: 'collide.pkml'.
      i The <DataSet> would be written to '<id>.pkml' in the data folder, a file another source already reads.
      i Remove the <DataSet>'s source with `removeObservedData()` and add it again under another `id`.

# saveProject keeps the data file of a source filed under another id

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Saving a <DataSet> added in this session would overwrite the data file of another source: 'cohort_b.pkml'.
      i The <DataSet> would be written to '<id>.pkml' in the data folder, a file another source already reads.
      i Remove the <DataSet>'s source with `removeObservedData()` and add it again under another `id`.

# saveProject aborts persisting a programmatic DataSet with no dataFolder

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Cannot save the <DataSet> "NoFolderSet" added in this session: dataFolder is not declared in `filePaths`.
      i A <DataSet> added with `addObservedData()` is written to a PKML file under dataFolder on save. Declare dataFolder, then save again.

# saveProject refuses a DataSet that shares its id with another source before writing it

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Two observed-data sources would be saved to the same definition file 'cohort_b.json'.
      i Each source is saved to a file named after its id. Give the sources distinct ids, or remove one with `removeObservedData()`.

# addObservedData rejects a duplicate declared id

    Code
      addObservedData(project, list(id = "obs", type = "pkml", file = "b.pkml"))
    Condition
      Error in `addObservedData()`:
      ! observed-data source "obs" already exists.
      i Pass `overwrite = TRUE` to replace it.

# the id argument wins over the entry's id, which wins over the source

    Code
      addObservedData(project, list(id = "laskin_entry", type = "pkml", file = "Laskin IV.pkml"),
      id = "laskin_argument")
      addObservedData(project, list(id = "laskin_entry", type = "pkml", file = "Laskin IV.pkml"))
      addObservedData(project, list(type = "pkml", file = "sub/Cohort B.pkml"))
      addObservedData(project, ospsuite::DataSet$new(name = "Scratch measurement"))
    Message
      i Added the <DataSet> "Scratch measurement" as observed-data source "scratch_measurement". It lives in this session until you save.
      i On `saveProject()` it is written to 'scratch_measurement.pkml' under the data folder, so it survives a reload.
    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Pilot measurement"), id = "pilot")
    Message
      i Added the <DataSet> "Pilot measurement" as observed-data source "pilot". It lives in this session until you save.
      i On `saveProject()` it is written to 'pilot.pkml' under the data folder, so it survives a reload.

# an id you give warns once when it is canonicalized, a taken one never

    Code
      addObservedData(project, list(type = "pkml", file = "laskin_iv.pkml"), id = "Laskin IV")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Laskin IV" -> "laskin_iv"
    Code
      addObservedData(project, list(id = "Laskin PO", type = "pkml", file = "laskin_po.pkml"))
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Laskin PO" -> "laskin_po"

# addObservedData rejects an id that is not a single non-empty string

    Code
      addObservedData(project, list(id = 2024, type = "pkml", file = "a.pkml"))
    Condition
      Error in `addObservedData()`:
      ! The `id` of an observed-data source must be a single non-empty string.
    Code
      addObservedData(project, list(type = "pkml", file = 2024))
    Condition
      Error in `addObservedData()`:
      x Cannot take an id from this observed-data source.
      i Give it an `id`, or a file or <DataSet> name to take the id from.

# a config entry cannot overwrite a live programmatic source

    Code
      addObservedData(project, list(id = "prog_src", type = "pkml", file = "x.pkml"),
      overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      ! observedData entry with id "prog_src" is a programmatic source holding a <DataSet> in this session, so it cannot be overwritten with a configuration entry.
      i Remove it first with `removeObservedData()`, then add the configuration entry.

# a DataSet clashes with the id of another source

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Cohort B"))
    Condition
      Error in `addObservedData()`:
      ! observed-data source "cohort_b" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedData(project, ospsuite::DataSet$new(name = "cohort b"))
    Condition
      Error in `addObservedData()`:
      ! observed-data source "cohort_b" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedData(project, ospsuite::DataSet$new(name = "Cohort B"), overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      x The observed-data source "cohort_b" reads its data from a file, so a <DataSet> cannot replace it.
      i Remove it first with `removeObservedData()`, or add the <DataSet> under another `id`.

# removeObservedData finds a source by the DataSet name its id was taken from

    Code
      removeObservedData(project, "Scratch measurement")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Scratch measurement" -> "scratch_measurement"

# removing a saved DataSet source warns about a dataCombined that names it

    Code
      removeObservedData(reloaded, "pilot")
    Condition
      Warning:
      Removed observedData "pilot" is still referenced by 1 dataCombined entry:
      * dc1
      i These dataCombined entries now have a dangling reference. Update or remove them.

# only the source holding the named data set warns on removal

    Code
      removeObservedData(project, "lab")
    Condition
      Warning:
      Removed observedData "lab" is still referenced by 1 dataCombined entry:
      * cohort_b_plot
      i These dataCombined entries now have a dangling reference. Update or remove them.

# removing an Excel source warns once about every dataCombined naming its data sets

    Code
      removeObservedData(project, "aciclovir_timevaluesdata")
    Condition
      Warning:
      Removed observedData "aciclovir_timevaluesdata" is still referenced by 2 dataCombined entries:
      * laskin_iv and laskin_iv_log
      i These dataCombined entries now have a dangling reference. Update or remove them.

# a DataSet renamed after the add is matched by its current name

    Code
      removeObservedData(project, "pilot")
    Condition
      Warning:
      Removed observedData "pilot" is still referenced by 1 dataCombined entry:
      * dc1
      i These dataCombined entries now have a dangling reference. Update or remove them.

# removing two referenced sources in one call warns once per source

    Code
      removeObservedData(project, c("pilot", "main"))
    Condition
      Warning:
      Removed observedData "pilot" is still referenced by 1 dataCombined entry:
      * dc1
      i These dataCombined entries now have a dangling reference. Update or remove them.
      Warning:
      Removed observedData "main" is still referenced by 1 dataCombined entry:
      * dc2
      i These dataCombined entries now have a dangling reference. Update or remove them.

# removeObservedData mutates memory only; a surviving collision aborts saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.serializeObservedDataSet()`:
      x Two observed-data sources would be saved to the same definition file 'obs.json'.
      i Each source is saved to a file named after its id. Give the sources distinct ids, or remove one with `removeObservedData()`.

# every observed-data source stores its id first and the section is keyed by it

    Code
      project$definitions$observedData[["cohort_b"]]$file <- "other.pkml"
    Condition
      Error in `[[<-`:
      ! observedData is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# an observed-data record without an id in a project file aborts the load

    Code
      loadProject(tmp)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind observedData has no usable id.
      x id must be a single non-empty string (it names the definition and its file).

---

    Code
      loadProject(tmp)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind observedData has no usable id.
      x id must be a single non-empty string (it names the definition and its file).

# a DataSet replaces a programmatic source with no data only with overwrite

    Code
      addObservedData(project, scratch)
    Condition
      Error in `addObservedData()`:
      ! observed-data source "scratch_measurement" already exists.
      i Pass `overwrite = TRUE` to replace it.

# a DataSet whose name gives the id of another DataSet aborts

    Code
      addObservedData(project, ospsuite::DataSet$new(name = "cohort"))
    Condition
      Error in `addObservedData()`:
      ! observed-data source "cohort" already exists.
      i Pass `overwrite = TRUE` to replace it.

# a DataSet overwrite is refused when another source provides its name

    Code
      addObservedData(project, lower, overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      x Another observed-data source already provides a data set named "cohort b".
      i Each data set needs a unique name. Rename the <DataSet>, or remove the other source with `removeObservedData()`.

# a DataSet overwrite is refused when another source reads its PKML file

    Code
      addObservedData(project, lower, overwrite = TRUE)
    Condition
      Error in `addObservedData()`:
      x The <DataSet> would be saved to 'cohort_b.pkml', the data file of another source.
      i Pass another `id`.

# print.ObservedDataSource renders the source declaration

    Code
      print(project$definitions$observedData[[1]])
    Output
      <ObservedDataSource>
        * Id: aciclovir_timevaluesdata
        * Type: excel
        * File: Aciclovir_TimeValuesData.xlsx
        * Importer Configuration: esqlabs_dataImporter_configuration.xml
        * Sheets: Laskin 1982.Group A

# printing the observedData section lists the declaration ids

    Code
      print(project$definitions$observedData)
    Output
      <DefinitionList>
      observedData (2 definitions):
        * aciclovir_timevaluesdata
        * simulated_plasma

