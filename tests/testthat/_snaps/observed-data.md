# loadObservedData errors when an entry has invalid type

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      x Invalid type "invalid_type" in an observed-data source.
      i Must be one of: "excel", "pkml", "script", and "programmatic".

# loadObservedData errors when excel entry is missing required fields

    Code
      loadObservedData(project)
    Condition
      Error in `loadObservedData()`:
      ! Observed-data source 1 (type "excel") is missing required fields importerConfiguration and sheets.

# loadObservedData errors when dataFolder is not declared

    Code
      loadObservedData(project)
    Condition
      Error in `.resolveDataPath()`:
      ! dataFolder is not declared in `filePaths`; cannot resolve 'x.pkml'.

# addObservedDataSource rejects an under-specified config entry, naming every gap at once

    Code
      addObservedDataSource(project, list(type = "excel", file = "x.xlsx"))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source 2 (type "excel") is missing required fields importerConfiguration and sheets.

---

    Code
      addObservedDataSource(project, list(type = "excel"))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source 2 (type "excel") is missing required fields file, importerConfiguration, and sheets.

# addObservedDataSource rejects a duplicate config entry file

    Code
      addObservedDataSource(project, list(type = "excel", file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml", sheets = list(
          "Laskin 1982.Group A")))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with id "Aciclovir_TimeValuesData.xlsx" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedDataSource aborts on a duplicate DataSet name, replaces with overwrite

    Code
      addObservedDataSource(project, ds2)
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "prog_ds" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedDataSource with a DataSet runs no script source

    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "Scratch measurement"))
    Message
      i Added the <DataSet> "Scratch measurement" as an observed-data source. It lives in this session until you save.
      i On `saveProject()` it is written to 'Scratch measurement.pkml' under the data folder, so it survives a reload.

# addObservedDataSource with a DataSet reads no other source's file

    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "Scratch measurement"))
    Message
      i Added the <DataSet> "Scratch measurement" as an observed-data source. It lives in this session until you save.
      i Declare a dataFolder in `filePaths` before saving: `saveProject()` writes it to a PKML file there so it survives a reload, and aborts if no data folder is declared.

# addObservedDataSource checks a DataSet name against the names already loaded

    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "Digitized plasma"))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "Digitized plasma" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedDataSource aborts on the name of a DataSet added in this session

    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "Scratch measurement"))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "Scratch measurement" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedDataSource refuses a DataSet saved earlier under its name

    Code
      addObservedDataSource(project, scratch)
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "Scratch measurement" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedDataSource(project, scratch, overwrite = TRUE)
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "Scratch measurement" is a file-based source, not a programmatic one, so it cannot be overwritten with a <DataSet>.
      i Remove it first with `removeObservedDataSource()` by its id, then add the <DataSet>.

# addObservedDataSource refuses a DataSet whose PKML file another source reads

    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "Cohort B"))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "Cohort B" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "Cohort B"),
      overwrite = TRUE)
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "Cohort B" is a file-based source, not a programmatic one, so it cannot be overwritten with a <DataSet>.
      i Remove it first with `removeObservedDataSource()` by its id, then add the <DataSet>.

# a DataSet name inside a script source not loaded yet aborts the next load

    Code
      suppressWarnings(suppressMessages(loadObservedData(project)))
    Condition
      Error in `loadObservedData()`:
      x Duplicate observed-data set name across sources: "Digitized plasma".
      i Each loaded <DataSet> must have a unique name; rename the source or the data set.

# loadObservedData aborts on an unknown source id, listing the ids

    Code
      loadObservedData(project, sources = c("Aciclovir_TimeValuesData.xlsx",
        "laskin_po.pkml"))
    Condition
      Error in `loadObservedData()`:
      ! Unknown `sources`: "laskin_po.pkml". Available: "Aciclovir_TimeValuesData.xlsx", "digitized_plasma.R", "cohort_b_export.pkml", and "Digitized plasma".

---

    Code
      emptyProject$loadObservedData(sources = "laskin_po.pkml")
    Condition
      Error:
      ! Unknown `sources`: "laskin_po.pkml". The project has no observed-data sources.

# loadObservedData rejects sources that are not source ids

    Code
      loadObservedData(project, sources = NA_character_)
    Condition
      Error in `loadObservedData()`:
      ! `sources` must be a character vector with no NA or empty element.

# loadObservedData warns only about a selected programmatic source with no data

    Code
      result <- loadObservedData(project, sources = "Lost cohort")
    Condition
      Warning:
      ! 1 programmatic observed-data source resolved to no data: "Lost cohort".
      i A programmatic source holds its <DataSet> only in the session that added it; it is not saved to disk.
      i Re-add it with `addObservedDataSource()` in this session, or declare it as a `script` or `pkml` source to persist across a reload.
      This warning is displayed once per session.

# a warning about selected sources leaves the warning of a full load

    Code
      result <- loadObservedData(project)
    Condition
      Warning:
      ! 2 programmatic observed-data sources resolved to no data: "Lost cohort A" and "Lost cohort B".
      i A programmatic source holds its <DataSet> only in the session that added it; it is not saved to disk.
      i Re-add them with `addObservedDataSource()` in this session, or declare them as a `script` or `pkml` source to persist across a reload.
      This warning is displayed once per session.

# observed-data sources sharing a basename fail saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.serializeObservedDataSet()`:
      x Two observed-data sources would be saved to the same definition file 'obs.pkml.json'.
      i Each source is saved to a file named after its id: the id it was given, the name of its <DataSet>, or else its file basename. Ids that differ only in case are the same file on Windows and macOS.
      i Give the sources distinct ids, or remove one with `removeObservedDataSource()`. For a <DataSet>, remove its source by its name, rename the <DataSet>, and add it again with `addObservedDataSource()`.

# saveProject aborts on a programmatic-to-PKML basename collision

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Saving a <DataSet> added in this session would overwrite the data file of another source: 'Collide.pkml'.
      i The <DataSet> would be written to '<name>.pkml' in the data folder, a file another source already reads or another <DataSet> is written to. File names that differ only in case are the same file on Windows and macOS.
      i Before saving, remove the <DataSet> with `removeObservedDataSource()` by its name, rename it, and add it again with `addObservedDataSource()`.

# saveProject keeps the data file of a source filed under a declared id

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Saving a <DataSet> added in this session would overwrite the data file of another source: 'Cohort B.pkml'.
      i The <DataSet> would be written to '<name>.pkml' in the data folder, a file another source already reads or another <DataSet> is written to. File names that differ only in case are the same file on Windows and macOS.
      i Before saving, remove the <DataSet> with `removeObservedDataSource()` by its name, rename it, and add it again with `addObservedDataSource()`.

# saveProject aborts persisting a programmatic DataSet with no dataFolder

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Cannot save the <DataSet> "NoFolderSet" added in this session: dataFolder is not declared in `filePaths`.
      i A <DataSet> added with `addObservedDataSource()` is written to a PKML file under dataFolder on save. Declare dataFolder, then save again.

# saveProject refuses a DataSet that shares its id with another source before writing it

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      x Two observed-data sources would be saved to the same definition file 'Cohort B.json'.
      i Each source is saved to a file named after its id: the id it was given, the name of its <DataSet>, or else its file basename. Ids that differ only in case are the same file on Windows and macOS.
      i Give the sources distinct ids, or remove one with `removeObservedDataSource()`. For a <DataSet>, remove its source by its name, rename the <DataSet>, and add it again with `addObservedDataSource()`.

# addObservedDataSource rejects a duplicate declared id

    Code
      addObservedDataSource(project, list(id = "obs", type = "pkml", file = "b.pkml"))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with id "obs" already exists.
      i Pass `overwrite = TRUE` to replace it.

# a config entry cannot overwrite a live programmatic source

    Code
      addObservedDataSource(project, list(id = "prog_src", type = "pkml", file = "x.pkml"),
      overwrite = TRUE)
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with id "prog_src" is a programmatic source holding a <DataSet> in this session, so it cannot be overwritten with a configuration entry.
      i Remove it first with `removeObservedDataSource()`, then add the configuration entry.

# a DataSet clashes with the id of another source

    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "cohort_b"))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "cohort_b" already exists.
      i Pass `overwrite = TRUE` to replace it.
    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "Aciclovir_TimeValuesData.xlsx"))
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "Aciclovir_TimeValuesData.xlsx" already exists.
      i Pass `overwrite = TRUE` to replace it.

# removeObservedDataSource mutates memory only; a surviving collision aborts saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.serializeObservedDataSet()`:
      x Two observed-data sources would be saved to the same definition file 'obs.pkml.json'.
      i Each source is saved to a file named after its id: the id it was given, the name of its <DataSet>, or else its file basename. Ids that differ only in case are the same file on Windows and macOS.
      i Give the sources distinct ids, or remove one with `removeObservedDataSource()`. For a <DataSet>, remove its source by its name, rename the <DataSet>, and add it again with `addObservedDataSource()`.

# every observed-data source stores its id first and the section is keyed by it

    Code
      project$definitions$observedDataSources[["Cohort B.pkml"]]$file <- "other.pkml"
    Condition
      Error in `[[<-`:
      ! observedDataSources is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# a DataSet replaces a programmatic source with no data only with overwrite

    Code
      addObservedDataSource(project, scratch)
    Condition
      Error in `addObservedDataSource()`:
      ! Observed-data source with name "Scratch measurement" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addObservedDataSource refuses an id that differs from another only in case

    Code
      addObservedDataSource(project, list(type = "pkml", file = "sub/laskin 1982.pkml"))
    Condition
      Error in `addObservedDataSource()`:
      x The observed-data id "laskin 1982.pkml" differs only in case from the id "Laskin 1982.pkml" of another source.
      i Each source is saved to a definition file named after its id, and file names that differ only in case are the same file on Windows and macOS.
      i Give the source another id, or a <DataSet> another name. To replace the other source with one of the same kind, pass `overwrite = TRUE`.
    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "Cohort"))
    Condition
      Error in `addObservedDataSource()`:
      x The observed-data id "Cohort" differs only in case from the id "cohort" of another source.
      i Each source is saved to a definition file named after its id, and file names that differ only in case are the same file on Windows and macOS.
      i Give the source another id, or a <DataSet> another name. To replace the other source with one of the same kind, pass `overwrite = TRUE`.
    Code
      addObservedDataSource(project, ospsuite::DataSet$new(name = "cohort b"))
    Condition
      Error in `addObservedDataSource()`:
      x The observed-data id "cohort b" differs only in case from the id "Cohort B" of another source.
      i Each source is saved to a definition file named after its id, and file names that differ only in case are the same file on Windows and macOS.
      i Give the source another id, or a <DataSet> another name. To replace the other source with one of the same kind, pass `overwrite = TRUE`.

# a DataSet overwrite is refused when another source provides its name

    Code
      addObservedDataSource(project, lower, overwrite = TRUE)
    Condition
      Error in `addObservedDataSource()`:
      x Cannot replace the observed-data source "Cohort B" with the <DataSet> "cohort b": another source already provides a data set of that name or reads its data from 'cohort b.pkml'.
      i Remove the other source first with `removeObservedDataSource()` by its id, or give the <DataSet> another name.

# a DataSet overwrite is refused when another source reads its PKML file

    Code
      addObservedDataSource(project, lower, overwrite = TRUE)
    Condition
      Error in `addObservedDataSource()`:
      x Cannot replace the observed-data source "Cohort B" with the <DataSet> "cohort b": another source already provides a data set of that name or reads its data from 'cohort b.pkml'.
      i Remove the other source first with `removeObservedDataSource()` by its id, or give the <DataSet> another name.

# print.ObservedDataSource renders the source declaration

    Code
      print(project$definitions$observedDataSources[[1]])
    Output
      <ObservedDataSource>
        * Id: Aciclovir_TimeValuesData.xlsx
        * Type: excel
        * File: Aciclovir_TimeValuesData.xlsx
        * Importer Configuration: esqlabs_dataImporter_configuration.xml
        * Sheets: Laskin 1982.Group A

# printing the observedDataSources section lists the declaration ids

    Code
      print(project$definitions$observedDataSources)
    Output
      <DefinitionList>
      observedDataSources (2 definitions):
        * Aciclovir_TimeValuesData.xlsx
        * simulated_plasma

# loading a project that keeps its sources in the earlier folder aborts

    Code
      loadProject(file.path(dir, "Project.json"))
    Condition
      Error:
      ! 'Project.json' was saved by an earlier development version of esqlabsR, which used other names for observed-data sources.
      x The project's 'definitions' folder has the subfolder 'observed-data'.
      i Rename the subfolder to 'observed-data-sources'.

# loading a Project.json with the earlier observed-data key aborts

    Code
      loadProject(path)
    Condition
      Error:
      ! 'Project.json' was saved by an earlier development version of esqlabsR, which used other names for observed-data sources.
      x The file uses the key observedData.
      i Rename the key to observedDataSources.

# loading a project with the earlier folder and key aborts naming both

    Code
      loadProject(path)
    Condition
      Error:
      ! 'Project.json' was saved by an earlier development version of esqlabsR, which used other names for observed-data sources.
      x The file uses the key observedData.
      i Rename the key to observedDataSources.
      x The project's 'definitions' folder has the subfolder 'observed-data'.
      i Rename the subfolder to 'observed-data-sources'.

# loading a project with both the earlier and the current names aborts

    Code
      loadProject(path)
    Condition
      Error:
      ! 'Project.json' was saved by an earlier development version of esqlabsR, which used other names for observed-data sources.
      x The file uses both the key observedData and the key observedDataSources.
      i Move the sources listed under observedData to observedDataSources, then delete the key observedData.
      x The project's 'definitions' folder has both the subfolder 'observed-data' and the subfolder 'observed-data-sources'.
      i Move the definition files from 'observed-data' to 'observed-data-sources', then delete 'observed-data'.

# loading a project whose earlier key lists sources next to the current folder aborts

    Code
      loadProject(path)
    Condition
      Error:
      ! 'Project.json' was saved by an earlier development version of esqlabsR, which used other names for observed-data sources.
      x The file lists sources under the key observedData, but the project's 'definitions' folder already has the subfolder 'observed-data-sources', which is read instead.
      i Save each source listed under observedData as its own definition file '<id>.json' in 'observed-data-sources', then delete the key observedData.

# restoring a snapshot with the earlier observed-data key aborts

    Code
      restoreProject(snapshot, dir)
    Condition
      Error in `restoreProject()`:
      ! 'aciclovir.esqlabsR' was saved by an earlier development version of esqlabsR, which used other names for observed-data sources.
      x The file uses the key observedData.
      i Rename the key to observedDataSources.

