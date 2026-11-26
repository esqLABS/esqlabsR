# loadProject on a previous-version snapshot points at importProjectFromExcel

    Code
      loadProject(snapshot)
    Condition
      Error:
      x '<path>' is a project configuration saved by `snapshotProjectConfiguration()` in esqlabsR 5, not a project of the current format.
      i It has to be upgraded before it can be opened.
      i Upgrade it with `importProjectFromExcel(<file>, outputDir = <newFolder>)`, then open the new project with `loadProject()`.

# importProjectFromExcel warns about a snapshot's missing data file, also when silent

    Code
      importProjectFromExcel(snapshot, outputDir = dir, silent = TRUE)
    Condition
      Warning:
      ! The configured data file 'TestProject_TimeValuesData.xlsx' was not found, so no observed data was imported.
      i Any plot or parameter-identification mapping that references observed data will not resolve, and `validateProject()` will report it, until the data file is present.

# importProjectFromExcel names the asset folders a lone snapshot could not bring

    Code
      importProjectFromExcel(snapshot, outputDir = dir)
    Condition
      Warning:
      ! The configured data file 'TestProject_TimeValuesData.xlsx' was not found, so no observed data was imported.
      i Any plot or parameter-identification mapping that references observed data will not resolve, and `validateProject()` will report it, until the data file is present.
    Message
      Imported '<tmp-path>' into the JSON project '<tmp-path>'.
      -- Definitions -----------------------------------------------------------------
        * Scenarios: 8
        * Individuals: 1
        * Populations: 2
        * Parameter Sets: 8
        * Applications: 3
        * Output Paths: 2
        * Data Combined: 2
        * Plots: 4
        * Plot Grids: 3
        * Parameter Identification: 3
      2 sections imported no definitions: initialConditions and observedData.
      Copied 1 referenced folder into the new project: 'Configurations/PopulationsCSV'.
      i Upgraded the project configuration that `snapshotProjectConfiguration()` saved in esqlabsR 5 to the current project format.
      ! 2 referenced folders are missing from the upgraded project: 'Models/Simulations' and 'Data'.
      i The JSON file holds only the configuration, so a model, data or population folder is found only when it sits beside the file at that relative path.
      i Nothing that reads from one of them will resolve until you place the folder in the project.

# importProjectFromExcel copies a snapshot's observed-data files when the data folder is the project folder

    Code
      writeLines(importAssetLines(importProjectFromExcel(file.path(src,
        "ProjectConfiguration.json"), outputDir = outputDir)))
    Output
      Copied 2 referenced folders into the new project: 'Models/Simulations/' and 'Configurations/PopulationsCSV'.
      Copied 2 observed-data files into the new project: 'TestProject_TimeValuesData.xlsx' and 'esqlabs_dataImporter_configuration.xml'.

# importProjectFromExcel adds a snapshot's missing population file to an existing folder in place

    Code
      importProjectFromExcel(snapshot, silent = FALSE)
    Message
      Imported '<tmp-path>' into the JSON project '<tmp-path>'.
      -- Definitions -----------------------------------------------------------------
        * Scenarios: 8
        * Individuals: 1
        * Populations: 2
        * Parameter Sets: 8
        * Applications: 3
        * Output Paths: 2
        * Observed Data: 1
        * Data Combined: 2
        * Plots: 4
        * Plot Grids: 3
        * Parameter Identification: 3
      1 section imported no definitions: initialConditions.
      Added 1 file the project folders lacked: 'Configurations/PopulationsCSV/TestPopulation.csv'.
      i Upgraded the project configuration that `snapshotProjectConfiguration()` saved in esqlabsR 5 to the current project format.

# importProjectFromExcel refuses to import a snapshot over an existing project unless overwrite

    Code
      importProjectFromExcel(.legacySnapshotFixture(), outputDir = dir)
    Condition
      Error in `importProjectFromExcel()`:
      ! A JSON project already exists in '<tmp-path>'.
      x Importing replaces it with the upgraded project and deletes any definitions that the imported file does not contain.
      i Pass `overwrite = TRUE` to replace the existing JSON project, or import into a different `outputDir`.

# importProjectFromExcel refuses to write the project over the snapshot it imports

    Code
      importProjectFromExcel(snapshot, projectFileName = "ProjectConfiguration.json",
        overwrite = TRUE)
    Condition
      Error in `importProjectFromExcel()`:
      ! The project file would replace '<tmp-path>', the file being imported.
      i Choose another `projectFileName` or `outputDir`.

# importProjectFromExcel points a current project file at loadProject

    Code
      importProjectFromExcel(project$info$projectFilePath, outputDir = withr::local_tempdir())
    Condition
      Error in `importProjectFromExcel()`:
      ! '<tmp-path>' is not the JSON file that `snapshotProjectConfiguration()` saved in esqlabsR 5.
      i It is already a project in the current format. Open it with `loadProject()`.

# importProjectFromExcel rejects a JSON file that is no project configuration

    Code
      importProjectFromExcel(other, outputDir = withr::local_tempdir())
    Condition
      Error in `importProjectFromExcel()`:
      ! '<tmp-path>' is not the JSON file that `snapshotProjectConfiguration()` saved in esqlabsR 5.
      i `importProjectFromExcel()` reads a 'Project.xlsx' workbook, or the JSON file that `snapshotProjectConfiguration()` saved in esqlabsR 5.

# importProjectFromExcel names a JSON file it cannot read

    Code
      cat(rlang::cnd_message(error, inherit = FALSE), "\n")
    Output
      '<tmp-path>' could not be read as a JSON file.
      i The file may be incomplete or damaged. `importProjectFromExcel()` reads a 'Project.xlsx' workbook, or the JSON file that `snapshotProjectConfiguration()` saved in esqlabsR 5. 

