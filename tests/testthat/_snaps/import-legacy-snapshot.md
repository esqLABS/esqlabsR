# loadProject on a previous-version snapshot points at importProjectFromExcel

    Code
      loadProject(snapshot)
    Condition
      Error:
      x '<path>' is a previous-version project snapshot, not a project of the current format.
      i A previous-version snapshot has to be upgraded before it can be opened.
      i Upgrade it with `importProjectFromExcel(<snapshot>, outputDir = <newFolder>)`, then open the new project with `loadProject()`.

# importProjectFromExcel names the asset folders a lone snapshot could not bring

    Code
      importProjectFromExcel(snapshot, outputDir = dir)
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
      i Detected a previous-version project snapshot and upgraded it to the current project format.
      ! Observed data does not travel in a snapshot; add it with `addObservedData()` if a plot or parameter identification needs it.
      ! 2 referenced folders are missing from the upgraded project: 'Models/Simulations' and 'Data'.
      i A snapshot carries only the configuration workbooks, so a model, data or population folder travels with it only when it sits beside the snapshot file at that relative path.
      i Nothing that reads from one of them will resolve until you place the folder in the project.

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
      ! '<tmp-path>' is not a project configuration file from an earlier esqlabsR version.
      i It is already a project in the current format. Open it with `loadProject()`.

# importProjectFromExcel rejects a JSON file that is no project configuration

    Code
      importProjectFromExcel(other, outputDir = withr::local_tempdir())
    Condition
      Error in `importProjectFromExcel()`:
      ! '<tmp-path>' is not a project configuration file from an earlier esqlabsR version.
      i `importProjectFromExcel()` reads a 'Project.xlsx' workbook, or the JSON file that `snapshotProjectConfiguration()` wrote in esqlabsR 5.

# importProjectFromExcel names a JSON file it cannot read

    Code
      cat(rlang::cnd_message(error, inherit = FALSE), "\n")
    Output
      '<tmp-path>' could not be read as a JSON file.
      i The file may be incomplete or damaged. `importProjectFromExcel()` reads a 'Project.xlsx' workbook, or the JSON file that `snapshotProjectConfiguration()` wrote in esqlabsR 5. 

