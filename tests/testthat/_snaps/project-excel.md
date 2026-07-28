# importProjectFromExcel aborts over an existing JSON project unless overwrite = TRUE

    Code
      suppressWarnings(importProjectFromExcel(testProjectExcelPath(), outputDir = out,
      silent = TRUE))
    Condition
      Error in `importProjectFromExcel()`:
      ! A JSON project already exists in '<tmp-path>'.
      x Re-importing replaces it with the Excel project and deletes any definitions that exist only on the JSON side.
      i Pass `overwrite = TRUE` to replace the existing JSON project with the Excel state, or import into a different `outputDir`.

# exportProjectToExcel aborts over existing workbooks unless overwrite = TRUE

    Code
      exportProjectToExcel(project, outputDir = excelOut, silent = TRUE)
    Condition
      Error in `exportProjectToExcel()`:
      ! Excel workbooks already exist in '<tmp-path>'.
      x Exporting overwrites 'Project.xlsx' and the 'Configurations' workbooks, discarding any hand-edits they carry.
      i Pass `overwrite = TRUE` to replace the existing workbooks, or export into a different `outputDir`.

# importProjectFromExcel aborts when two ids canonicalize to the same value

    Code
      importProjectFromExcel(file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(), silent = TRUE)
    Condition
      Error in `.canonicalizeId()`:
      ! Ids collide after canonicalization:
      x "Aciclovir_PVB" and "aciclovir_pvb" -> "aciclovir_pvb"
      i Two distinct ids that canonicalize to the same id are ambiguous; rename so they differ by more than case or forbidden characters.

# importProjectFromExcel warns naming each renamed duplicate parameter set

    Code
      cat(renameWarning)
    Output
      ! 1 parameter set in 'Individuals.xlsx' reuses an id that is already taken, so it was renamed:
      * "Global" -> "Global_1"
      i The three former parameter-set kinds now share one parameterSets namespace, so one sheet name cannot serve two sets. References made in 'Individuals.xlsx' point at the renamed set; rename the sheet in Excel to choose the id yourself.

# .parseExcelScenarios aborts on an unparseable boolean cell

    Code
      .parseExcelScenarios(scenarioDf)
    Condition
      Error in `.toLogical()`:
      ! Cannot interpret SteadyState value "maybe" as a logical.
      i Use a boolean-like value ("TRUE"/"FALSE", "1"/"0", "Yes"/"No").

# .parseExcelScenarios aborts on a renamed required column

    Code
      .parseExcelScenarios(scenarioDf)
    Condition
      Error in `.parseExcelScenarios()`:
      ! The Scenarios sheet is missing required column: "OutputPathsIds".
      i Expected columns: "Scenario_name", "IndividualId", "PopulationId", "ReadPopulationFromCSV", "ModelParameterSheets", "ApplicationProtocol", "SimulationTime", "SimulationTimeUnit", "SteadyState", "SteadyStateTime", "SteadyStateTimeUnit", "ModelFile", and "OutputPathsIds".

