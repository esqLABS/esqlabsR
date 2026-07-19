# importProjectFromExcel aborts when two ids canonicalize to the same value

    Code
      importProjectFromExcel(file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(), silent = TRUE)
    Condition
      Error in `.canonicalizeId()`:
      ! Ids collide after canonicalization:
      x "Aciclovir_PVB" and "aciclovir_pvb" -> "aciclovir_pvb"
      i Two distinct ids that canonicalize to the same id are ambiguous; rename so they differ by more than case or forbidden characters.

# .parseExcelParameterSheets aborts on a non-numeric Value cell

    Code
      .parseExcelParameterSheets(paramFile)
    Condition
      Error in `.parseNumericCell()`:
      ! Cannot interpret the Value cell as a number.
      x Sheet "Global", row 1: "not_a_number".
      i A blank cell is allowed; a non-blank cell must be numeric (use "." as the decimal separator).

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
      i Expected columns: "Scenario_name", "IndividualId", "PopulationId", "ReadPopulationFromCSV", "ModelParameterSheets", "ApplicationProtocol", "SimulationTime", "SimulationTimeUnit", "SteadyState", "SteadyStateTime", "SteadyStateTimeUnit", "OverwriteFormulasInSS", "ModelFile", and "OutputPathsIds".

