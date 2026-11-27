# convertProjectToV6() refuses a path that is not an esqlabsR 5 workbook

    Code
      convertProjectToV6(file.path(projectDir, "Missing.xlsx"))
    Condition
      Error in `convertProjectToV6()`:
      ! File not found: '<tmp-path>'

---

    Code
      convertProjectToV6(file.path(projectDir, "Data"))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is not an Excel file. `convertProjectToV6()` reads the 'ProjectConfiguration.xlsx' workbook of an esqlabsR 5 project.

---

    Code
      convertProjectToV6(file.path(projectDir, "Data",
        "esqlabs_dataImporter_configuration.xml"))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is not an Excel file. `convertProjectToV6()` reads the 'ProjectConfiguration.xlsx' workbook of an esqlabsR 5 project.

---

    Code
      convertProjectToV6(file.path(projectDir, "Data",
        "TestProject_TimeValuesData.xlsx"))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is not the project configuration of an esqlabsR 5 project: its first sheet has no Property and Value columns.

# convertProjectToV6() refuses a workbook that is already in the v6 layout

    Code
      convertProjectToV6(workbook)
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is already a v6 workbook: its property sheet has a schemaVersion row.
      i Import it with `importProjectFromExcel()`.

# convertProjectToV6() refuses a folder that holds a v6 project or an earlier conversion

    Code
      convertProjectToV6(configPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' already holds a v6 project or an earlier conversion: 'Project.json' and 'definitions'.
      i Load the v6 project with `loadProject()`. To convert the esqlabsR 5 workbooks again, first move these out of the folder.

---

    Code
      convertProjectToV6(configPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' already holds a v6 project or an earlier conversion: 'Project.xlsx' and 'esqlabsR5'.
      i Load the v6 project with `loadProject()`. To convert the esqlabsR 5 workbooks again, first move these out of the folder.

# convertProjectToV6() writes the v6 workbooks

    Code
      cat(.writtenWorkbookLayout(projectDir), sep = "\n")
    Output
      Project.xlsx [Project] 16 rows: Property | Value | Description
      Configurations/Applications.xlsx [ApplicationProtocols] 3 rows: ApplicationId | ParameterSets
      Configurations/Individuals.xlsx [IndividualBiometrics] 1 rows: IndividualId | Species | Population | Gender | Weight [kg] | Height [cm] | Age [year(s)] | Protein Ontogenies | ParameterSets
      Configurations/ModelParameters.xlsx [Global] 1 rows: Container Path | Parameter Name | Value | Units
      Configurations/ModelParameters.xlsx [Sheet, with comma] 1 rows: Container Path | Parameter Name | Value | Units
      Configurations/ModelParameters.xlsx [MissingParam] 1 rows: Container Path | Parameter Name | Value | Units
      Configurations/ModelParameters.xlsx [Aciclovir] 1 rows: Container Path | Parameter Name | Value | Units
      Configurations/ModelParameters.xlsx [Indiv1] 1 rows: Container Path | Parameter Name | Value | Units
      Configurations/ModelParameters.xlsx [Aciclovir_iv_250mg] 1 rows: Container Path | Parameter Name | Value | Units
      Configurations/ModelParameters.xlsx [Protocol_250mg] 1 rows: Container Path | Parameter Name | Value | Units
      Configurations/ModelParameters.xlsx [Protocol_500mg] 1 rows: Container Path | Parameter Name | Value | Units
      Configurations/ParameterIdentification.xlsx [PIOutputMappings] 4 rows: PITaskName | Scenarios | OutputPath | ObservedDataSheet | DataSet | Scaling | xOffset | yOffset | xFactor | yFactor | Weight
      Configurations/ParameterIdentification.xlsx [PIParameters] 6 rows: PITaskName | Scenarios | Container Path | Parameter Name | Units | MinValue | MaxValue | StartValue | Group
      Configurations/ParameterIdentification.xlsx [PIConfiguration] 1 rows: PITaskName | Algorithm | CIMethod | PrintEvaluationFeedback | AutoEstimateCI | numberOfCores | checkForNegativeValues | ObjectiveFunctionType | ResidualWeightingMethod | RobustMethod | ScaleVar | LinScaleCV | LogScaleSD
      Configurations/ParameterIdentification.xlsx [AlgorithmOptions] 2 rows: PITaskName | OptionName | OptionValue
      Configurations/ParameterIdentification.xlsx [CIOptions] 1 rows: PITaskName | OptionName | OptionValue
      Configurations/Plots.xlsx [DataCombined] 4 rows: DataCombinedName | dataType | label | scenario | path | dataSet | group | xOffsets | xOffsetsUnits | yOffsets | yOffsetsUnits | xScaleFactors | yScaleFactors
      Configurations/Plots.xlsx [plotConfiguration] 4 rows: plotID | DataCombinedName | plotType | title | xUnit | yUnit | xAxisScale | yAxisScale | xValuesLimits | yValuesLimits | aggregation | quantiles | nsd | foldDistance | subtitle
      Configurations/Plots.xlsx [plotGrids] 3 rows: name | plotIDs | title | subtitle
      Configurations/Plots.xlsx [exportConfiguration] 0 rows: plotGridName | outputName | width
      Configurations/Plots.xlsx [dataTypes] 2 rows: dataType
      Configurations/Plots.xlsx [plotTypes] 5 rows: plotType
      Configurations/Plots.xlsx [ObservedDataNames] 0 rows: 
      Configurations/Populations.xlsx [Demographics] 2 rows: PopulationName | species | population | numberOfIndividuals | proportionOfFemales | weightMin | weightMax | weightUnit | heightMin | heightMax | heightUnit | ageMin | ageMax | BMIMin | BMIMax | BMIUnit | Protein Ontogenies
      Configurations/Scenarios.xlsx [OutputPaths] 2 rows: OutputPathId | OutputPath
      Configurations/Scenarios.xlsx [Scenarios] 8 rows: Scenario_name | IndividualId | PopulationId | ReadPopulationFromCSV | ModelParameterSheets | ApplicationProtocol | SimulationTime | SimulationTimeUnit | SteadyState | SteadyStateTime | SteadyStateTimeUnit | OverwriteFormulasInSS | ModelFile | OutputPathsIds

# convertProjectToV6() writes the v6 property sheet

    Code
      print(.convertedProperties(projectDir))
    Output
                              Property                                  Value
      1                  schemaVersion                                    2.0
      2                esqlabsRVersion                              <version>
      3              simulationsFolder                    Models/Simulations/
      4           configurationsFolder                        Configurations/
      5                modelParamsFile                   ModelParameters.xlsx
      6                individualsFile                       Individuals.xlsx
      7                populationsFile                       Populations.xlsx
      8              populationsFolder          Configurations/PopulationsCSV
      9                  scenariosFile                         Scenarios.xlsx
      10              applicationsFile                      Applications.xlsx
      11                     plotsFile                             Plots.xlsx
      12   parameterIdentificationFile           ParameterIdentification.xlsx
      13                    dataFolder                                  Data/
      14                      dataFile        TestProject_TimeValuesData.xlsx
      15 dataImporterConfigurationFile esqlabs_dataImporter_configuration.xml
      16                  outputFolder                               Results/
      17         initialConditionsFile                 InitialConditions.xlsx

# convertProjectToV6() refuses a workbook file name outside the configurations folder

    Code
      convertProjectToV6(configPath)
    Condition
      Error in `convertProjectToV6()`:
      x plotsFile "../Plots.xlsx" resolves outside the project folder.
      i It must stay under '<tmp-path>'. A project file cannot reference a path outside the project.

# convertProjectToV6() refuses a folder outside the project folder

    Code
      convertProjectToV6(configPath)
    Condition
      Error in `convertProjectToV6()`:
      x A folder of the project points outside the project folder:
      * dataFolder: '<tmp-path>'
      i A v6 project keeps its folders inside the project folder, or names them through an environment variable. Move the folder into the project and give its path from the project folder, or write the value with the variable, such as "${SHARED}/Data", then convert again.

---

    Code
      convertProjectToV6(configPath)
    Condition
      Error in `convertProjectToV6()`:
      x A folder of the project points outside the project folder:
      * dataFolder: '../elsewhere'
      i A v6 project keeps its folders inside the project folder, or names them through an environment variable. Move the folder into the project and give its path from the project folder, or write the value with the variable, such as "${SHARED}/Data", then convert again.

---

    Code
      convertProjectToV6(configPath)
    Condition
      Error in `convertProjectToV6()`:
      x A folder of the project points outside the project folder:
      * modelFolder: '../Models/Simulations'
      i A v6 project keeps its folders inside the project folder, or names them through an environment variable. Move the folder into the project and give its path from the project folder, or write the value with the variable, such as "${SHARED}/Data", then convert again.

---

    Code
      convertProjectToV6(configPath)
    Condition
      Error in `convertProjectToV6()`:
      x A folder of the project points outside the project folder:
      * outputFolder: 'Results/../../Results'
      i A v6 project keeps its folders inside the project folder, or names them through an environment variable. Move the folder into the project and give its path from the project folder, or write the value with the variable, such as "${SHARED}/Data", then convert again.

---

    Code
      convertProjectToV6(configPath)
    Condition
      Error in `convertProjectToV6()`:
      x configurationsFolder points outside the project folder: '../Configurations'.
      i The esqlabsR 5 workbooks move into the 'esqlabsR5' folder of the project, so they have to be inside it. Move the configurations folder into the project and give its path from the project folder, then convert again.

---

    Code
      convertProjectToV6(configPath)
    Condition
      Error in `convertProjectToV6()`:
      x configurationsFolder points outside the project folder: '${ESQLABSR_TEST_CONFIGURATIONS}'.
      i The esqlabsR 5 workbooks move into the 'esqlabsR5' folder of the project, so they have to be inside it. Move the configurations folder into the project and give its path from the project folder, then convert again.

# the conversion summary names each folder value it rewrote

    Code
      cat(summary, sep = "\n")
    Output
      v Converted the esqlabsR 5 project. Its esqlabsR 5 workbooks are now in '<tmp-path>'.
      i Edit the v6 workbooks and import them with `importProjectFromExcel()`, or load 'Project.json' with `loadProject()`.
      i Folders name an environment variable the esqlabsR 5 way, as a part of the path. A v6 project writes it as `${NAME}`, so the values were rewritten:
      * modelFolder: "MDL/Simulations/" -> "${MDL}/Simulations/"
      * dataFolder: "OBSDATA" -> "${OBSDATA}"
      i A sheet with no place in the v6 workbooks was not carried over; the esqlabsR 5 workbooks in 'esqlabsR5' keep it:
      * 'Populations.xlsx': "UserDefinedVariability"

# an Individual Parameter Sets cell naming another set is reported

    Code
      cat(conditionMessage(warning))
    Output
      ! The Individual Parameter Sets column of '<tmp-path>' names a parameter set that esqlabsR 5 did not apply, and that is not linked to the individual:
      x Individual "Adult": "Global".
      i esqlabsR 5 applies to an individual the sheet named after it, and so does the converted project. To apply another set as well, add it to the individual's ParameterSets cell in the v6 individuals workbook and import again.

# a filled UserDefinedVariability sheet is reported, an empty one is not

    Code
      cat(conditionMessage(warning))
    Output
      ! The UserDefinedVariability sheet of '<tmp-path>' is not converted: 1 row of parameter variability.
      i esqlabsR 5 did not read the sheet, and a v6 population has no field for it. The esqlabsR 5 workbook in 'esqlabsR5' keeps it.

# convertProjectToV6() stops on a scenario boolean it cannot read, before changing the folder

    Code
      convertProjectToV6(legacyExcelProjectPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x Cannot read the SteadyState value "maybe" in '<tmp-path>' as true or false.
      i Use "TRUE" / "FALSE", "1" / "0" or "Yes" / "No", then convert again.

