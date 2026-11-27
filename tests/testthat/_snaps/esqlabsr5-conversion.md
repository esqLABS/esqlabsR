# convertProjectToV6() takes one path and one TRUE or FALSE

    Code
      convertProjectToV6(c("A.xlsx", "B.xlsx"))
    Condition
      Error in `ospsuite.utils::validateIsOfLength()`:
      ! `test()`: Object should be of length 1, but is of length 2 instead.
    Code
      convertProjectToV6(NA_character_)
    Condition
      Error in `convertProjectToV6()`:
      x `projectConfigPath` must not be `NA`.
    Code
      convertProjectToV6("A.xlsx", silent = c(TRUE, FALSE))
    Condition
      Error in `ospsuite.utils::validateIsOfLength()`:
      ! `test()`: Object should be of length 1, but is of length 2 instead.
    Code
      convertProjectToV6("A.xlsx", silent = NA)
    Condition
      Error in `convertProjectToV6()`:
      x `silent` must not be `NA`.

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

# convertProjectToV6() puts the folder back when the import fails

    Code
      convertProjectToV6(legacyExcelProjectPath(projectDir), silent = TRUE)
    Condition
      Error in `convertProjectToV6()`:
      x The conversion stopped while importing the v6 workbooks it wrote, and the project folder was put back as it was.
      i The error below comes from the v6 workbooks, which the conversion wrote from the esqlabsR 5 ones. Fix what it points at in the esqlabsR 5 workbooks, then convert again.
      Caused by error in `.canonicalizeId()`:
      ! Ids collide after canonicalization:
      x "Adult Scenario" and "Adult,Scenario" -> "adult_scenario"
      i Two distinct ids that canonicalize to the same id are ambiguous; rename so they differ by more than case or forbidden characters.

# a file the restore cannot move back is named, and kept in esqlabsR5/

    Code
      cat(warnings, sep = "\n")
    Output
      ! The project folder could not be put back completely. These files are not where they were before: 'ProjectConfiguration.xlsx', 'Configurations/Scenarios.xlsx', 'Configurations/ModelParameters.xlsx', 'Configurations/Individuals.xlsx', 'Configurations/Populations.xlsx', 'Configurations/Applications.xlsx', 'Configurations/Plots.xlsx', and 'Configurations/ParameterIdentification.xlsx'.
      i Find them in the 'esqlabsR5' folder and move them back by hand.

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
      Configurations/ParameterIdentification.xlsx [PITasks] 3 rows: taskId | scenarios | config.algorithm | config.ciMethod | config.autoEstimateCI | config.printEvaluationFeedback | config.algorithmOptions.maxeval | config.algorithmOptions.ftol_rel | config.ciOptions.confLevel
      Configurations/ParameterIdentification.xlsx [PIParameters] 5 rows: taskId | id | scenarios | path | units | minValue | maxValue | startValue
      Configurations/ParameterIdentification.xlsx [PIOutputMappings] 4 rows: taskId | id | scenarios | outputPath | observedData | scaling | xOffset | yOffset | xFactor | yFactor | weight
      Configurations/Plots.xlsx [DataCombined] 4 rows: DataCombinedName | dataType | label | scenario | path | dataSet | group | xOffsets | xOffsetsUnits | yOffsets | yOffsetsUnits | xScaleFactors | yScaleFactors
      Configurations/Plots.xlsx [plotConfiguration] 4 rows: plotId | dataCombined | plotType | title | xUnit | yUnit | xAxisScale | yAxisScale | xValuesLimits | yValuesLimits | aggregation | quantiles | nsd | foldDistance | subtitle
      Configurations/Plots.xlsx [plotGrids] 3 rows: plotGridId | plots | title | subtitle
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

# convertProjectToV6() refuses a workbook linked from outside the project folder

    Code
      convertProjectToV6(legacyExcelProjectPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x Workbooks of the project lie outside the project folder once links are followed: 'Configurations/Scenarios.xlsx', 'Configurations/ModelParameters.xlsx', 'Configurations/Individuals.xlsx', 'Configurations/Populations.xlsx', 'Configurations/Applications.xlsx', 'Configurations/Plots.xlsx', and 'Configurations/ParameterIdentification.xlsx'.
      i The esqlabsR 5 workbooks move into the 'esqlabsR5' folder of the project, which would take them out of the folder the link points into. Copy the files or their folder into the project instead of linking them, then convert again.

# convertProjectToV6() refuses a workbook file name outside the configurations folder

    Code
      convertProjectToV6(configPath)
    Condition
      Error in `convertProjectToV6()`:
      x plotsFile "../Plots.xlsx" resolves outside the project folder.
      i It must stay under '<tmp-path>'. A project file cannot reference a path outside the project.

# convertProjectToV6() refuses a project whose workbooks it cannot find

    Code
      convertProjectToV6(legacyExcelProjectPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x No esqlabsR 5 workbook was found in the configurations folder 'Missing', so there is nothing to convert.
      i Point configurationsFolder at the folder of the project that holds the workbooks, then convert again.

---

    Code
      convertProjectToV6(legacyExcelProjectPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x No esqlabsR 5 workbook was found in the configurations folder 'MYCONF', so there is nothing to convert.
      i "MYCONF" is the name of a set environment variable, which esqlabsR 5 replaced by its value. The conversion does not: the configurations folder has to be a folder of the project.
      i Point configurationsFolder at the folder of the project that holds the workbooks, then convert again.

---

    Code
      convertProjectToV6(legacyExcelProjectPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x The property sheet names no configurationsFolder, so there is no esqlabsR 5 workbook to convert.
      i Point configurationsFolder at the folder of the project that holds the workbooks, then convert again.

# convertProjectToV6() refuses the esqlabsR 5 workbooks a conversion kept

    Code
      convertProjectToV6(file.path(projectDir, "esqlabsR5",
        "ProjectConfiguration.xlsx"))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is in the 'esqlabsR5' folder of the converted project '<tmp-path>', which keeps the esqlabsR 5 workbooks of that conversion.
      i Load the converted project with `loadProject()`. To convert the esqlabsR 5 workbooks again, first move them back to where they were in the project folder, and the files of the v6 project out of it.

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
      i Folders name an environment variable the esqlabsR 5 way, as a part of the path. A v6 project writes them as `${NAME}`, so the values were rewritten:
      * modelFolder: "MDL/Simulations/" -> "${MDL}/Simulations/"
      * dataFolder: "OBSDATA" -> "${OBSDATA}"
      i Some sheets with no place in the v6 workbooks were not carried over; the esqlabsR 5 workbooks in 'esqlabsR5' keep them:
      * 'Populations.xlsx': "UserDefinedVariability"
      * 'Plots.xlsx': "exportConfiguration", "dataTypes", "plotTypes", and "ObservedDataNames"

# an Individual Parameter Sets cell naming another set is reported

    Code
      cat(conditionMessage(warning))
    Output
      ! The Individual Parameter Sets column of '<tmp-path>' names a parameter set that esqlabsR 5 did not apply, and that is not linked to the individual:
      x Individual "Adult": "Global".
      i esqlabsR 5 applies to an individual the sheet named after it, and so does the converted project. To apply another set as well, add it to the individual's ParameterSets cell in the v6 individuals workbook and import again.

# a filled UserDefinedVariability sheet is reported

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
      i Use `TRUE` / `FALSE`, `1` / `0` or `Yes` / `No`, then convert again.

# convertProjectToV6() writes the parameter-identification sheets an export writes

    Code
      for (sheet in readxl::excel_sheets(piFile)) {
        cat(sheet, "\n")
        print(as.data.frame(readExcel(piFile, sheet = sheet)))
      }
    Output
      PITasks 
              taskId               scenarios config.algorithm config.ciMethod
      1 AciclovirFit PIScenario, PIScenario2           BOBYQA         hessian
        config.autoEstimateCI config.printEvaluationFeedback
      1                 FALSE                           TRUE
        config.algorithmOptions.maxeval config.algorithmOptions.ftol_rel
      1                             100                                0
        config.ciOptions.confLevel
      1                       0.95
      PIParameters 
              taskId            id               scenarios
      1 AciclovirFit lipophilicity              PIScenario
      2 AciclovirFit        tsspec PIScenario, PIScenario2
                                                                                     path
      1                                                           Aciclovir|Lipophilicity
      2 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec
            units minValue maxValue startValue
      1 Log Units      -10       10        1.0
      2        mg        0       10        0.5
      PIOutputMappings 
              taskId                                 id   scenarios    outputPath
      1 AciclovirFit   plasma_(peripheral_venous_blood)  PIScenario Aciclovir_PVB
      2 AciclovirFit plasma_(peripheral_venous_blood)_2 PIScenario2 Aciclovir_PVB
                                                                                 observedData
      1 Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_
      2 Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_
        scaling xOffset yOffset xFactor yFactor weight
      1     lin      NA      NA      NA      NA     NA
      2     lin      NA      NA      NA      NA     NA

# an ObservedDataSheet the data workbook lacks is reported

    Code
      cat(conditionMessage(warning))
    Output
      ! A sheet named in the ObservedDataSheet column of the PIOutputMappings sheet is not in the data workbook: "Laskin 1982.Group B".
      i A v6 project imports every sheet of the data workbook and finds a mapping's data set by its name, so the column is not carried over. A data set on a sheet the workbook does not have is not in the project: the mapping's DataSet does not resolve, and `validateProject()` reports it.
      i Point dataFile at the workbook that holds it in the v6 property sheet and import again.

# .v5ParsePIMappings keeps and reports an underivable path

    Code
      cat(conditionMessage(warning))
    Output
      ! 1 output mapping of parameter-identification task "t1" has no output path.
      x Scenarios cell "s1".
      i This PIOutputMappings sheet has no OutputPath column, so each mapping takes its outputs from the OutputPathsIds of the scenarios it names, and these scenarios name none.
      i The mappings are kept, so `validateProject()` reports each one until its outputPath cell in the v6 PIOutputMappings sheet is filled and the workbooks are imported again.

# an unpairable ontogeny declaration warns instead of dropping it in silence

    Code
      indiv <- .convertedIndividuals(.legacyOntogenyIndividual("Indiv1",
        "CYP3A4,CYP2D6", "CYP3A4"))
    Condition
      Warning:
      ! The protein ontogenies of individual "Indiv1" are not converted: the Protein and Ontogeny columns cannot be paired.
      i 2 proteins against 1 ontogeny; each protein needs exactly one ontogeny.
      i Write the pairs into the Protein Ontogenies cell of the v6 workbook as "Protein:Ontogeny,Protein:Ontogeny" and import again.

---

    Code
      pop <- .convertedPopulations(.legacyOntogenyPopulation("Pop1", "CYP3A4", NA))
    Condition
      Warning:
      ! The protein ontogenies of population "Pop1" are not converted: the Protein and Ontogeny columns cannot be paired.
      i 1 protein against 0 ontogenies; each protein needs exactly one ontogeny.
      i Write the pairs into the Protein Ontogenies cell of the v6 workbook as "Protein:Ontogeny,Protein:Ontogeny" and import again.

# the conversion warns naming each renamed duplicate parameter set

    Code
      cat(renameWarning)
    Output
      ! 1 parameter set in 'Individuals.xlsx' reuses an id that is already taken, so it was renamed:
      * "Global" -> "Global_2"
      i A v6 project keeps every parameter set in the model-parameters workbook, so one sheet name cannot serve two sets. References made in 'Individuals.xlsx' point at the renamed set; rename the sheet and its references in the v6 workbooks to choose the id yourself.

