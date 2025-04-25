# snapshotProjectConfiguration exports project configuration to JSON with correct filename

    Code
      config_data
    Output
      $files
      $files$projectConfiguration
      $files$projectConfiguration$ProjectConfiguration
      $files$projectConfiguration$ProjectConfiguration$column_names
      [1] "Property"    "Value"       "Description"
      
      $files$projectConfiguration$ProjectConfiguration$rows
      $files$projectConfiguration$ProjectConfiguration$rows[[1]]
      $files$projectConfiguration$ProjectConfiguration$rows[[1]]$Property
      [1] "modelFolder"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[1]]$Value
      [1] "Models/Simulations/"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[1]]$Description
      [1] "Path to the folder with pkml simulation files; relative to the location of this file"
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[2]]
      $files$projectConfiguration$ProjectConfiguration$rows[[2]]$Property
      [1] "configurationsFolder"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[2]]$Value
      [1] "Configurations/"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[2]]$Description
      [1] "Path to the folder with excel files with parametrization; relative to the location of this file"
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[3]]
      $files$projectConfiguration$ProjectConfiguration$rows[[3]]$Property
      [1] "modelParamsFile"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[3]]$Value
      [1] "ModelParameters.xlsx"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[3]]$Description
      [1] "Name of the excel file with global model parametrization. Must be located in the \"paramsFolder\""
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[4]]
      $files$projectConfiguration$ProjectConfiguration$rows[[4]]$Property
      [1] "individualsFile"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[4]]$Value
      [1] "Individuals.xlsx"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[4]]$Description
      [1] "Name of the excel file with individual-specific model parametrization. Must be located in the \"paramsFolder\""
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[5]]
      $files$projectConfiguration$ProjectConfiguration$rows[[5]]$Property
      [1] "populationsFile"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[5]]$Value
      [1] "Populations.xlsx"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[5]]$Description
      [1] "Name of the excel file with population information. Must be located in the \"paramsFolder\""
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[6]]
      $files$projectConfiguration$ProjectConfiguration$rows[[6]]$Property
      [1] "populationsFolder"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[6]]$Value
      [1] "PopulationsCSV"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[6]]$Description
      [1] "Name of the folder containing population defined in files"
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[7]]
      $files$projectConfiguration$ProjectConfiguration$rows[[7]]$Property
      [1] "scenariosFile"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[7]]$Value
      [1] "Scenarios.xlsx"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[7]]$Description
      [1] "Name of the excel file with scenario definitions. Must be located in the \"paramsFolder\""
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[8]]
      $files$projectConfiguration$ProjectConfiguration$rows[[8]]$Property
      [1] "applicationsFile"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[8]]$Value
      [1] "Applications.xlsx"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[8]]$Description
      [1] "Name of the excel file scenario-specific parameters such as application protocol parameters. Must be located in the \"paramsFolder\""
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[9]]
      $files$projectConfiguration$ProjectConfiguration$rows[[9]]$Property
      [1] "plotsFile"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[9]]$Value
      [1] "Plots.xlsx"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[9]]$Description
      [1] "Name of the excel file with plot definitions. Must be located in the \"paramsFolder\""
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[10]]
      $files$projectConfiguration$ProjectConfiguration$rows[[10]]$Property
      [1] "dataFolder"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[10]]$Value
      [1] "Data/"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[10]]$Description
      [1] "Path to the folder where experimental data files are located; relative to the location of this file"
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[11]]
      $files$projectConfiguration$ProjectConfiguration$rows[[11]]$Property
      [1] "dataFile"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[11]]$Value
      [1] "TestProject_TimeValuesData.xlsx"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[11]]$Description
      [1] "Name of the excel file with experimental data. Must be located in the \"dataFolder\""
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[12]]
      $files$projectConfiguration$ProjectConfiguration$rows[[12]]$Property
      [1] "dataImporterConfigurationFile"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[12]]$Value
      [1] "esqlabs_dataImporter_configuration.xml"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[12]]$Description
      [1] "Name of data importer configuration file in xml format used to load the data. Must be located in the \"dataFolder\""
      
      
      $files$projectConfiguration$ProjectConfiguration$rows[[13]]
      $files$projectConfiguration$ProjectConfiguration$rows[[13]]$Property
      [1] "outputFolder"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[13]]$Value
      [1] "Results/"
      
      $files$projectConfiguration$ProjectConfiguration$rows[[13]]$Description
      [1] "Path to the folder where the results should be saved to; relative to the location of this file"
      
      
      
      
      
      $files$modelParameters
      $files$modelParameters$Global
      $files$modelParameters$Global$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$modelParameters$Global$rows
      $files$modelParameters$Global$rows[[1]]
      $files$modelParameters$Global$rows[[1]]$`Container Path`
      [1] "Organism|Liver"
      
      $files$modelParameters$Global$rows[[1]]$`Parameter Name`
      [1] "EHC continuous fraction"
      
      $files$modelParameters$Global$rows[[1]]$Value
      [1] "1"
      
      $files$modelParameters$Global$rows[[1]]$Units
      [1] NA
      
      
      
      
      $files$modelParameters$MissingParam
      $files$modelParameters$MissingParam$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$modelParameters$MissingParam$rows
      $files$modelParameters$MissingParam$rows[[1]]
      $files$modelParameters$MissingParam$rows[[1]]$`Container Path`
      [1] "foo"
      
      $files$modelParameters$MissingParam$rows[[1]]$`Parameter Name`
      [1] "bar"
      
      $files$modelParameters$MissingParam$rows[[1]]$Value
      [1] "2"
      
      $files$modelParameters$MissingParam$rows[[1]]$Units
      [1] NA
      
      
      
      
      $files$modelParameters$Aciclovir
      $files$modelParameters$Aciclovir$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$modelParameters$Aciclovir$rows
      $files$modelParameters$Aciclovir$rows[[1]]
      $files$modelParameters$Aciclovir$rows[[1]]$`Container Path`
      [1] "Aciclovir"
      
      $files$modelParameters$Aciclovir$rows[[1]]$`Parameter Name`
      [1] "Lipophilicity"
      
      $files$modelParameters$Aciclovir$rows[[1]]$Value
      [1] "-0.1"
      
      $files$modelParameters$Aciclovir$rows[[1]]$Units
      [1] "Log Units"
      
      
      
      
      
      $files$individuals
      $files$individuals$IndividualBiometrics
      $files$individuals$IndividualBiometrics$column_names
      [1] "IndividualId"  "Species"       "Population"    "Gender"       
      [5] "Weight [kg]"   "Height [cm]"   "Age [year(s)]" "Protein"      
      [9] "Ontogeny"     
      
      $files$individuals$IndividualBiometrics$rows
      $files$individuals$IndividualBiometrics$rows[[1]]
      $files$individuals$IndividualBiometrics$rows[[1]]$IndividualId
      [1] "Indiv1"
      
      $files$individuals$IndividualBiometrics$rows[[1]]$Species
      [1] "Human"
      
      $files$individuals$IndividualBiometrics$rows[[1]]$Population
      [1] "European_ICRP_2002"
      
      $files$individuals$IndividualBiometrics$rows[[1]]$Gender
      [1] "MALE"
      
      $files$individuals$IndividualBiometrics$rows[[1]]$`Weight [kg]`
      [1] "73"
      
      $files$individuals$IndividualBiometrics$rows[[1]]$`Height [cm]`
      [1] "176"
      
      $files$individuals$IndividualBiometrics$rows[[1]]$`Age [year(s)]`
      [1] "30"
      
      $files$individuals$IndividualBiometrics$rows[[1]]$Protein
      [1] NA
      
      $files$individuals$IndividualBiometrics$rows[[1]]$Ontogeny
      [1] NA
      
      
      
      
      $files$individuals$Indiv1
      $files$individuals$Indiv1$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$individuals$Indiv1$rows
      $files$individuals$Indiv1$rows[[1]]
      $files$individuals$Indiv1$rows[[1]]$`Container Path`
      [1] "Organism|Kidney"
      
      $files$individuals$Indiv1$rows[[1]]$`Parameter Name`
      [1] "GFR"
      
      $files$individuals$Indiv1$rows[[1]]$Value
      [1] "90"
      
      $files$individuals$Indiv1$rows[[1]]$Units
      [1] "ml/min"
      
      
      
      
      
      $files$populations
      $files$populations$Demographics
      $files$populations$Demographics$column_names
       [1] "PopulationName"      "species"             "population"         
       [4] "numberOfIndividuals" "proportionOfFemales" "weightMin"          
       [7] "weightMax"           "weightUnit"          "heightMin"          
      [10] "heightMax"           "heightUnit"          "ageMin"             
      [13] "ageMax"              "BMIMin"              "BMIMax"             
      [16] "BMIUnit"             "Protein"             "Ontogeny"           
      
      $files$populations$Demographics$rows
      $files$populations$Demographics$rows[[1]]
      $files$populations$Demographics$rows[[1]]$PopulationName
      [1] "TestPopulation"
      
      $files$populations$Demographics$rows[[1]]$species
      [1] "Human"
      
      $files$populations$Demographics$rows[[1]]$population
      [1] "European_ICRP_2002"
      
      $files$populations$Demographics$rows[[1]]$numberOfIndividuals
      [1] "2"
      
      $files$populations$Demographics$rows[[1]]$proportionOfFemales
      [1] "0"
      
      $files$populations$Demographics$rows[[1]]$weightMin
      [1] NA
      
      $files$populations$Demographics$rows[[1]]$weightMax
      [1] NA
      
      $files$populations$Demographics$rows[[1]]$weightUnit
      [1] "kg"
      
      $files$populations$Demographics$rows[[1]]$heightMin
      [1] NA
      
      $files$populations$Demographics$rows[[1]]$heightMax
      [1] NA
      
      $files$populations$Demographics$rows[[1]]$heightUnit
      [1] "cm"
      
      $files$populations$Demographics$rows[[1]]$ageMin
      [1] "22"
      
      $files$populations$Demographics$rows[[1]]$ageMax
      [1] "41"
      
      $files$populations$Demographics$rows[[1]]$BMIMin
      [1] NA
      
      $files$populations$Demographics$rows[[1]]$BMIMax
      [1] NA
      
      $files$populations$Demographics$rows[[1]]$BMIUnit
      [1] "kg/m²"
      
      $files$populations$Demographics$rows[[1]]$Protein
      [1] "CYP3A4, CYP2D6"
      
      $files$populations$Demographics$rows[[1]]$Ontogeny
      [1] "CYP3A4, CYP2D6"
      
      
      $files$populations$Demographics$rows[[2]]
      $files$populations$Demographics$rows[[2]]$PopulationName
      [1] "TestPopulation_noOnto"
      
      $files$populations$Demographics$rows[[2]]$species
      [1] "Human"
      
      $files$populations$Demographics$rows[[2]]$population
      [1] "European_ICRP_2002"
      
      $files$populations$Demographics$rows[[2]]$numberOfIndividuals
      [1] "2"
      
      $files$populations$Demographics$rows[[2]]$proportionOfFemales
      [1] "0"
      
      $files$populations$Demographics$rows[[2]]$weightMin
      [1] NA
      
      $files$populations$Demographics$rows[[2]]$weightMax
      [1] NA
      
      $files$populations$Demographics$rows[[2]]$weightUnit
      [1] "kg"
      
      $files$populations$Demographics$rows[[2]]$heightMin
      [1] NA
      
      $files$populations$Demographics$rows[[2]]$heightMax
      [1] NA
      
      $files$populations$Demographics$rows[[2]]$heightUnit
      [1] "cm"
      
      $files$populations$Demographics$rows[[2]]$ageMin
      [1] "22"
      
      $files$populations$Demographics$rows[[2]]$ageMax
      [1] "41"
      
      $files$populations$Demographics$rows[[2]]$BMIMin
      [1] NA
      
      $files$populations$Demographics$rows[[2]]$BMIMax
      [1] NA
      
      $files$populations$Demographics$rows[[2]]$BMIUnit
      [1] "kg/m²"
      
      $files$populations$Demographics$rows[[2]]$Protein
      [1] NA
      
      $files$populations$Demographics$rows[[2]]$Ontogeny
      [1] NA
      
      
      
      
      $files$populations$UserDefinedVariability
      $files$populations$UserDefinedVariability$column_names
      [1] "Container Path" "Parameter Name" "Mean"           "SD"            
      [5] "Distribution"  
      
      $files$populations$UserDefinedVariability$rows
      list()
      
      
      
      $files$scenarios
      $files$scenarios$Scenarios
      $files$scenarios$Scenarios$column_names
       [1] "Scenario_name"         "IndividualId"          "PopulationId"         
       [4] "ReadPopulationFromCSV" "ModelParameterSheets"  "ApplicationProtocol"  
       [7] "SimulationTime"        "SimulationTimeUnit"    "SteadyState"          
      [10] "SteadyStateTime"       "SteadyStateTimeUnit"   "ModelFile"            
      [13] "OutputPathsIds"       
      
      $files$scenarios$Scenarios$rows
      $files$scenarios$Scenarios$rows[[1]]
      $files$scenarios$Scenarios$rows[[1]]$Scenario_name
      [1] "TestScenario"
      
      $files$scenarios$Scenarios$rows[[1]]$IndividualId
      [1] "Indiv1"
      
      $files$scenarios$Scenarios$rows[[1]]$PopulationId
      [1] NA
      
      $files$scenarios$Scenarios$rows[[1]]$ReadPopulationFromCSV
      [1] NA
      
      $files$scenarios$Scenarios$rows[[1]]$ModelParameterSheets
      [1] "Global"
      
      $files$scenarios$Scenarios$rows[[1]]$ApplicationProtocol
      [1] "Aciclovir_iv_250mg"
      
      $files$scenarios$Scenarios$rows[[1]]$SimulationTime
      [1] "0, 24, 60"
      
      $files$scenarios$Scenarios$rows[[1]]$SimulationTimeUnit
      [1] "h"
      
      $files$scenarios$Scenarios$rows[[1]]$SteadyState
      [1] NA
      
      $files$scenarios$Scenarios$rows[[1]]$SteadyStateTime
      [1] NA
      
      $files$scenarios$Scenarios$rows[[1]]$SteadyStateTimeUnit
      [1] NA
      
      $files$scenarios$Scenarios$rows[[1]]$ModelFile
      [1] "Aciclovir.pkml"
      
      $files$scenarios$Scenarios$rows[[1]]$OutputPathsIds
      [1] NA
      
      
      $files$scenarios$Scenarios$rows[[2]]
      $files$scenarios$Scenarios$rows[[2]]$Scenario_name
      [1] "TestScenario2"
      
      $files$scenarios$Scenarios$rows[[2]]$IndividualId
      [1] "Indiv1"
      
      $files$scenarios$Scenarios$rows[[2]]$PopulationId
      [1] NA
      
      $files$scenarios$Scenarios$rows[[2]]$ReadPopulationFromCSV
      [1] NA
      
      $files$scenarios$Scenarios$rows[[2]]$ModelParameterSheets
      [1] "Global"
      
      $files$scenarios$Scenarios$rows[[2]]$ApplicationProtocol
      [1] "Aciclovir_iv_250mg"
      
      $files$scenarios$Scenarios$rows[[2]]$SimulationTime
      [1] "0, 1, 60; 1, 12, 20"
      
      $files$scenarios$Scenarios$rows[[2]]$SimulationTimeUnit
      [1] "h"
      
      $files$scenarios$Scenarios$rows[[2]]$SteadyState
      [1] "TRUE"
      
      $files$scenarios$Scenarios$rows[[2]]$SteadyStateTime
      [1] "500"
      
      $files$scenarios$Scenarios$rows[[2]]$SteadyStateTimeUnit
      [1] "min"
      
      $files$scenarios$Scenarios$rows[[2]]$ModelFile
      [1] "Aciclovir.pkml"
      
      $files$scenarios$Scenarios$rows[[2]]$OutputPathsIds
      [1] "Aciclovir_PVB, Aciclovir_fat_cell"
      
      
      $files$scenarios$Scenarios$rows[[3]]
      $files$scenarios$Scenarios$rows[[3]]$Scenario_name
      [1] "PopulationScenario"
      
      $files$scenarios$Scenarios$rows[[3]]$IndividualId
      [1] "Indiv1"
      
      $files$scenarios$Scenarios$rows[[3]]$PopulationId
      [1] "TestPopulation"
      
      $files$scenarios$Scenarios$rows[[3]]$ReadPopulationFromCSV
      [1] "FALSE"
      
      $files$scenarios$Scenarios$rows[[3]]$ModelParameterSheets
      [1] "Global"
      
      $files$scenarios$Scenarios$rows[[3]]$ApplicationProtocol
      [1] "Aciclovir_iv_250mg"
      
      $files$scenarios$Scenarios$rows[[3]]$SimulationTime
      [1] "0, 12, 20"
      
      $files$scenarios$Scenarios$rows[[3]]$SimulationTimeUnit
      [1] "h"
      
      $files$scenarios$Scenarios$rows[[3]]$SteadyState
      [1] "FALSE"
      
      $files$scenarios$Scenarios$rows[[3]]$SteadyStateTime
      [1] NA
      
      $files$scenarios$Scenarios$rows[[3]]$SteadyStateTimeUnit
      [1] NA
      
      $files$scenarios$Scenarios$rows[[3]]$ModelFile
      [1] "Aciclovir.pkml"
      
      $files$scenarios$Scenarios$rows[[3]]$OutputPathsIds
      [1] NA
      
      
      $files$scenarios$Scenarios$rows[[4]]
      $files$scenarios$Scenarios$rows[[4]]$Scenario_name
      [1] "PopulationScenarioFromCSV"
      
      $files$scenarios$Scenarios$rows[[4]]$IndividualId
      [1] "Indiv1"
      
      $files$scenarios$Scenarios$rows[[4]]$PopulationId
      [1] "TestPopulation"
      
      $files$scenarios$Scenarios$rows[[4]]$ReadPopulationFromCSV
      [1] "TRUE"
      
      $files$scenarios$Scenarios$rows[[4]]$ModelParameterSheets
      [1] "Global"
      
      $files$scenarios$Scenarios$rows[[4]]$ApplicationProtocol
      [1] "Aciclovir_iv_250mg"
      
      $files$scenarios$Scenarios$rows[[4]]$SimulationTime
      [1] "0, 12, 20"
      
      $files$scenarios$Scenarios$rows[[4]]$SimulationTimeUnit
      [1] "h"
      
      $files$scenarios$Scenarios$rows[[4]]$SteadyState
      [1] "FALSE"
      
      $files$scenarios$Scenarios$rows[[4]]$SteadyStateTime
      [1] NA
      
      $files$scenarios$Scenarios$rows[[4]]$SteadyStateTimeUnit
      [1] NA
      
      $files$scenarios$Scenarios$rows[[4]]$ModelFile
      [1] "Aciclovir.pkml"
      
      $files$scenarios$Scenarios$rows[[4]]$OutputPathsIds
      [1] NA
      
      
      $files$scenarios$Scenarios$rows[[5]]
      $files$scenarios$Scenarios$rows[[5]]$Scenario_name
      [1] "TestScenario_missingParam"
      
      $files$scenarios$Scenarios$rows[[5]]$IndividualId
      [1] "Indiv1"
      
      $files$scenarios$Scenarios$rows[[5]]$PopulationId
      [1] NA
      
      $files$scenarios$Scenarios$rows[[5]]$ReadPopulationFromCSV
      [1] NA
      
      $files$scenarios$Scenarios$rows[[5]]$ModelParameterSheets
      [1] "Global, MissingParam"
      
      $files$scenarios$Scenarios$rows[[5]]$ApplicationProtocol
      [1] "Aciclovir_iv_250mg"
      
      $files$scenarios$Scenarios$rows[[5]]$SimulationTime
      [1] "0, 24, 60"
      
      $files$scenarios$Scenarios$rows[[5]]$SimulationTimeUnit
      [1] "h"
      
      $files$scenarios$Scenarios$rows[[5]]$SteadyState
      [1] NA
      
      $files$scenarios$Scenarios$rows[[5]]$SteadyStateTime
      [1] NA
      
      $files$scenarios$Scenarios$rows[[5]]$SteadyStateTimeUnit
      [1] NA
      
      $files$scenarios$Scenarios$rows[[5]]$ModelFile
      [1] "Aciclovir.pkml"
      
      $files$scenarios$Scenarios$rows[[5]]$OutputPathsIds
      [1] NA
      
      
      
      
      $files$scenarios$OutputPaths
      $files$scenarios$OutputPaths$column_names
      [1] "OutputPathId" "OutputPath"  
      
      $files$scenarios$OutputPaths$rows
      $files$scenarios$OutputPaths$rows[[1]]
      $files$scenarios$OutputPaths$rows[[1]]$OutputPathId
      [1] "Aciclovir_PVB"
      
      $files$scenarios$OutputPaths$rows[[1]]$OutputPath
      [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
      
      
      $files$scenarios$OutputPaths$rows[[2]]
      $files$scenarios$OutputPaths$rows[[2]]$OutputPathId
      [1] "Aciclovir_fat_cell"
      
      $files$scenarios$OutputPaths$rows[[2]]$OutputPath
      [1] "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
      
      
      
      
      
      $files$applications
      $files$applications$Aciclovir_iv_250mg
      $files$applications$Aciclovir_iv_250mg$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$applications$Aciclovir_iv_250mg$rows
      $files$applications$Aciclovir_iv_250mg$rows[[1]]
      $files$applications$Aciclovir_iv_250mg$rows[[1]]$`Container Path`
      [1] "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem"
      
      $files$applications$Aciclovir_iv_250mg$rows[[1]]$`Parameter Name`
      [1] "Dose"
      
      $files$applications$Aciclovir_iv_250mg$rows[[1]]$Value
      [1] "250"
      
      $files$applications$Aciclovir_iv_250mg$rows[[1]]$Units
      [1] "mg"
      
      
      
      
      
      $files$plots
      $files$plots$DataCombined
      $files$plots$DataCombined$column_names
       [1] "DataCombinedName" "dataType"         "label"            "scenario"        
       [5] "path"             "dataSet"          "group"            "xOffsets"        
       [9] "xOffsetsUnits"    "yOffsets"         "yOffsetsUnits"    "xScaleFactors"   
      [13] "yScaleFactors"   
      
      $files$plots$DataCombined$rows
      $files$plots$DataCombined$rows[[1]]
      $files$plots$DataCombined$rows[[1]]$DataCombinedName
      [1] "AciclovirPVB"
      
      $files$plots$DataCombined$rows[[1]]$dataType
      [1] "simulated"
      
      $files$plots$DataCombined$rows[[1]]$label
      [1] "Aciclovir simulated"
      
      $files$plots$DataCombined$rows[[1]]$scenario
      [1] "TestScenario"
      
      $files$plots$DataCombined$rows[[1]]$path
      [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
      
      $files$plots$DataCombined$rows[[1]]$dataSet
      [1] NA
      
      $files$plots$DataCombined$rows[[1]]$group
      [1] "Aciclovir PVB"
      
      $files$plots$DataCombined$rows[[1]]$xOffsets
      [1] "1"
      
      $files$plots$DataCombined$rows[[1]]$xOffsetsUnits
      [1] "h"
      
      $files$plots$DataCombined$rows[[1]]$yOffsets
      [1] NA
      
      $files$plots$DataCombined$rows[[1]]$yOffsetsUnits
      [1] NA
      
      $files$plots$DataCombined$rows[[1]]$xScaleFactors
      [1] NA
      
      $files$plots$DataCombined$rows[[1]]$yScaleFactors
      [1] NA
      
      
      $files$plots$DataCombined$rows[[2]]
      $files$plots$DataCombined$rows[[2]]$DataCombinedName
      [1] "AciclovirPVB"
      
      $files$plots$DataCombined$rows[[2]]$dataType
      [1] "observed"
      
      $files$plots$DataCombined$rows[[2]]$label
      [1] "Aciclovri observed"
      
      $files$plots$DataCombined$rows[[2]]$scenario
      [1] NA
      
      $files$plots$DataCombined$rows[[2]]$path
      [1] NA
      
      $files$plots$DataCombined$rows[[2]]$dataSet
      [1] "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      
      $files$plots$DataCombined$rows[[2]]$group
      [1] "Aciclovir PVB"
      
      $files$plots$DataCombined$rows[[2]]$xOffsets
      [1] "1"
      
      $files$plots$DataCombined$rows[[2]]$xOffsetsUnits
      [1] "min"
      
      $files$plots$DataCombined$rows[[2]]$yOffsets
      [1] NA
      
      $files$plots$DataCombined$rows[[2]]$yOffsetsUnits
      [1] NA
      
      $files$plots$DataCombined$rows[[2]]$xScaleFactors
      [1] NA
      
      $files$plots$DataCombined$rows[[2]]$yScaleFactors
      [1] NA
      
      
      $files$plots$DataCombined$rows[[3]]
      $files$plots$DataCombined$rows[[3]]$DataCombinedName
      [1] "AciclovirPop"
      
      $files$plots$DataCombined$rows[[3]]$dataType
      [1] "simulated"
      
      $files$plots$DataCombined$rows[[3]]$label
      [1] "Aciclovir simulated"
      
      $files$plots$DataCombined$rows[[3]]$scenario
      [1] "PopulationScenario"
      
      $files$plots$DataCombined$rows[[3]]$path
      [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
      
      $files$plots$DataCombined$rows[[3]]$dataSet
      [1] NA
      
      $files$plots$DataCombined$rows[[3]]$group
      [1] "AciclovirPop"
      
      $files$plots$DataCombined$rows[[3]]$xOffsets
      [1] "1"
      
      $files$plots$DataCombined$rows[[3]]$xOffsetsUnits
      [1] "h"
      
      $files$plots$DataCombined$rows[[3]]$yOffsets
      [1] NA
      
      $files$plots$DataCombined$rows[[3]]$yOffsetsUnits
      [1] NA
      
      $files$plots$DataCombined$rows[[3]]$xScaleFactors
      [1] NA
      
      $files$plots$DataCombined$rows[[3]]$yScaleFactors
      [1] NA
      
      
      $files$plots$DataCombined$rows[[4]]
      $files$plots$DataCombined$rows[[4]]$DataCombinedName
      [1] "AciclovirPop"
      
      $files$plots$DataCombined$rows[[4]]$dataType
      [1] "observed"
      
      $files$plots$DataCombined$rows[[4]]$label
      [1] "Aciclovri observed"
      
      $files$plots$DataCombined$rows[[4]]$scenario
      [1] NA
      
      $files$plots$DataCombined$rows[[4]]$path
      [1] NA
      
      $files$plots$DataCombined$rows[[4]]$dataSet
      [1] "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      
      $files$plots$DataCombined$rows[[4]]$group
      [1] "AciclovirPop"
      
      $files$plots$DataCombined$rows[[4]]$xOffsets
      [1] "1"
      
      $files$plots$DataCombined$rows[[4]]$xOffsetsUnits
      [1] "min"
      
      $files$plots$DataCombined$rows[[4]]$yOffsets
      [1] NA
      
      $files$plots$DataCombined$rows[[4]]$yOffsetsUnits
      [1] NA
      
      $files$plots$DataCombined$rows[[4]]$xScaleFactors
      [1] NA
      
      $files$plots$DataCombined$rows[[4]]$yScaleFactors
      [1] NA
      
      
      
      
      $files$plots$plotConfiguration
      $files$plots$plotConfiguration$column_names
       [1] "plotID"           "DataCombinedName" "plotType"         "title"           
       [5] "xUnit"            "yUnit"            "xAxisScale"       "yAxisScale"      
       [9] "xValuesLimits"    "yValuesLimits"    "aggregation"      "quantiles"       
      [13] "nsd"              "foldDistance"    
      
      $files$plots$plotConfiguration$rows
      $files$plots$plotConfiguration$rows[[1]]
      $files$plots$plotConfiguration$rows[[1]]$plotID
      [1] "P1"
      
      $files$plots$plotConfiguration$rows[[1]]$DataCombinedName
      [1] "AciclovirPVB"
      
      $files$plots$plotConfiguration$rows[[1]]$plotType
      [1] "individual"
      
      $files$plots$plotConfiguration$rows[[1]]$title
      [1] NA
      
      $files$plots$plotConfiguration$rows[[1]]$xUnit
      [1] "h"
      
      $files$plots$plotConfiguration$rows[[1]]$yUnit
      [1] NA
      
      $files$plots$plotConfiguration$rows[[1]]$xAxisScale
      [1] NA
      
      $files$plots$plotConfiguration$rows[[1]]$yAxisScale
      [1] NA
      
      $files$plots$plotConfiguration$rows[[1]]$xValuesLimits
      [1] "0, 24"
      
      $files$plots$plotConfiguration$rows[[1]]$yValuesLimits
      [1] NA
      
      $files$plots$plotConfiguration$rows[[1]]$aggregation
      [1] NA
      
      $files$plots$plotConfiguration$rows[[1]]$quantiles
      [1] NA
      
      $files$plots$plotConfiguration$rows[[1]]$nsd
      [1] NA
      
      $files$plots$plotConfiguration$rows[[1]]$foldDistance
      [1] NA
      
      
      $files$plots$plotConfiguration$rows[[2]]
      $files$plots$plotConfiguration$rows[[2]]$plotID
      [1] "P2"
      
      $files$plots$plotConfiguration$rows[[2]]$DataCombinedName
      [1] "AciclovirPVB"
      
      $files$plots$plotConfiguration$rows[[2]]$plotType
      [1] "observedVsSimulated"
      
      $files$plots$plotConfiguration$rows[[2]]$title
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$xUnit
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$yUnit
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$xAxisScale
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$yAxisScale
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$xValuesLimits
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$yValuesLimits
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$aggregation
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$quantiles
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$nsd
      [1] NA
      
      $files$plots$plotConfiguration$rows[[2]]$foldDistance
      [1] "2, 3"
      
      
      $files$plots$plotConfiguration$rows[[3]]
      $files$plots$plotConfiguration$rows[[3]]$plotID
      [1] "P3"
      
      $files$plots$plotConfiguration$rows[[3]]$DataCombinedName
      [1] "AciclovirPVB"
      
      $files$plots$plotConfiguration$rows[[3]]$plotType
      [1] "residualsVsSimulated"
      
      $files$plots$plotConfiguration$rows[[3]]$title
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$xUnit
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$yUnit
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$xAxisScale
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$yAxisScale
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$xValuesLimits
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$yValuesLimits
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$aggregation
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$quantiles
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$nsd
      [1] NA
      
      $files$plots$plotConfiguration$rows[[3]]$foldDistance
      [1] NA
      
      
      $files$plots$plotConfiguration$rows[[4]]
      $files$plots$plotConfiguration$rows[[4]]$plotID
      [1] "P4"
      
      $files$plots$plotConfiguration$rows[[4]]$DataCombinedName
      [1] "AciclovirPop"
      
      $files$plots$plotConfiguration$rows[[4]]$plotType
      [1] "population"
      
      $files$plots$plotConfiguration$rows[[4]]$title
      [1] NA
      
      $files$plots$plotConfiguration$rows[[4]]$xUnit
      [1] "h"
      
      $files$plots$plotConfiguration$rows[[4]]$yUnit
      [1] NA
      
      $files$plots$plotConfiguration$rows[[4]]$xAxisScale
      [1] NA
      
      $files$plots$plotConfiguration$rows[[4]]$yAxisScale
      [1] NA
      
      $files$plots$plotConfiguration$rows[[4]]$xValuesLimits
      [1] "0, 24"
      
      $files$plots$plotConfiguration$rows[[4]]$yValuesLimits
      [1] NA
      
      $files$plots$plotConfiguration$rows[[4]]$aggregation
      [1] "arithmetic"
      
      $files$plots$plotConfiguration$rows[[4]]$quantiles
      [1] NA
      
      $files$plots$plotConfiguration$rows[[4]]$nsd
      [1] "1.96"
      
      $files$plots$plotConfiguration$rows[[4]]$foldDistance
      [1] NA
      
      
      
      
      $files$plots$plotGrids
      $files$plots$plotGrids$column_names
      [1] "name"    "plotIDs" "title"  
      
      $files$plots$plotGrids$rows
      $files$plots$plotGrids$rows[[1]]
      $files$plots$plotGrids$rows[[1]]$name
      [1] "Aciclovir"
      
      $files$plots$plotGrids$rows[[1]]$plotIDs
      [1] "P1, P2, P3"
      
      $files$plots$plotGrids$rows[[1]]$title
      [1] NA
      
      
      $files$plots$plotGrids$rows[[2]]
      $files$plots$plotGrids$rows[[2]]$name
      [1] "Aciclovir2"
      
      $files$plots$plotGrids$rows[[2]]$plotIDs
      [1] "P2"
      
      $files$plots$plotGrids$rows[[2]]$title
      [1] NA
      
      
      $files$plots$plotGrids$rows[[3]]
      $files$plots$plotGrids$rows[[3]]$name
      [1] "Aciclovir3"
      
      $files$plots$plotGrids$rows[[3]]$plotIDs
      [1] "P4"
      
      $files$plots$plotGrids$rows[[3]]$title
      [1] NA
      
      
      
      
      $files$plots$exportConfiguration
      $files$plots$exportConfiguration$column_names
      [1] "plotGridName" "outputName"   "width"       
      
      $files$plots$exportConfiguration$rows
      list()
      
      
      $files$plots$dataTypes
      $files$plots$dataTypes$column_names
      [1] "dataType"
      
      $files$plots$dataTypes$rows
      $files$plots$dataTypes$rows[[1]]
      $files$plots$dataTypes$rows[[1]]$dataType
      [1] "observed"
      
      
      $files$plots$dataTypes$rows[[2]]
      $files$plots$dataTypes$rows[[2]]$dataType
      [1] "simulated"
      
      
      
      
      $files$plots$plotTypes
      $files$plots$plotTypes$column_names
      [1] "plotType"
      
      $files$plots$plotTypes$rows
      $files$plots$plotTypes$rows[[1]]
      $files$plots$plotTypes$rows[[1]]$plotType
      [1] "individual"
      
      
      $files$plots$plotTypes$rows[[2]]
      $files$plots$plotTypes$rows[[2]]$plotType
      [1] "population"
      
      
      $files$plots$plotTypes$rows[[3]]
      $files$plots$plotTypes$rows[[3]]$plotType
      [1] "observedVsSimulated"
      
      
      $files$plots$plotTypes$rows[[4]]
      $files$plots$plotTypes$rows[[4]]$plotType
      [1] "residualsVsSimulated"
      
      
      $files$plots$plotTypes$rows[[5]]
      $files$plots$plotTypes$rows[[5]]$plotType
      [1] "residualsVsTime"
      
      
      
      
      $files$plots$ObservedDataNames
      $files$plots$ObservedDataNames$column_names
      character(0)
      
      $files$plots$ObservedDataNames$rows
      list()
      
      
      
      $files$populationsCSV
      $files$populationsCSV$TestPopulation.csv
      $files$populationsCSV$TestPopulation.csv$column_names
        [1] "IndividualId"                                                             
        [2] "Gender"                                                                   
        [3] "Population"                                                               
        [4] "Organism.Weight"                                                          
        [5] "Organism.BMI"                                                             
        [6] "Organism.BSA"                                                             
        [7] "Organism.Age"                                                             
        [8] "Organism.Gestational.age"                                                 
        [9] "Organism.Height"                                                          
       [10] "Organism.Ontogeny.factor..albumin."                                       
       [11] "Organism.Ontogeny.factor..alpha1.acid.glycoprotein."                      
       [12] "Organism.Hematocrit"                                                      
       [13] "Organism.VenousBlood.Volume"                                              
       [14] "Organism.ArterialBlood.Volume"                                            
       [15] "Organism.Bone.Specific.blood.flow.rate"                                   
       [16] "Organism.Bone.Volume"                                                     
       [17] "Organism.Brain.Volume"                                                    
       [18] "Organism.Brain.Specific.blood.flow.rate"                                  
       [19] "Organism.Fat.Volume"                                                      
       [20] "Organism.Fat.Vf..lipid."                                                  
       [21] "Organism.Fat.Vf..neutral.lipid..PT"                                       
       [22] "Organism.Fat.Vf..phospholipid..PT"                                        
       [23] "Organism.Fat.Fraction.interstitial"                                       
       [24] "Organism.Fat.Vf..water."                                                  
       [25] "Organism.Fat.Vf..water..PT"                                               
       [26] "Organism.Fat.Vf..neutral.lipid..RR"                                       
       [27] "Organism.Fat.Vf..neutral.lipid..WS"                                       
       [28] "Organism.Fat.Vf..neutral.phospholipid..RR"                                
       [29] "Organism.Fat.Vf..neutral.phospholipid..plasma..WS"                        
       [30] "Organism.Fat.Vf..extracellular.water..RR"                                 
       [31] "Organism.Fat.Vf..intracellular.water..RR"                                 
       [32] "Organism.Fat.Vf..water..WS"                                               
       [33] "Organism.Fat.Specific.blood.flow.rate"                                    
       [34] "Organism.Gonads.Volume"                                                   
       [35] "Organism.Gonads.Specific.blood.flow.rate"                                 
       [36] "Organism.Heart.Volume"                                                    
       [37] "Organism.Heart.Specific.blood.flow.rate"                                  
       [38] "Organism.Kidney.Volume"                                                   
       [39] "Organism.Kidney.Age.of.aging.onset"                                       
       [40] "Organism.Kidney.Aging.half.time"                                          
       [41] "Organism.Kidney.GFRmat"                                                   
       [42] "Organism.Kidney.Hill.coefficient.for.aging.GFR"                           
       [43] "Organism.Kidney.Hill.coefficient.for.GFR"                                 
       [44] "Organism.Kidney.Maximal.decreasing.rate.factor"                           
       [45] "Organism.Kidney.TM50.for.GFR"                                             
       [46] "Organism.Kidney.Specific.blood.flow.rate"                                 
       [47] "Organism.Lumen.Effective.surface.area.variability.factor"                 
       [48] "Organism.Lumen.Stomach.Distal.radius"                                     
       [49] "Organism.Lumen.Stomach.Gastric.emptying.time"                             
       [50] "Organism.Lumen.Stomach.GET_alpha..Weibull.function..variability.factor"   
       [51] "Organism.Lumen.Stomach.GET_beta..Weibull.function..variability.factor"    
       [52] "Organism.Lumen.Stomach.Length"                                            
       [53] "Organism.Lumen.Stomach.Proximal.radius"                                   
       [54] "Organism.Lumen.Duodenum.Effective.surface.area.enhancement.factor"        
       [55] "Organism.Lumen.UpperJejunum.Effective.surface.area.enhancement.factor"    
       [56] "Organism.Lumen.LowerJejunum.Effective.surface.area.enhancement.factor"    
       [57] "Organism.Lumen.UpperIleum.Effective.surface.area.enhancement.factor"      
       [58] "Organism.Lumen.LowerIleum.Effective.surface.area.enhancement.factor"      
       [59] "Organism.Lumen.Caecum.Effective.surface.area.enhancement.factor"          
       [60] "Organism.Lumen.ColonAscendens.Effective.surface.area.enhancement.factor"  
       [61] "Organism.Lumen.ColonTransversum.Effective.surface.area.enhancement.factor"
       [62] "Organism.Lumen.ColonDescendens.Effective.surface.area.enhancement.factor" 
       [63] "Organism.Lumen.ColonSigmoid.Effective.surface.area.enhancement.factor"    
       [64] "Organism.Lumen.Rectum.Effective.surface.area.enhancement.factor"          
       [65] "Organism.Stomach.Volume"                                                  
       [66] "Organism.Stomach.Specific.blood.flow.rate"                                
       [67] "Organism.SmallIntestine.Small.intestinal.transit.time"                    
       [68] "Organism.SmallIntestine.Volume"                                           
       [69] "Organism.SmallIntestine.Specific.blood.flow.rate"                         
       [70] "Organism.LargeIntestine.Large.intestinal.transit.time"                    
       [71] "Organism.LargeIntestine.Volume"                                           
       [72] "Organism.LargeIntestine.Specific.blood.flow.rate"                         
       [73] "Organism.Liver.Volume"                                                    
       [74] "Organism.Liver.Specific.blood.flow.rate"                                  
       [75] "Organism.Lung.Volume"                                                     
       [76] "Organism.Lung.Fraction.vascular"                                          
       [77] "Organism.Muscle.Volume"                                                   
       [78] "Organism.Muscle.Vf..lipid."                                               
       [79] "Organism.Muscle.Vf..neutral.lipid..PT"                                    
       [80] "Organism.Muscle.Vf..phospholipid..PT"                                     
       [81] "Organism.Muscle.Vf..protein."                                             
       [82] "Organism.Muscle.Vf..water."                                               
       [83] "Organism.Muscle.Vf..water..PT"                                            
       [84] "Organism.Muscle.Fraction.interstitial"                                    
       [85] "Organism.Muscle.Vf..neutral.lipid..RR"                                    
       [86] "Organism.Muscle.Vf..neutral.lipid..WS"                                    
       [87] "Organism.Muscle.Vf..neutral.phospholipid..RR"                             
       [88] "Organism.Muscle.Vf..neutral.phospholipid..plasma..WS"                     
       [89] "Organism.Muscle.Vf..extracellular.water..RR"                              
       [90] "Organism.Muscle.Vf..protein..WS"                                          
       [91] "Organism.Muscle.Vf..intracellular.water..RR"                              
       [92] "Organism.Muscle.Vf..water..WS"                                            
       [93] "Organism.Muscle.Specific.blood.flow.rate"                                 
       [94] "Organism.Pancreas.Volume"                                                 
       [95] "Organism.Pancreas.Specific.blood.flow.rate"                               
       [96] "Organism.PortalVein.Volume"                                               
       [97] "Organism.Skin.Volume"                                                     
       [98] "Organism.Skin.Specific.blood.flow.rate"                                   
       [99] "Organism.Spleen.Volume"                                                   
      [100] "Organism.Spleen.Specific.blood.flow.rate"                                 
      
      $files$populationsCSV$TestPopulation.csv$rows
      $files$populationsCSV$TestPopulation.csv$rows[[1]]
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$IndividualId
      [1] "0"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Gender
      [1] "MALE"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Population
      [1] "European_ICRP_2002"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Weight
      [1] "61.5347980234939"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.BMI
      [1] "0.22110435036443"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.BSA
      [1] "168.868520364698"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Age
      [1] "40.2758611595611"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Gestational.age
      [1] "40"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Height
      [1] "16.6825235226199"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Ontogeny.factor..albumin.
      [1] "0.899636802882332"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Ontogeny.factor..alpha1.acid.glycoprotein.
      [1] "1.15949978271202"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Hematocrit
      [1] "0.47"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.VenousBlood.Volume
      [1] "0.90091056303014"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.ArterialBlood.Volume
      [1] "0.406135603100194"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Bone.Specific.blood.flow.rate
      [1] "0.0315090234960704"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Bone.Volume
      [1] "10.8690045999271"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Brain.Volume
      [1] "1.57168243846914"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Brain.Specific.blood.flow.rate
      [1] "0.489495988836155"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Volume
      [1] "7.52250463972452"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..lipid.
      [1] "0.8"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..neutral.lipid..PT
      [1] "0.79"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..phospholipid..PT
      [1] "0.002"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Fraction.interstitial
      [1] "0.16"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..water.
      [1] "0.15"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..water..PT
      [1] "0.18"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..neutral.lipid..RR
      [1] "0.853"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..neutral.lipid..WS
      [1] "0.92"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..neutral.phospholipid..RR
      [1] "0.0016"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..neutral.phospholipid..plasma..WS
      [1] "0.002024"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..extracellular.water..RR
      [1] "0.135"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..intracellular.water..RR
      [1] "0.00899999999999998"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Vf..water..WS
      [1] "0.03"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Fat.Specific.blood.flow.rate
      [1] "0.0206732323263958"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Gonads.Volume
      [1] "0.0363901626133667"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Gonads.Specific.blood.flow.rate
      [1] "0.0806178855218515"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Heart.Volume
      [1] "0.427747241993127"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Heart.Specific.blood.flow.rate
      [1] "0.682071775659985"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.Volume
      [1] "0.49070666449687"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.Age.of.aging.onset
      [1] "30"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.Aging.half.time
      [1] "54"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.GFRmat
      [1] "0.0702927779334989"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.Hill.coefficient.for.aging.GFR
      [1] "1.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.Hill.coefficient.for.GFR
      [1] "15.3024731123431"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.Maximal.decreasing.rate.factor
      [1] "0.9"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.TM50.for.GFR
      [1] "45.2037451452012"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Kidney.Specific.blood.flow.rate
      [1] "2.8377452010901"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Effective.surface.area.variability.factor
      [1] "1.77008432606349"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Stomach.Distal.radius
      [1] "0.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Stomach.Gastric.emptying.time
      [1] "5.96819895017132"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Stomach.GET_alpha..Weibull.function..variability.factor
      [1] "3.2105442703701"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Stomach.GET_beta..Weibull.function..variability.factor
      [1] "1.1006818299719"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Stomach.Length
      [1] "2"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Stomach.Proximal.radius
      [1] "0.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Duodenum.Effective.surface.area.enhancement.factor
      [1] "292.6883"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.UpperJejunum.Effective.surface.area.enhancement.factor
      [1] "447.9877"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.LowerJejunum.Effective.surface.area.enhancement.factor
      [1] "372.9358"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.UpperIleum.Effective.surface.area.enhancement.factor
      [1] "260.7527"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.LowerIleum.Effective.surface.area.enhancement.factor
      [1] "146.565"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Caecum.Effective.surface.area.enhancement.factor
      [1] "1.8"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.ColonAscendens.Effective.surface.area.enhancement.factor
      [1] "2.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.ColonTransversum.Effective.surface.area.enhancement.factor
      [1] "2.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.ColonDescendens.Effective.surface.area.enhancement.factor
      [1] "2.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.ColonSigmoid.Effective.surface.area.enhancement.factor
      [1] "2.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lumen.Rectum.Effective.surface.area.enhancement.factor
      [1] "3.56"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Stomach.Volume
      [1] "0.194438572408107"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Stomach.Specific.blood.flow.rate
      [1] "0.448201473480319"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.SmallIntestine.Small.intestinal.transit.time
      [1] "107.630542050005"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.SmallIntestine.Volume
      [1] "0.592413461580726"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.SmallIntestine.Specific.blood.flow.rate
      [1] "0.92329394054034"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.LargeIntestine.Large.intestinal.transit.time
      [1] "2652"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.LargeIntestine.Volume
      [1] "0.463264373078633"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.LargeIntestine.Specific.blood.flow.rate
      [1] "0.635781142717255"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Liver.Volume
      [1] "2.63322588671254"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Liver.Specific.blood.flow.rate
      [1] "0.175550792244102"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lung.Volume
      [1] "1.17197321761479"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Lung.Fraction.vascular
      [1] "0.58"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Volume
      [1] "29.3017789696476"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..lipid.
      [1] "0.013"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..neutral.lipid..PT
      [1] "0.0238"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..phospholipid..PT
      [1] "0.0072"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..protein.
      [1] "0.177"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..water.
      [1] "0.811"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..water..PT
      [1] "0.76"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Fraction.interstitial
      [1] "0.16"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..neutral.lipid..RR
      [1] "0.022"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..neutral.lipid..WS
      [1] "0.0049"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..neutral.phospholipid..RR
      [1] "0.0078"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..neutral.phospholipid..plasma..WS
      [1] "0.0042"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..extracellular.water..RR
      [1] "0.079"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..protein..WS
      [1] "0.19"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..intracellular.water..RR
      [1] "0.666"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Vf..water..WS
      [1] "0.76"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Muscle.Specific.blood.flow.rate
      [1] "0.036611489119001"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Pancreas.Volume
      [1] "0.27516244712599"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Pancreas.Specific.blood.flow.rate
      [1] "0.372521833876031"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.PortalVein.Volume
      [1] "0.942640954550504"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Skin.Volume
      [1] "3.49102282995465"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Skin.Specific.blood.flow.rate
      [1] "0.0845837873081867"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Spleen.Volume
      [1] "0.243795397465916"
      
      $files$populationsCSV$TestPopulation.csv$rows[[1]]$Organism.Spleen.Specific.blood.flow.rate
      [1] "0.909827131937137"
      
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$IndividualId
      [1] "1"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Gender
      [1] "MALE"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Population
      [1] "European_ICRP_2002"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Weight
      [1] "62.9655208804219"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.BMI
      [1] "0.196540160342841"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.BSA
      [1] "176.938242514102"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Age
      [1] "23.9117595571614"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Gestational.age
      [1] "40"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Height
      [1] "17.8988754990528"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Ontogeny.factor..albumin.
      [1] "0.8530616107376"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Ontogeny.factor..alpha1.acid.glycoprotein.
      [1] "0.699689929706047"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Hematocrit
      [1] "0.449705865190538"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.VenousBlood.Volume
      [1] "0.92167719798216"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.ArterialBlood.Volume
      [1] "0.424738350141017"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Bone.Specific.blood.flow.rate
      [1] "0.0319717624193636"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Bone.Volume
      [1] "11.4983895299467"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Brain.Volume
      [1] "1.47702462355843"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Brain.Specific.blood.flow.rate
      [1] "0.510232110737281"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Volume
      [1] "8.19841733381987"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..lipid.
      [1] "0.771588211266753"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..neutral.lipid..PT
      [1] "0.761943358625919"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..phospholipid..PT
      [1] "0.00192897052816688"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Fraction.interstitial
      [1] "0.184352961771355"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..water.
      [1] "0.178411788733247"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..water..PT
      [1] "0.214094146479896"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..neutral.lipid..RR
      [1] "0.822705930263175"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..neutral.lipid..WS
      [1] "0.887326442956766"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..neutral.phospholipid..RR
      [1] "0.00154317642253351"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..neutral.phospholipid..plasma..WS
      [1] "0.00195211817450489"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..extracellular.water..RR
      [1] "0.160570609859922"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..intracellular.water..RR
      [1] "0.0107047073239948"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Vf..water..WS
      [1] "0.0356823577466494"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Fat.Specific.blood.flow.rate
      [1] "0.0257942437085706"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Gonads.Volume
      [1] "0.0302628508592311"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Gonads.Specific.blood.flow.rate
      [1] "0.107258474391816"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Heart.Volume
      [1] "0.198169802641703"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Heart.Specific.blood.flow.rate
      [1] "0.710677787703658"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.Volume
      [1] "0.493479572294716"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.Age.of.aging.onset
      [1] "30"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.Aging.half.time
      [1] "54"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.GFRmat
      [1] "0.107102771082758"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.Hill.coefficient.for.aging.GFR
      [1] "1.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.Hill.coefficient.for.GFR
      [1] "15.6612677239469"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.Maximal.decreasing.rate.factor
      [1] "0.9"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.TM50.for.GFR
      [1] "44.0003667679178"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Kidney.Specific.blood.flow.rate
      [1] "3.38592193173909"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Effective.surface.area.variability.factor
      [1] "0.867556397888081"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Stomach.Distal.radius
      [1] "0.489621579458441"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Stomach.Gastric.emptying.time
      [1] "13.9818897126764"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Stomach.GET_alpha..Weibull.function..variability.factor
      [1] "0.977244398387356"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Stomach.GET_beta..Weibull.function..variability.factor
      [1] "1.04051643753397"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Stomach.Length
      [1] "1.95847820017984"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Stomach.Proximal.radius
      [1] "0.489621579458441"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Duodenum.Effective.surface.area.enhancement.factor
      [1] "292.6883"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.UpperJejunum.Effective.surface.area.enhancement.factor
      [1] "447.9877"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.LowerJejunum.Effective.surface.area.enhancement.factor
      [1] "372.9358"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.UpperIleum.Effective.surface.area.enhancement.factor
      [1] "260.7527"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.LowerIleum.Effective.surface.area.enhancement.factor
      [1] "146.565"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Caecum.Effective.surface.area.enhancement.factor
      [1] "1.8"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.ColonAscendens.Effective.surface.area.enhancement.factor
      [1] "2.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.ColonTransversum.Effective.surface.area.enhancement.factor
      [1] "2.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.ColonDescendens.Effective.surface.area.enhancement.factor
      [1] "2.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.ColonSigmoid.Effective.surface.area.enhancement.factor
      [1] "2.5"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lumen.Rectum.Effective.surface.area.enhancement.factor
      [1] "3.56"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Stomach.Volume
      [1] "0.187016027845032"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Stomach.Specific.blood.flow.rate
      [1] "0.397796555465622"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.SmallIntestine.Small.intestinal.transit.time
      [1] "79.8673742545636"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.SmallIntestine.Volume
      [1] "0.64221475348445"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.SmallIntestine.Specific.blood.flow.rate
      [1] "0.893277355348927"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.LargeIntestine.Large.intestinal.transit.time
      [1] "2592.33524366018"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.LargeIntestine.Volume
      [1] "0.420636829269417"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.LargeIntestine.Specific.blood.flow.rate
      [1] "0.691223816270522"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Liver.Volume
      [1] "2.3273205469601"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Liver.Specific.blood.flow.rate
      [1] "0.211653907325067"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lung.Volume
      [1] "1.2070302874223"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Lung.Fraction.vascular
      [1] "0.58"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Volume
      [1] "30.4937318900507"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..lipid.
      [1] "0.013"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..neutral.lipid..PT
      [1] "0.0238"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..phospholipid..PT
      [1] "0.0072"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..protein.
      [1] "0.177"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..water.
      [1] "0.811"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..water..PT
      [1] "0.76"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Fraction.interstitial
      [1] "0.168117653923785"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..neutral.lipid..RR
      [1] "0.022"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..neutral.lipid..WS
      [1] "0.0049"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..neutral.phospholipid..RR
      [1] "0.0078"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..neutral.phospholipid..plasma..WS
      [1] "0.0042"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..extracellular.water..RR
      [1] "0.079"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..protein..WS
      [1] "0.19"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..intracellular.water..RR
      [1] "0.666"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Vf..water..WS
      [1] "0.76"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Muscle.Specific.blood.flow.rate
      [1] "0.0338854472547501"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Pancreas.Volume
      [1] "0.240890815218283"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Pancreas.Specific.blood.flow.rate
      [1] "0.391209979824467"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.PortalVein.Volume
      [1] "1.03448435940802"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Skin.Volume
      [1] "3.08838802634855"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Skin.Specific.blood.flow.rate
      [1] "0.102410398588552"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Spleen.Volume
      [1] "0.0816480831712129"
      
      $files$populationsCSV$TestPopulation.csv$rows[[2]]$Organism.Spleen.Specific.blood.flow.rate
      [1] "0.866536264266006"
      
      
      
      
      
      

---

    Code
      json_content
    Output
      $files
      $files$projectConfiguration
      $files$projectConfiguration$ProjectConfiguration
      $files$projectConfiguration$ProjectConfiguration$column_names
      [1] "Property"    "Value"       "Description"
      
      $files$projectConfiguration$ProjectConfiguration$rows
                              Property                                  Value
      1                    modelFolder                    Models/Simulations/
      2           configurationsFolder                        Configurations/
      3                modelParamsFile                   ModelParameters.xlsx
      4                individualsFile                       Individuals.xlsx
      5                populationsFile                       Populations.xlsx
      6              populationsFolder                         PopulationsCSV
      7                  scenariosFile                         Scenarios.xlsx
      8               applicationsFile                      Applications.xlsx
      9                      plotsFile                             Plots.xlsx
      10                    dataFolder                                  Data/
      11                      dataFile        TestProject_TimeValuesData.xlsx
      12 dataImporterConfigurationFile esqlabs_dataImporter_configuration.xml
      13                  outputFolder                               Results/
                                                                                                                                Description
      1                                                Path to the folder with pkml simulation files; relative to the location of this file
      2                                     Path to the folder with excel files with parametrization; relative to the location of this file
      3                                     Name of the excel file with global model parametrization. Must be located in the "paramsFolder"
      4                        Name of the excel file with individual-specific model parametrization. Must be located in the "paramsFolder"
      5                                           Name of the excel file with population information. Must be located in the "paramsFolder"
      6                                                                           Name of the folder containing population defined in files
      7                                             Name of the excel file with scenario definitions. Must be located in the "paramsFolder"
      8  Name of the excel file scenario-specific parameters such as application protocol parameters. Must be located in the "paramsFolder"
      9                                                 Name of the excel file with plot definitions. Must be located in the "paramsFolder"
      10                                Path to the folder where experimental data files are located; relative to the location of this file
      11                                                 Name of the excel file with experimental data. Must be located in the "dataFolder"
      12                  Name of data importer configuration file in xml format used to load the data. Must be located in the "dataFolder"
      13                                     Path to the folder where the results should be saved to; relative to the location of this file
      
      
      
      $files$modelParameters
      $files$modelParameters$Global
      $files$modelParameters$Global$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$modelParameters$Global$rows
        Container Path          Parameter Name Value Units
      1 Organism|Liver EHC continuous fraction     1    NA
      
      
      $files$modelParameters$MissingParam
      $files$modelParameters$MissingParam$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$modelParameters$MissingParam$rows
        Container Path Parameter Name Value Units
      1            foo            bar     2    NA
      
      
      $files$modelParameters$Aciclovir
      $files$modelParameters$Aciclovir$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$modelParameters$Aciclovir$rows
        Container Path Parameter Name Value     Units
      1      Aciclovir  Lipophilicity  -0.1 Log Units
      
      
      
      $files$individuals
      $files$individuals$IndividualBiometrics
      $files$individuals$IndividualBiometrics$column_names
      [1] "IndividualId"  "Species"       "Population"    "Gender"       
      [5] "Weight [kg]"   "Height [cm]"   "Age [year(s)]" "Protein"      
      [9] "Ontogeny"     
      
      $files$individuals$IndividualBiometrics$rows
        IndividualId Species         Population Gender Weight [kg] Height [cm]
      1       Indiv1   Human European_ICRP_2002   MALE          73         176
        Age [year(s)] Protein Ontogeny
      1            30      NA       NA
      
      
      $files$individuals$Indiv1
      $files$individuals$Indiv1$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$individuals$Indiv1$rows
         Container Path Parameter Name Value  Units
      1 Organism|Kidney            GFR    90 ml/min
      
      
      
      $files$populations
      $files$populations$Demographics
      $files$populations$Demographics$column_names
       [1] "PopulationName"      "species"             "population"         
       [4] "numberOfIndividuals" "proportionOfFemales" "weightMin"          
       [7] "weightMax"           "weightUnit"          "heightMin"          
      [10] "heightMax"           "heightUnit"          "ageMin"             
      [13] "ageMax"              "BMIMin"              "BMIMax"             
      [16] "BMIUnit"             "Protein"             "Ontogeny"           
      
      $files$populations$Demographics$rows
               PopulationName species         population numberOfIndividuals
      1        TestPopulation   Human European_ICRP_2002                   2
      2 TestPopulation_noOnto   Human European_ICRP_2002                   2
        proportionOfFemales weightMin weightMax weightUnit heightMin heightMax
      1                   0        NA        NA         kg        NA        NA
      2                   0        NA        NA         kg        NA        NA
        heightUnit ageMin ageMax BMIMin BMIMax BMIUnit        Protein       Ontogeny
      1         cm     22     41     NA     NA   kg/m² CYP3A4, CYP2D6 CYP3A4, CYP2D6
      2         cm     22     41     NA     NA   kg/m²           <NA>           <NA>
      
      
      $files$populations$UserDefinedVariability
      $files$populations$UserDefinedVariability$column_names
      [1] "Container Path" "Parameter Name" "Mean"           "SD"            
      [5] "Distribution"  
      
      $files$populations$UserDefinedVariability$rows
      list()
      
      
      
      $files$scenarios
      $files$scenarios$Scenarios
      $files$scenarios$Scenarios$column_names
       [1] "Scenario_name"         "IndividualId"          "PopulationId"         
       [4] "ReadPopulationFromCSV" "ModelParameterSheets"  "ApplicationProtocol"  
       [7] "SimulationTime"        "SimulationTimeUnit"    "SteadyState"          
      [10] "SteadyStateTime"       "SteadyStateTimeUnit"   "ModelFile"            
      [13] "OutputPathsIds"       
      
      $files$scenarios$Scenarios$rows
                    Scenario_name IndividualId   PopulationId ReadPopulationFromCSV
      1              TestScenario       Indiv1           <NA>                  <NA>
      2             TestScenario2       Indiv1           <NA>                  <NA>
      3        PopulationScenario       Indiv1 TestPopulation                 FALSE
      4 PopulationScenarioFromCSV       Indiv1 TestPopulation                  TRUE
      5 TestScenario_missingParam       Indiv1           <NA>                  <NA>
        ModelParameterSheets ApplicationProtocol      SimulationTime
      1               Global  Aciclovir_iv_250mg           0, 24, 60
      2               Global  Aciclovir_iv_250mg 0, 1, 60; 1, 12, 20
      3               Global  Aciclovir_iv_250mg           0, 12, 20
      4               Global  Aciclovir_iv_250mg           0, 12, 20
      5 Global, MissingParam  Aciclovir_iv_250mg           0, 24, 60
        SimulationTimeUnit SteadyState SteadyStateTime SteadyStateTimeUnit
      1                  h        <NA>            <NA>                <NA>
      2                  h        TRUE             500                 min
      3                  h       FALSE            <NA>                <NA>
      4                  h       FALSE            <NA>                <NA>
      5                  h        <NA>            <NA>                <NA>
             ModelFile                    OutputPathsIds
      1 Aciclovir.pkml                              <NA>
      2 Aciclovir.pkml Aciclovir_PVB, Aciclovir_fat_cell
      3 Aciclovir.pkml                              <NA>
      4 Aciclovir.pkml                              <NA>
      5 Aciclovir.pkml                              <NA>
      
      
      $files$scenarios$OutputPaths
      $files$scenarios$OutputPaths$column_names
      [1] "OutputPathId" "OutputPath"  
      
      $files$scenarios$OutputPaths$rows
              OutputPathId
      1      Aciclovir_PVB
      2 Aciclovir_fat_cell
                                                                       OutputPath
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      2           Organism|Fat|Intracellular|Aciclovir|Concentration in container
      
      
      
      $files$applications
      $files$applications$Aciclovir_iv_250mg
      $files$applications$Aciclovir_iv_250mg$column_names
      [1] "Container Path" "Parameter Name" "Value"          "Units"         
      
      $files$applications$Aciclovir_iv_250mg$rows
                                                      Container Path Parameter Name
      1 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem           Dose
        Value Units
      1   250    mg
      
      
      
      $files$plots
      $files$plots$DataCombined
      $files$plots$DataCombined$column_names
       [1] "DataCombinedName" "dataType"         "label"            "scenario"        
       [5] "path"             "dataSet"          "group"            "xOffsets"        
       [9] "xOffsetsUnits"    "yOffsets"         "yOffsetsUnits"    "xScaleFactors"   
      [13] "yScaleFactors"   
      
      $files$plots$DataCombined$rows
        DataCombinedName  dataType               label           scenario
      1     AciclovirPVB simulated Aciclovir simulated       TestScenario
      2     AciclovirPVB  observed  Aciclovri observed               <NA>
      3     AciclovirPop simulated Aciclovir simulated PopulationScenario
      4     AciclovirPop  observed  Aciclovri observed               <NA>
                                                                             path
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      2                                                                      <NA>
      3 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      4                                                                      <NA>
                                                                                      dataSet
      1                                                                                  <NA>
      2 Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_
      3                                                                                  <NA>
      4 Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_
                group xOffsets xOffsetsUnits yOffsets yOffsetsUnits xScaleFactors
      1 Aciclovir PVB        1             h       NA            NA            NA
      2 Aciclovir PVB        1           min       NA            NA            NA
      3  AciclovirPop        1             h       NA            NA            NA
      4  AciclovirPop        1           min       NA            NA            NA
        yScaleFactors
      1            NA
      2            NA
      3            NA
      4            NA
      
      
      $files$plots$plotConfiguration
      $files$plots$plotConfiguration$column_names
       [1] "plotID"           "DataCombinedName" "plotType"         "title"           
       [5] "xUnit"            "yUnit"            "xAxisScale"       "yAxisScale"      
       [9] "xValuesLimits"    "yValuesLimits"    "aggregation"      "quantiles"       
      [13] "nsd"              "foldDistance"    
      
      $files$plots$plotConfiguration$rows
        plotID DataCombinedName             plotType title xUnit yUnit xAxisScale
      1     P1     AciclovirPVB           individual    NA     h    NA         NA
      2     P2     AciclovirPVB  observedVsSimulated    NA  <NA>    NA         NA
      3     P3     AciclovirPVB residualsVsSimulated    NA  <NA>    NA         NA
      4     P4     AciclovirPop           population    NA     h    NA         NA
        yAxisScale xValuesLimits yValuesLimits aggregation quantiles  nsd
      1         NA         0, 24            NA        <NA>        NA <NA>
      2         NA          <NA>            NA        <NA>        NA <NA>
      3         NA          <NA>            NA        <NA>        NA <NA>
      4         NA         0, 24            NA  arithmetic        NA 1.96
        foldDistance
      1         <NA>
      2         2, 3
      3         <NA>
      4         <NA>
      
      
      $files$plots$plotGrids
      $files$plots$plotGrids$column_names
      [1] "name"    "plotIDs" "title"  
      
      $files$plots$plotGrids$rows
              name    plotIDs title
      1  Aciclovir P1, P2, P3    NA
      2 Aciclovir2         P2    NA
      3 Aciclovir3         P4    NA
      
      
      $files$plots$exportConfiguration
      $files$plots$exportConfiguration$column_names
      [1] "plotGridName" "outputName"   "width"       
      
      $files$plots$exportConfiguration$rows
      list()
      
      
      $files$plots$dataTypes
      $files$plots$dataTypes$column_names
      [1] "dataType"
      
      $files$plots$dataTypes$rows
         dataType
      1  observed
      2 simulated
      
      
      $files$plots$plotTypes
      $files$plots$plotTypes$column_names
      [1] "plotType"
      
      $files$plots$plotTypes$rows
                    plotType
      1           individual
      2           population
      3  observedVsSimulated
      4 residualsVsSimulated
      5      residualsVsTime
      
      
      $files$plots$ObservedDataNames
      $files$plots$ObservedDataNames$column_names
      list()
      
      $files$plots$ObservedDataNames$rows
      list()
      
      
      
      $files$populationsCSV
      $files$populationsCSV$TestPopulation.csv
      $files$populationsCSV$TestPopulation.csv$column_names
        [1] "IndividualId"                                                             
        [2] "Gender"                                                                   
        [3] "Population"                                                               
        [4] "Organism.Weight"                                                          
        [5] "Organism.BMI"                                                             
        [6] "Organism.BSA"                                                             
        [7] "Organism.Age"                                                             
        [8] "Organism.Gestational.age"                                                 
        [9] "Organism.Height"                                                          
       [10] "Organism.Ontogeny.factor..albumin."                                       
       [11] "Organism.Ontogeny.factor..alpha1.acid.glycoprotein."                      
       [12] "Organism.Hematocrit"                                                      
       [13] "Organism.VenousBlood.Volume"                                              
       [14] "Organism.ArterialBlood.Volume"                                            
       [15] "Organism.Bone.Specific.blood.flow.rate"                                   
       [16] "Organism.Bone.Volume"                                                     
       [17] "Organism.Brain.Volume"                                                    
       [18] "Organism.Brain.Specific.blood.flow.rate"                                  
       [19] "Organism.Fat.Volume"                                                      
       [20] "Organism.Fat.Vf..lipid."                                                  
       [21] "Organism.Fat.Vf..neutral.lipid..PT"                                       
       [22] "Organism.Fat.Vf..phospholipid..PT"                                        
       [23] "Organism.Fat.Fraction.interstitial"                                       
       [24] "Organism.Fat.Vf..water."                                                  
       [25] "Organism.Fat.Vf..water..PT"                                               
       [26] "Organism.Fat.Vf..neutral.lipid..RR"                                       
       [27] "Organism.Fat.Vf..neutral.lipid..WS"                                       
       [28] "Organism.Fat.Vf..neutral.phospholipid..RR"                                
       [29] "Organism.Fat.Vf..neutral.phospholipid..plasma..WS"                        
       [30] "Organism.Fat.Vf..extracellular.water..RR"                                 
       [31] "Organism.Fat.Vf..intracellular.water..RR"                                 
       [32] "Organism.Fat.Vf..water..WS"                                               
       [33] "Organism.Fat.Specific.blood.flow.rate"                                    
       [34] "Organism.Gonads.Volume"                                                   
       [35] "Organism.Gonads.Specific.blood.flow.rate"                                 
       [36] "Organism.Heart.Volume"                                                    
       [37] "Organism.Heart.Specific.blood.flow.rate"                                  
       [38] "Organism.Kidney.Volume"                                                   
       [39] "Organism.Kidney.Age.of.aging.onset"                                       
       [40] "Organism.Kidney.Aging.half.time"                                          
       [41] "Organism.Kidney.GFRmat"                                                   
       [42] "Organism.Kidney.Hill.coefficient.for.aging.GFR"                           
       [43] "Organism.Kidney.Hill.coefficient.for.GFR"                                 
       [44] "Organism.Kidney.Maximal.decreasing.rate.factor"                           
       [45] "Organism.Kidney.TM50.for.GFR"                                             
       [46] "Organism.Kidney.Specific.blood.flow.rate"                                 
       [47] "Organism.Lumen.Effective.surface.area.variability.factor"                 
       [48] "Organism.Lumen.Stomach.Distal.radius"                                     
       [49] "Organism.Lumen.Stomach.Gastric.emptying.time"                             
       [50] "Organism.Lumen.Stomach.GET_alpha..Weibull.function..variability.factor"   
       [51] "Organism.Lumen.Stomach.GET_beta..Weibull.function..variability.factor"    
       [52] "Organism.Lumen.Stomach.Length"                                            
       [53] "Organism.Lumen.Stomach.Proximal.radius"                                   
       [54] "Organism.Lumen.Duodenum.Effective.surface.area.enhancement.factor"        
       [55] "Organism.Lumen.UpperJejunum.Effective.surface.area.enhancement.factor"    
       [56] "Organism.Lumen.LowerJejunum.Effective.surface.area.enhancement.factor"    
       [57] "Organism.Lumen.UpperIleum.Effective.surface.area.enhancement.factor"      
       [58] "Organism.Lumen.LowerIleum.Effective.surface.area.enhancement.factor"      
       [59] "Organism.Lumen.Caecum.Effective.surface.area.enhancement.factor"          
       [60] "Organism.Lumen.ColonAscendens.Effective.surface.area.enhancement.factor"  
       [61] "Organism.Lumen.ColonTransversum.Effective.surface.area.enhancement.factor"
       [62] "Organism.Lumen.ColonDescendens.Effective.surface.area.enhancement.factor" 
       [63] "Organism.Lumen.ColonSigmoid.Effective.surface.area.enhancement.factor"    
       [64] "Organism.Lumen.Rectum.Effective.surface.area.enhancement.factor"          
       [65] "Organism.Stomach.Volume"                                                  
       [66] "Organism.Stomach.Specific.blood.flow.rate"                                
       [67] "Organism.SmallIntestine.Small.intestinal.transit.time"                    
       [68] "Organism.SmallIntestine.Volume"                                           
       [69] "Organism.SmallIntestine.Specific.blood.flow.rate"                         
       [70] "Organism.LargeIntestine.Large.intestinal.transit.time"                    
       [71] "Organism.LargeIntestine.Volume"                                           
       [72] "Organism.LargeIntestine.Specific.blood.flow.rate"                         
       [73] "Organism.Liver.Volume"                                                    
       [74] "Organism.Liver.Specific.blood.flow.rate"                                  
       [75] "Organism.Lung.Volume"                                                     
       [76] "Organism.Lung.Fraction.vascular"                                          
       [77] "Organism.Muscle.Volume"                                                   
       [78] "Organism.Muscle.Vf..lipid."                                               
       [79] "Organism.Muscle.Vf..neutral.lipid..PT"                                    
       [80] "Organism.Muscle.Vf..phospholipid..PT"                                     
       [81] "Organism.Muscle.Vf..protein."                                             
       [82] "Organism.Muscle.Vf..water."                                               
       [83] "Organism.Muscle.Vf..water..PT"                                            
       [84] "Organism.Muscle.Fraction.interstitial"                                    
       [85] "Organism.Muscle.Vf..neutral.lipid..RR"                                    
       [86] "Organism.Muscle.Vf..neutral.lipid..WS"                                    
       [87] "Organism.Muscle.Vf..neutral.phospholipid..RR"                             
       [88] "Organism.Muscle.Vf..neutral.phospholipid..plasma..WS"                     
       [89] "Organism.Muscle.Vf..extracellular.water..RR"                              
       [90] "Organism.Muscle.Vf..protein..WS"                                          
       [91] "Organism.Muscle.Vf..intracellular.water..RR"                              
       [92] "Organism.Muscle.Vf..water..WS"                                            
       [93] "Organism.Muscle.Specific.blood.flow.rate"                                 
       [94] "Organism.Pancreas.Volume"                                                 
       [95] "Organism.Pancreas.Specific.blood.flow.rate"                               
       [96] "Organism.PortalVein.Volume"                                               
       [97] "Organism.Skin.Volume"                                                     
       [98] "Organism.Skin.Specific.blood.flow.rate"                                   
       [99] "Organism.Spleen.Volume"                                                   
      [100] "Organism.Spleen.Specific.blood.flow.rate"                                 
      
      $files$populationsCSV$TestPopulation.csv$rows
        IndividualId Gender         Population  Organism.Weight      Organism.BMI
      1            0   MALE European_ICRP_2002 61.5347980234939  0.22110435036443
      2            1   MALE European_ICRP_2002 62.9655208804219 0.196540160342841
            Organism.BSA     Organism.Age Organism.Gestational.age  Organism.Height
      1 168.868520364698 40.2758611595611                       40 16.6825235226199
      2 176.938242514102 23.9117595571614                       40 17.8988754990528
        Organism.Ontogeny.factor..albumin.
      1                  0.899636802882332
      2                    0.8530616107376
        Organism.Ontogeny.factor..alpha1.acid.glycoprotein. Organism.Hematocrit
      1                                    1.15949978271202                0.47
      2                                   0.699689929706047   0.449705865190538
        Organism.VenousBlood.Volume Organism.ArterialBlood.Volume
      1            0.90091056303014             0.406135603100194
      2            0.92167719798216             0.424738350141017
        Organism.Bone.Specific.blood.flow.rate Organism.Bone.Volume
      1                     0.0315090234960704     10.8690045999271
      2                     0.0319717624193636     11.4983895299467
        Organism.Brain.Volume Organism.Brain.Specific.blood.flow.rate
      1      1.57168243846914                       0.489495988836155
      2      1.47702462355843                       0.510232110737281
        Organism.Fat.Volume Organism.Fat.Vf..lipid.
      1    7.52250463972452                     0.8
      2    8.19841733381987       0.771588211266753
        Organism.Fat.Vf..neutral.lipid..PT Organism.Fat.Vf..phospholipid..PT
      1                               0.79                             0.002
      2                  0.761943358625919               0.00192897052816688
        Organism.Fat.Fraction.interstitial Organism.Fat.Vf..water.
      1                               0.16                    0.15
      2                  0.184352961771355       0.178411788733247
        Organism.Fat.Vf..water..PT Organism.Fat.Vf..neutral.lipid..RR
      1                       0.18                              0.853
      2          0.214094146479896                  0.822705930263175
        Organism.Fat.Vf..neutral.lipid..WS Organism.Fat.Vf..neutral.phospholipid..RR
      1                               0.92                                    0.0016
      2                  0.887326442956766                       0.00154317642253351
        Organism.Fat.Vf..neutral.phospholipid..plasma..WS
      1                                          0.002024
      2                               0.00195211817450489
        Organism.Fat.Vf..extracellular.water..RR
      1                                    0.135
      2                        0.160570609859922
        Organism.Fat.Vf..intracellular.water..RR Organism.Fat.Vf..water..WS
      1                      0.00899999999999998                       0.03
      2                       0.0107047073239948         0.0356823577466494
        Organism.Fat.Specific.blood.flow.rate Organism.Gonads.Volume
      1                    0.0206732323263958     0.0363901626133667
      2                    0.0257942437085706     0.0302628508592311
        Organism.Gonads.Specific.blood.flow.rate Organism.Heart.Volume
      1                       0.0806178855218515     0.427747241993127
      2                        0.107258474391816     0.198169802641703
        Organism.Heart.Specific.blood.flow.rate Organism.Kidney.Volume
      1                       0.682071775659985       0.49070666449687
      2                       0.710677787703658      0.493479572294716
        Organism.Kidney.Age.of.aging.onset Organism.Kidney.Aging.half.time
      1                                 30                              54
      2                                 30                              54
        Organism.Kidney.GFRmat Organism.Kidney.Hill.coefficient.for.aging.GFR
      1     0.0702927779334989                                            1.5
      2      0.107102771082758                                            1.5
        Organism.Kidney.Hill.coefficient.for.GFR
      1                         15.3024731123431
      2                         15.6612677239469
        Organism.Kidney.Maximal.decreasing.rate.factor Organism.Kidney.TM50.for.GFR
      1                                            0.9             45.2037451452012
      2                                            0.9             44.0003667679178
        Organism.Kidney.Specific.blood.flow.rate
      1                          2.8377452010901
      2                         3.38592193173909
        Organism.Lumen.Effective.surface.area.variability.factor
      1                                         1.77008432606349
      2                                        0.867556397888081
        Organism.Lumen.Stomach.Distal.radius
      1                                  0.5
      2                    0.489621579458441
        Organism.Lumen.Stomach.Gastric.emptying.time
      1                             5.96819895017132
      2                             13.9818897126764
        Organism.Lumen.Stomach.GET_alpha..Weibull.function..variability.factor
      1                                                        3.2105442703701
      2                                                      0.977244398387356
        Organism.Lumen.Stomach.GET_beta..Weibull.function..variability.factor
      1                                                       1.1006818299719
      2                                                      1.04051643753397
        Organism.Lumen.Stomach.Length Organism.Lumen.Stomach.Proximal.radius
      1                             2                                    0.5
      2              1.95847820017984                      0.489621579458441
        Organism.Lumen.Duodenum.Effective.surface.area.enhancement.factor
      1                                                          292.6883
      2                                                          292.6883
        Organism.Lumen.UpperJejunum.Effective.surface.area.enhancement.factor
      1                                                              447.9877
      2                                                              447.9877
        Organism.Lumen.LowerJejunum.Effective.surface.area.enhancement.factor
      1                                                              372.9358
      2                                                              372.9358
        Organism.Lumen.UpperIleum.Effective.surface.area.enhancement.factor
      1                                                            260.7527
      2                                                            260.7527
        Organism.Lumen.LowerIleum.Effective.surface.area.enhancement.factor
      1                                                             146.565
      2                                                             146.565
        Organism.Lumen.Caecum.Effective.surface.area.enhancement.factor
      1                                                             1.8
      2                                                             1.8
        Organism.Lumen.ColonAscendens.Effective.surface.area.enhancement.factor
      1                                                                     2.5
      2                                                                     2.5
        Organism.Lumen.ColonTransversum.Effective.surface.area.enhancement.factor
      1                                                                       2.5
      2                                                                       2.5
        Organism.Lumen.ColonDescendens.Effective.surface.area.enhancement.factor
      1                                                                      2.5
      2                                                                      2.5
        Organism.Lumen.ColonSigmoid.Effective.surface.area.enhancement.factor
      1                                                                   2.5
      2                                                                   2.5
        Organism.Lumen.Rectum.Effective.surface.area.enhancement.factor
      1                                                            3.56
      2                                                            3.56
        Organism.Stomach.Volume Organism.Stomach.Specific.blood.flow.rate
      1       0.194438572408107                         0.448201473480319
      2       0.187016027845032                         0.397796555465622
        Organism.SmallIntestine.Small.intestinal.transit.time
      1                                      107.630542050005
      2                                      79.8673742545636
        Organism.SmallIntestine.Volume
      1              0.592413461580726
      2               0.64221475348445
        Organism.SmallIntestine.Specific.blood.flow.rate
      1                                 0.92329394054034
      2                                0.893277355348927
        Organism.LargeIntestine.Large.intestinal.transit.time
      1                                                  2652
      2                                      2592.33524366018
        Organism.LargeIntestine.Volume
      1              0.463264373078633
      2              0.420636829269417
        Organism.LargeIntestine.Specific.blood.flow.rate Organism.Liver.Volume
      1                                0.635781142717255      2.63322588671254
      2                                0.691223816270522       2.3273205469601
        Organism.Liver.Specific.blood.flow.rate Organism.Lung.Volume
      1                       0.175550792244102     1.17197321761479
      2                       0.211653907325067      1.2070302874223
        Organism.Lung.Fraction.vascular Organism.Muscle.Volume
      1                            0.58       29.3017789696476
      2                            0.58       30.4937318900507
        Organism.Muscle.Vf..lipid. Organism.Muscle.Vf..neutral.lipid..PT
      1                      0.013                                0.0238
      2                      0.013                                0.0238
        Organism.Muscle.Vf..phospholipid..PT Organism.Muscle.Vf..protein.
      1                               0.0072                        0.177
      2                               0.0072                        0.177
        Organism.Muscle.Vf..water. Organism.Muscle.Vf..water..PT
      1                      0.811                          0.76
      2                      0.811                          0.76
        Organism.Muscle.Fraction.interstitial Organism.Muscle.Vf..neutral.lipid..RR
      1                                  0.16                                 0.022
      2                     0.168117653923785                                 0.022
        Organism.Muscle.Vf..neutral.lipid..WS
      1                                0.0049
      2                                0.0049
        Organism.Muscle.Vf..neutral.phospholipid..RR
      1                                       0.0078
      2                                       0.0078
        Organism.Muscle.Vf..neutral.phospholipid..plasma..WS
      1                                               0.0042
      2                                               0.0042
        Organism.Muscle.Vf..extracellular.water..RR Organism.Muscle.Vf..protein..WS
      1                                       0.079                            0.19
      2                                       0.079                            0.19
        Organism.Muscle.Vf..intracellular.water..RR Organism.Muscle.Vf..water..WS
      1                                       0.666                          0.76
      2                                       0.666                          0.76
        Organism.Muscle.Specific.blood.flow.rate Organism.Pancreas.Volume
      1                        0.036611489119001         0.27516244712599
      2                       0.0338854472547501        0.240890815218283
        Organism.Pancreas.Specific.blood.flow.rate Organism.PortalVein.Volume
      1                          0.372521833876031          0.942640954550504
      2                          0.391209979824467           1.03448435940802
        Organism.Skin.Volume Organism.Skin.Specific.blood.flow.rate
      1     3.49102282995465                     0.0845837873081867
      2     3.08838802634855                      0.102410398588552
        Organism.Spleen.Volume Organism.Spleen.Specific.blood.flow.rate
      1      0.243795397465916                        0.909827131937137
      2     0.0816480831712129                        0.866536264266006
      
      
      
      

