# `ScenarioConfiguration` active bindings are modified

    Code
      mySC <- ScenarioConfiguration$new(projectConfiguration = ProjectConfiguration$
        new())
      mySC$simulateSteadyState <- TRUE
      mySC$simulationTime <- "0, 10, 1"
      mySC$steadyStateTime <- 5
      mySC$simulationType <- "Population"
      mySC$removeParamSheets(NULL)
      mySC$addParamSheets(c("mySheet1", "mySheet2"))
      mySC$print(projectConfiguration = FALSE)
    Message
      <ScenarioConfiguration>
      
      -- Scenario configuration ------------------------------------------------------
        * Scenario name: NULL
        * Model file name: NULL
        * Application protocol: NULL
        * Simulation type: Population
        * Individual Id: NULL
        * Population Id: NULL
        * Read population from csv file: FALSE
        * Parameters sheets: mySheet1, mySheet2
        * Simulate steady-state: TRUE
        * Steady-state time: 5
      
      -- Simulation time intervals --
      
      Interval 1:
        * Start: 0
        * End: 10
        * Resolution: 1
        * Simulation time intervals unit: min
      Steady state:
        * Simulate steady-state: TRUE
        * Steady-state time: 5

# It can print Scenario Configuration

    Code
      print(scenarioConfigurations[[1]])
    Message
      <ScenarioConfiguration>
      
      -- Project configuration -------------------------------------------------------
        * Working Directory: D:/GitHub repositories/ESQlabs/esqlabsR/tests/testthat
        * Project Configuration file stored at: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/ProjectConfiguration.xlsx
      
      -- Paths --
      
      Folders:
        * Configurations Folder: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Configurations
        * Model Folder: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Models/Simulations
        * Data Folder: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Data
        * Output Folder: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Results
        * Populations Folder: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Configurations/PopulationsCSV
      Files:
        * Model Parameters File: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Configurations/ModelParameters.xlsx
        * Individuals File: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Configurations/Individuals.xlsx
        * Populations File: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Configurations/Populations.xlsx
        * Scenarios File: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Configurations/Scenarios.xlsx
        * Applications File: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Configurations/Applications.xlsx
        * Plots File: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Configurations/Plots.xlsx
        * Data File: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Data/TestProject_TimeValuesData.xlsx
        * Data Importer Configuration File: D:/GitHub
        repositories/ESQlabs/esqlabsR/inst/extdata/examples/TestProject/Data/esqlabs_dataImporter_configuration.xml
      
      -- Scenario configuration ------------------------------------------------------
        * Scenario name: TestScenario
        * Model file name: Aciclovir.pkml
        * Application protocol: Aciclovir_iv_250mg
        * Simulation type: Individual
        * Individual Id: Indiv1
        * Population Id: NULL
        * Read population from csv file: FALSE
        * Parameters sheets: Global
        * Simulate steady-state: FALSE
        * Steady-state time: 1000
      
      -- Simulation time intervals --
      
      Interval 1:
        * Start: 0
        * End: 24
        * Resolution: 60
        * Simulation time intervals unit: h

