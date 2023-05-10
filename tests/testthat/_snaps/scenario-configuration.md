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
      mySC
    Output
      ProjectConfiguration: 
         Relative path from working directory: C:/Users/Felix/Code/esqlabsR/tests/testthat 
         Project Configuration File: 
         Model folder: 
         Parameters folder: 
         Parameters file name: 
         Individual parameters file name: 
         Population parameters file name: 
         Scenario definitions file name: 
         Scenario applications definitions file name: 
         Plot definitions file name: 
         Experimental data folder: 
         Experimental data file: 
         Data importer configuration: 
         Compound Properties File: 
         Output folder: 
      ScenarioConfiguration: 
         Model file name: 
         Scenario name: 
         Parameters sheets: mySheet1 mySheet2 
         Individual Id: 
         Population Id: 
         Read population from csv file: FALSE 
         Application protocol: 
         Simulation time intervals: 
           Interval: 1 
             Start: 0 
             End: 10 
             Resolution: 1 
         Simulation time intervals unit: min 
         Simulate steady-state: TRUE 
         Steady-state time: 5 

