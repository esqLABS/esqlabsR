# `ScenarioConfiguration` active bindings are modified

    Code
      mySC <- ScenarioConfiguration$new(projectConfiguration = ProjectConfiguration$
        new())
      mySC$setTestParameters <- TRUE
      mySC$simulateSteadyState <- TRUE
      mySC$simulationTime <- 10
      mySC$steadyStateTime <- 5
      mySC$pointsPerMinute <- 100
      mySC$simulationType <- "Population"
      mySC$simulationRunOptions <- NULL
      mySC$removeParamSheets(NULL)
      mySC$addParamSheets(c("mySheet1", "mySheet2"))
      mySC
    Output
      ProjectConfiguration: 
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
         Output folder: 
      ScenarioConfiguration: 
         Model file name: 
         Scenario name: 
         Parameters sheets: mySheet1 mySheet2 
         Individual Id: 
         Population Id: 
         Read population from csv file: FALSE 
         Application protocol: 
         Simulation time: 10 
         Points per minute: 100 
         Simulate steady-state: TRUE 
         Steady-state time: 5 
         Set test parameters: TRUE 

