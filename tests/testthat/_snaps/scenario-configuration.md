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
    Output
      ScenarioConfiguration: 
         Model file name: NULL 
         Scenario name: NULL 
         Parameters sheets: mySheet1 mySheet2 
         Individual Id: NULL 
         Population Id: NULL 
         Read population from csv file: FALSE 
         Application protocol: NULL 
         Simulation time intervals: 
           Interval: 1 
             Start: 0 
             End: 10 
             Resolution: 1 
         Simulation time intervals unit: min 
         Simulate steady-state: TRUE 
         Steady-state time: 5 

