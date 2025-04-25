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

