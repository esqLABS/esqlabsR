# `ScenarioConfiguration` active bindings are modified

    Code
      project <- testProject()
      mySC <- ScenarioConfiguration$new(project = project, scenarioConfigurationData = project$
        configurations$scenarios$TestScenario$toDataFrame())
      mySC$simulationTime <- "0, 10, 1"
      mySC$steadyStateTime <- 5
      mySC$print()
    Output
      $id
      [1] "TestScenario"
      
      $individual
      [1] "Indiv1"
      
      $population
      [1] NA
      
      $populationFromCSV
      [1] NA
      
      $modelParameters
      [1] "Global"
      
      $applications
      [1] "Aciclovir_iv_250mg"
      
      $simulationTime
      [1] "0, 10, 1"
      
      $simulationTimeUnit
      [1] NA
      
      $steadyState
      [1] FALSE
      
      $steadyStateTime
      [1] 5
      
      $steadyStateTimeUnit
      [1] NA
      
      $model
      [1] "Aciclovir.pkml"
      
      $outputPaths
      [1] NA
      

