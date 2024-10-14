# All scenarios are available after project is initialized

    Code
      names(project$scenarios)
    Output
      [1] "TestScenario"              "TestScenario2"            
      [3] "PopulationScenario"        "PopulationScenarioFromCSV"
      [5] "TestScenario_missingParam"

# The user can activate only some scenarios

    Code
      purrr::map_chr(project$scenarios, "status")
    Output
                   TestScenario             TestScenario2        PopulationScenario 
                       "active"                  "active"                "inactive" 
      PopulationScenarioFromCSV TestScenario_missingParam 
                     "inactive"                "inactive" 

# Scenarios can be loaded manually

    Code
      purrr::map_chr(project$scenarios, "status")
    Output
                   TestScenario             TestScenario2        PopulationScenario 
                       "loaded"                  "loaded"                "inactive" 
      PopulationScenarioFromCSV TestScenario_missingParam 
                     "inactive"                "inactive" 

# Scenario can be run

    Code
      project$simulationResults
    Output
      $TestScenario
      SimulationResults: 
         Number of individuals: 1 
      
      $TestScenario2
      SimulationResults: 
         Number of individuals: 1 
      

