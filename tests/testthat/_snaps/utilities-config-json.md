# projectConfigurationStatus() detects sheet-level changes in Excel files

    Code
      status_result$details
    Output
      $file_status
      $file_status$projectConfiguration
      [1] "in-sync"
      
      $file_status$modelParameterSets
      [1] "in-sync"
      
      $file_status$Individuals
      [1] "in-sync"
      
      $file_status$Populations
      [1] "in-sync"
      
      $file_status$Scenarios
      [1] "in-sync"
      
      $file_status$Applications
      [1] "in-sync"
      
      $file_status$Plots
      [1] "out-of-sync"
      
      
      $file_changes
      NULL
      
      $sheet_changes
      $sheet_changes$Plots
      $sheet_changes$Plots$added
      [1] "NewTestSheet"
      
      
      
      $data_changes
      NULL
      

# projectConfigurationStatus() detects data-level changes in Excel sheets

    Code
      status_result$details
    Output
      $file_status
      $file_status$projectConfiguration
      [1] "in-sync"
      
      $file_status$modelParameterSets
      [1] "in-sync"
      
      $file_status$Individuals
      [1] "in-sync"
      
      $file_status$Populations
      [1] "in-sync"
      
      $file_status$Scenarios
      [1] "out-of-sync"
      
      $file_status$Applications
      [1] "in-sync"
      
      $file_status$Plots
      [1] "in-sync"
      
      
      $file_changes
      NULL
      
      $sheet_changes
      NULL
      
      $data_changes
      $data_changes$Scenarios
      [1] "Scenarios"
      
      

# projectConfigurationStatus() handles simultaneous sheet, and data changes

    Code
      status_result$details
    Output
      $file_status
      $file_status$projectConfiguration
      [1] "in-sync"
      
      $file_status$modelParameterSets
      [1] "in-sync"
      
      $file_status$Individuals
      [1] "in-sync"
      
      $file_status$Populations
      [1] "in-sync"
      
      $file_status$Scenarios
      [1] "out-of-sync"
      
      $file_status$Applications
      [1] "in-sync"
      
      $file_status$Plots
      [1] "out-of-sync"
      
      
      $file_changes
      NULL
      
      $sheet_changes
      $sheet_changes$Plots
      $sheet_changes$Plots$added
      [1] "CombinedTestSheet"
      
      
      
      $data_changes
      $data_changes$Scenarios
      [1] "Scenarios"
      
      

