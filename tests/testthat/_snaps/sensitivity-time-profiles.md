# sensitivityTimeProfiles plots are as expected

    Code
      pb$plot$labels
    Output
      $x
      [1] "Time [min]"
      
      $y
      [1] "Concentration (molar) [µmol/l]"
      
      $colour
      [1] "Parameter factor"
      
      $title
      NULL
      
      $group
      [1] "ParameterFactor"
      
      $alt
      [1] ""
      

# sensitivityTimeProfiles correctly applies linear y-axis scaling

    Code
      unlist(plotParams)
    Output
      [1]  -50.76 1065.86  -50.76 1065.86  -50.76 1065.86

