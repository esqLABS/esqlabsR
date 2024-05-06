# Individual configuration can be initialized

    Code
      project$configurations$individuals$Indiv1
    Output
      $id
      [1] "Indiv1"
      
      $specy
      [1] "Human"
      
      $population
      [1] "European_ICRP_2002"
      
      $gender
      [1] "MALE"
      
      $weight
      [1] 73
      
      $height
      [1] 176
      
      $age
      [1] 30
      
      $protein
      [1] NA
      
      $ontogeny
      [1] NA
      
      $GFR
      $containerPath
      [1] "Organism|Kidney"
      
      $parameterName
      [1] "GFR"
      
      $value
      [1] 90
      
      $units
      [1] "ml/min"
      
      

# Individual can be exported as dataFrame

    Code
      project$configurations$individuals$Indiv1$toDataFrame()
    Output
      $characteristics
      # A tibble: 1 x 9
        IndividualId Species Population         Gender `Weight [kg]` `Height [cm]`
        <chr>        <chr>   <chr>              <chr>          <dbl>         <dbl>
      1 Indiv1       Human   European_ICRP_2002 MALE              73           176
      # i 3 more variables: `Age [year(s)]` <dbl>, Protein <lgl>, Ontogeny <lgl>
      
      $parameters
      # A tibble: 1 x 4
        `Container Path` `Parameter Name` Value Units 
        <chr>            <chr>            <dbl> <chr> 
      1 Organism|Kidney  GFR                 90 ml/min
      

