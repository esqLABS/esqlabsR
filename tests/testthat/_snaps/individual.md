# Individual configuration can be initialized

    Code
      project$configurations$individuals$Indiv1
    Message
      * Individual ID: Indiv1
        * Characteristics:
          * Specy: Human
          * Population: European_ICRP_2002
          * Gender: MALE
          * Weight: 73
          * Height: 176
          * Age: 30
          * Protein: NA
          * Ontogeny: NA
        * Parameters:
          * GFR

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
      

