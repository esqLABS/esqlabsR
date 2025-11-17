# extendPopulationByUserDefinedParams works

    Code
      population$getParameterValuesForIndividual(4)
    Output
      $paths
      [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
      [2] "Organism|Muscle|Intracellular|Aciclovir|Concentration"                    
      [3] "Organism|Kidney|GFR"                                                      
      
      $values
      [1] 0.0000000 0.0000000 0.1198664
      

# extendPopulationFromXLS works

    Code
      population$getParameterValuesForIndividual(4)
    Output
      $paths
      [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
      [2] "Organism|Muscle|Intracellular|Aciclovir|Concentration"                    
      [3] "Organism|Kidney|GFR"                                                      
      [4] "Organism|Kidney|eGFR"                                                     
      
      $values
      [1] 0.0000000 0.0000000 0.1198664 0.1186738
      

# extendPopulationFromXLS throws an error if specified sheet is empty or data is missing

    Code
      extendPopulationFromXLS(population, PopulationParameters, sheet = "UserDefinedVariability")
    Condition
      Warning in `extendPopulationFromXLS()`:
      x The specified excel sheet contains uncomplete row(s)
      i Using only complete rows to define population parameters
      Error in `extendPopulationFromXLS()`:
      ! x The specified excel sheet does not contain any complete row
      * Please fill all the columns and try again.

