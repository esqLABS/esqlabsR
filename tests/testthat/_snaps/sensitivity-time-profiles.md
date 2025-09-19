# sensitivityTimeProfiles creates expected default plot

    Code
      pb$plot$labels
    Output
      <ggplot2::labels> List of 6
       $ x     : chr "Time [min]"
       $ y     : chr "Concentration (molar) [µmol/l]"
       $ colour: chr "Parameter factor"
       $ title : NULL
       $ group : chr "ParameterFactor"
       $ alt   : chr ""

# sensitivityTimeProfiles applies linear y-axis scaling correctly

    Code
      extractAxisRange(p)
    Output
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`$yRange
      [1]  -50.76 1065.86  -50.76 1065.86  -50.76 1065.86
      
      

# sensitivityTimeProfiles applies y-unit conversion for multiple paths

    Code
      extractAxisRange(p)
    Output
      $`Organism|Age`
      $`Organism|Age`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|Age`$yRange
      [1] 2.442 2.539 2.442 2.539 2.442 2.539
      
      
      $`Organism|ArterialBlood|Plasma|Aciclovir`
      $`Organism|ArterialBlood|Plasma|Aciclovir`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|ArterialBlood|Plasma|Aciclovir`$yRange
      [1] -0.4633  5.8395 -0.4633  5.8395 -0.4633  5.8395
      
      
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`$yRange
      [1] -8.534 -2.735 -8.534 -2.735 -8.534 -2.735
      
      

# sensitivityTimeProfiles handles y-unit conversion with `NULL` for multiple paths

    Code
      extractAxisRange(p)
    Output
      $`Organism|Age`
      $`Organism|Age`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|Age`$yRange
      [1] 1.363 1.460 1.363 1.460 1.363 1.460
      
      
      $`Organism|ArterialBlood|Plasma|Aciclovir`
      $`Organism|ArterialBlood|Plasma|Aciclovir`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|ArterialBlood|Plasma|Aciclovir`$yRange
      [1] -9.463 -3.161 -9.463 -3.161 -9.463 -3.161
      
      
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`$yRange
      [1] -8.534 -2.735 -8.534 -2.735 -8.534 -2.735
      
      

# sensitivityTimeProfiles applies y-unit conversion with a single unit for multiple paths

    Code
      extractAxisRange(p1)
    Output
      $`Organism|Age`
      $`Organism|Age`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|Age`$yRange
      [1] 1.363 1.460 1.363 1.460 1.363 1.460
      
      
      $`Organism|ArterialBlood|Plasma|Aciclovir`
      $`Organism|ArterialBlood|Plasma|Aciclovir`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|ArterialBlood|Plasma|Aciclovir`$yRange
      [1] -3.463  2.839 -3.463  2.839 -3.463  2.839
      
      
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`$xRange
      [1]  -72 1512  -72 1512  -72 1512
      
      $`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`$yRange
      [1] -6.1814 -0.3824 -6.1814 -0.3824 -6.1814 -0.3824
      
      

