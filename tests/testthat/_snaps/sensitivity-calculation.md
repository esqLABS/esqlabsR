# sensitivityCalculation PK parameters tidy dataframe is as expected

    Code
      df1_pk
    Output
      $charColumnSummary
      # A tibble: 3 x 4
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      2 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      3 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath           PKParameter Unit      
        <chr>                   <chr>       <chr>     
      1 Aciclovir|Lipophilicity AUC_inf     µmol*min/l
      2 Aciclovir|Lipophilicity C_max       µmol/l    
      3 Aciclovir|Lipophilicity t_max       h         
      
      $numericColumnSummary
      # A tibble: 5 x 8
        column                 Min.        `1st Qu.`   Median      Mean       
        <chr>                  <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor         0.1000      0.7750      1.500000      5.775000
      2 ParameterValue         -1.9400     -0.6305     -0.145500     -0.560175
      3 PKParameterValue        0.1833      0.1833     50.964733   1382.726744
      4 PercentChangePK        -2.6806      0.0000      0.000000      3.313318
      5 SensitivityPKParameter  0.0000      0.0000      0.004299      0.009798
        `3rd Qu.`   Max.        `NA's`     
        <table[1d]> <table[1d]> <table[1d]>
      1    6.50000    20.00000  NA         
      2   -0.07518    -0.00970  NA         
      3 4059.14498  4154.58500  NA         
      4    0.82553    37.60585  NA         
      5    0.01979     0.02978   3         
      

---

    Code
      df2_pk
    Output
      $charColumnSummary
      # A tibble: 3 x 4
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      2 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      3 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath                                                     PKParameter
        <chr>                                                             <chr>      
      1 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose AUC_inf    
      2 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose C_max      
      3 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose t_max      
        Unit      
        <chr>     
      1 µmol*min/l
      2 µmol/l    
      3 h         
      
      $numericColumnSummary
      # A tibble: 5 x 8
        column                 Min.        `1st Qu.`   Median      Mean       
        <chr>                  <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor          0.100000  0.7750000    1.500000      5.775000
      2 ParameterValue           0.000025  0.0001937    0.000375      0.001444
      3 PKParameterValue         0.183333  0.1833333   75.379087   7936.607132
      4 PercentChangePK        -90.000112  0.0000000    0.000000    318.333335
      5 SensitivityPKParameter   0.000000  0.0000000    1.000000      0.666667
        `3rd Qu.`   Max.        `NA's`     
        <table[1d]> <table[1d]> <table[1d]>
      1    6.500000    20.000   NA         
      2    0.001625     0.005   NA         
      3 1771.948050 81452.570   NA         
      4  100.000006  1900.000   NA         
      5    1.000000     1.000    3         
      

---

    Code
      df3_pk
    Output
      $charColumnSummary
      # A tibble: 3 x 4
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      2 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      3 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath                                                                 
        <chr>                                                                         
      1 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fr~
      2 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fr~
      3 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fr~
        PKParameter Unit      
        <chr>       <chr>     
      1 AUC_inf     µmol*min/l
      2 C_max       µmol/l    
      3 t_max       h         
      
      $numericColumnSummary
      # A tibble: 5 x 8
        column                 Min.        `1st Qu.`   Median      Mean       
        <chr>                  <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor          0.1000     0.77500     1.50000       5.7750  
      2 ParameterValue           0.1000     0.77500     1.50000       5.7750  
      3 PKParameterValue         0.1667     0.18333    49.78161    1210.6412  
      4 PercentChangePK        -52.4018    -9.74787     0.00000      -5.9048  
      5 SensitivityPKParameter  -0.1784    -0.02758    -0.01875      -0.0418  
        `3rd Qu.`   Max.        `NA's`     
        <table[1d]> <table[1d]> <table[1d]>
      1    6.500000   20.00     NA         
      2    6.500000   20.00     NA         
      3 2352.716300 4726.35     NA         
      4    0.000000   16.05     NA         
      5   -0.004785    0.00      3         
      

# sensitivityCalculation returns expected results with single custom function

    Code
      customPKData
    Output
      # A tibble: 12 x 9
         OutputPath                                                               
         <chr>                                                                    
       1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       2 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       3 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       4 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       5 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       6 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       7 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       8 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       9 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      10 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      11 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      12 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
         ParameterPath                                                                
         <chr>                                                                        
       1 Aciclovir|Lipophilicity                                                      
       2 Aciclovir|Lipophilicity                                                      
       3 Aciclovir|Lipophilicity                                                      
       4 Aciclovir|Lipophilicity                                                      
       5 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose            
       6 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose            
       7 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose            
       8 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose            
       9 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR f~
      10 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR f~
      11 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR f~
      12 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR f~
         ParameterFactor ParameterValue PKParameter PKParameterValue Unit 
                   <dbl>          <dbl> <chr>                  <dbl> <chr>
       1             0.1      -0.0097   minmax            0.0006992  <NA> 
       2             1        -0.097    minmax            0.0006476  <NA> 
       3             2        -0.194    minmax            0.0006206  <NA> 
       4            20        -1.94     minmax            0.0008589  <NA> 
       5             0.1       0.000025 minmax            0.0006475  <NA> 
       6             1         0.00025  minmax            0.0006476  <NA> 
       7             2         0.0005   minmax            0.0006476  <NA> 
       8            20         0.005    minmax            0.0006476  <NA> 
       9             0.1       0.1      minmax            0.001230   <NA> 
      10             1         1        minmax            0.0006476  <NA> 
      11             2         2        minmax            0.0003805  <NA> 
      12            20        20        minmax            0.00005314 <NA> 
         PercentChangePK SensitivityPKParameter
                   <dbl>                  <dbl>
       1      7.968                   -8.854e-2
       2      0                      NaN       
       3     -4.157                   -4.157e-2
       4     32.64                     1.718e-2
       5     -0.01035                  1.149e-4
       6      0                      NaN       
       7     -0.00002663              -2.663e-7
       8     -0.00002665              -1.403e-8
       9     89.87                    -9.985e-1
      10      0                      NaN       
      11    -41.24                    -4.124e-1
      12    -91.79                    -4.831e-2

# sensitivityCalculation returns expected results with multiple custom functions

    Code
      customPKData
    Output
      # A tibble: 24 x 9
         OutputPath                                                               
         <chr>                                                                    
       1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       2 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       3 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       4 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       5 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       6 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       7 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       8 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       9 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      10 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
         ParameterPath                                                    
         <chr>                                                            
       1 Aciclovir|Lipophilicity                                          
       2 Aciclovir|Lipophilicity                                          
       3 Aciclovir|Lipophilicity                                          
       4 Aciclovir|Lipophilicity                                          
       5 Aciclovir|Lipophilicity                                          
       6 Aciclovir|Lipophilicity                                          
       7 Aciclovir|Lipophilicity                                          
       8 Aciclovir|Lipophilicity                                          
       9 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose
      10 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose
         ParameterFactor ParameterValue PKParameter PKParameterValue Unit 
                   <dbl>          <dbl> <chr>                  <dbl> <chr>
       1             0.1      -0.0097   max_slope             5.847  <NA> 
       2             1        -0.097    max_slope             5.916  <NA> 
       3             2        -0.194    max_slope             5.979  <NA> 
       4            20        -1.94     max_slope             7.418  <NA> 
       5             0.1      -0.0097   minmax             1430.     <NA> 
       6             1        -0.097    minmax             1544.     <NA> 
       7             2        -0.194    minmax             1611.     <NA> 
       8            20        -1.94     minmax             1164.     <NA> 
       9             0.1       0.000025 max_slope             0.5916 <NA> 
      10             1         0.00025  max_slope             5.916  <NA> 
         PercentChangePK SensitivityPKParameter
                   <dbl>                  <dbl>
       1          -1.167                0.01297
       2           0                  NaN      
       3           1.057                0.01057
       4          25.38                 0.01336
       5          -7.380                0.08200
       6           0                  NaN      
       7           4.338                0.04338
       8         -24.61                -0.01295
       9         -90.00                 1.000  
      10           0                  NaN      
      # i 14 more rows

# sensitivityCalculation time series dataframe is as expected

    Code
      df1_ts
    Output
      $charColumnSummary
      # A tibble: 1 x 6
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath           TimeDimension TimeUnit Dimension             Unit  
        <chr>                   <chr>         <chr>    <chr>                 <chr> 
      1 Aciclovir|Lipophilicity Time          min      Concentration (molar) µmol/l
      
      $numericColumnSummary
      # A tibble: 5 x 7
        column          Min.        `1st Qu.`   Median      Mean        `3rd Qu.`  
        <chr>           <table[1d]> <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor   0.10        0.7750      1.5000      5.7750       6.50000 
      2 ParameterValue   -1.94       -0.6305     -0.1455     -0.5602      -0.07518 
      3 Time              0.00      336.0000    705.0000    705.4888    1074.00000 
      4 Concentration     0.00        0.1159      0.4693      3.4824       2.79714 
      5 molWeight       225.21      225.2100    225.2100    225.2100     225.21000 
        Max.       
        <table[1d]>
      1   20.0000  
      2   -0.0097  
      3 1440.0000  
      4   69.1507  
      5  225.2100  
      

---

    Code
      df2_ts
    Output
      $charColumnSummary
      # A tibble: 1 x 6
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath                                                    
        <chr>                                                            
      1 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose
        TimeDimension TimeUnit Dimension             Unit  
        <chr>         <chr>    <chr>                 <chr> 
      1 Time          min      Concentration (molar) µmol/l
      
      $numericColumnSummary
      # A tibble: 5 x 7
        column          Min.        `1st Qu.`   Median      Mean        `3rd Qu.`  
        <chr>           <table[1d]> <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor   0.100000    0.7750000   1.500000    5.775000     6.500000
      2 ParameterValue    0.000025    0.0001937   0.000375    0.001444     0.001625
      3 Time              0.000000  336.0000000 705.000000  705.488798  1074.000000
      4 Concentration     0.000000    0.1381199   0.981653   19.741239     6.778747
      5 molWeight       225.210000  225.2100000 225.210000  225.210000   225.210000
        Max.       
        <table[1d]>
      1   20.000   
      2    0.005   
      3 1440.000   
      4 1005.055   
      5  225.210   
      

---

    Code
      df3_ts
    Output
      $charColumnSummary
      # A tibble: 1 x 6
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath                                                                 
        <chr>                                                                         
      1 Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fr~
        TimeDimension TimeUnit Dimension             Unit  
        <chr>         <chr>    <chr>                 <chr> 
      1 Time          min      Concentration (molar) µmol/l
      
      $numericColumnSummary
      # A tibble: 5 x 7
        column          Min.        `1st Qu.`   Median      Mean        `3rd Qu.`  
        <chr>           <table[1d]> <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor   0.1         0.775       1.5000      5.775        6.500   
      2 ParameterValue    0.1         0.775       1.5000      5.775        6.500   
      3 Time              0.0       336.000     705.0000    705.489     1074.000   
      4 Concentration     0.0         0.083       0.4509      3.065        2.758   
      5 molWeight       225.2       225.210     225.2100    225.210      225.210   
        Max.       
        <table[1d]>
      1   20.00    
      2   20.00    
      3 1440.00    
      4   51.26    
      5  225.21    
      

# sensitivityCalculation applies custom PK parameter function correctly with multiple output paths

    Code
      customPKDataMultiple
    Output
      # A tibble: 27 x 9
         OutputPath                                                               
         <chr>                                                                    
       1 Organism|ArterialBlood|Plasma|Aciclovir                                  
       2 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       3 Organism|Age                                                             
       4 Organism|ArterialBlood|Plasma|Aciclovir                                  
       5 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       6 Organism|Age                                                             
       7 Organism|ArterialBlood|Plasma|Aciclovir                                  
       8 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
       9 Organism|Age                                                             
      10 Organism|ArterialBlood|Plasma|Aciclovir                                  
         ParameterPath                                                    
         <chr>                                                            
       1 Aciclovir|Lipophilicity                                          
       2 Aciclovir|Lipophilicity                                          
       3 Aciclovir|Lipophilicity                                          
       4 Aciclovir|Lipophilicity                                          
       5 Aciclovir|Lipophilicity                                          
       6 Aciclovir|Lipophilicity                                          
       7 Aciclovir|Lipophilicity                                          
       8 Aciclovir|Lipophilicity                                          
       9 Aciclovir|Lipophilicity                                          
      10 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose
         ParameterFactor ParameterValue PKParameter PKParameterValue Unit 
                   <dbl>          <dbl> <chr>                  <dbl> <chr>
       1               1       -0.097   minmax             0.0003409 <NA> 
       2               1       -0.097   minmax             0.0006476 <NA> 
       3               1       -0.097   minmax             1         <NA> 
       4               5       -0.485   minmax             0.0002932 <NA> 
       5               5       -0.485   minmax             0.0005992 <NA> 
       6               5       -0.485   minmax             1         <NA> 
       7              10       -0.97    minmax             0.0005275 <NA> 
       8              10       -0.97    minmax             0.0009463 <NA> 
       9              10       -0.97    minmax             1         <NA> 
      10               1        0.00025 minmax             0.0003409 <NA> 
         PercentChangePK SensitivityPKParameter
                   <dbl>                  <dbl>
       1           0                  NaN      
       2           0                  NaN      
       3           0                  NaN      
       4         -14.01                -0.03501
       5          -7.474               -0.01868
       6           0                    0      
       7          54.72                 0.06080
       8          46.13                 0.05126
       9           0                    0      
      10           0                  NaN      
      # i 17 more rows
