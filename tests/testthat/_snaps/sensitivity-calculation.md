# sensitivityCalculation PK parameters tidy dataframe is as expected

    Code
      df1_pk
    Output
      $charColumnSummary
      # A tibble: 3 x 5
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      2 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      3 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath           ParameterUnit PKParameter Unit      
        <chr>                   <chr>         <chr>       <chr>     
      1 Aciclovir|Lipophilicity Log Units     AUC_inf     µmol*min/l
      2 Aciclovir|Lipophilicity Log Units     C_max       µmol/l    
      3 Aciclovir|Lipophilicity Log Units     t_max       h         
      
      $numericColumnSummary
      # A tibble: 5 x 8
        column                 Min.        `1st Qu.`   Median      Mean       
        <chr>                  <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor         0.1000      0.7750      1.500000      5.775000
      2 ParameterValue         -1.9400     -0.6305     -0.145500     -0.560175
      3 PKParameterValue        0.1833      0.1833     50.964733   1382.726744
      4 PKPercentChange        -2.6806      0.0000      0.000000      3.313318
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
      # A tibble: 3 x 5
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      2 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
      3 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath                                                    
        <chr>                                                            
      1 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose
      2 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose
      3 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose
        ParameterUnit PKParameter Unit      
        <chr>         <chr>       <chr>     
      1 kg            AUC_inf     µmol*min/l
      2 kg            C_max       µmol/l    
      3 kg            t_max       h         
      
      $numericColumnSummary
      # A tibble: 5 x 8
        column                 Min.        `1st Qu.`   Median      Mean       
        <chr>                  <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor          0.100000  0.7750000    1.500000      5.775000
      2 ParameterValue           0.000025  0.0001937    0.000375      0.001444
      3 PKParameterValue         0.183333  0.1833333   75.379087   7936.607132
      4 PKPercentChange        -90.000112  0.0000000    0.000000    318.333335
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
      # A tibble: 3 x 5
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
        ParameterUnit PKParameter Unit      
        <chr>         <chr>       <chr>     
      1 ""            AUC_inf     µmol*min/l
      2 ""            C_max       µmol/l    
      3 ""            t_max       h         
      
      $numericColumnSummary
      # A tibble: 5 x 8
        column                 Min.        `1st Qu.`   Median      Mean       
        <chr>                  <table[1d]> <table[1d]> <table[1d]> <table[1d]>
      1 ParameterFactor          0.1000     0.77500     1.50000       5.7750  
      2 ParameterValue           0.1000     0.77500     1.50000       5.7750  
      3 PKParameterValue         0.1667     0.18333    49.78161    1210.6412  
      4 PKPercentChange        -52.4018    -9.74787     0.00000      -5.9048  
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
      # A tibble: 12 x 10
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
         ParameterFactor ParameterValue ParameterUnit PKParameter PKParameterValue
                   <dbl>          <dbl> <chr>         <chr>                  <dbl>
       1             0.1      -0.0097   "Log Units"   minmax            0.0006992 
       2             1        -0.097    "Log Units"   minmax            0.0006476 
       3             2        -0.194    "Log Units"   minmax            0.0006206 
       4            20        -1.94     "Log Units"   minmax            0.0008589 
       5             0.1       0.000025 "kg"          minmax            0.0006475 
       6             1         0.00025  "kg"          minmax            0.0006476 
       7             2         0.0005   "kg"          minmax            0.0006476 
       8            20         0.005    "kg"          minmax            0.0006476 
       9             0.1       0.1      ""            minmax            0.001230  
      10             1         1        ""            minmax            0.0006476 
      11             2         2        ""            minmax            0.0003805 
      12            20        20        ""            minmax            0.00005314
         PKPercentChange Unit  SensitivityPKParameter
                   <dbl> <chr>                  <dbl>
       1      7.968      <NA>               -8.854e-2
       2      0          <NA>              NaN       
       3     -4.157      <NA>               -4.157e-2
       4     32.64       <NA>                1.718e-2
       5     -0.01035    <NA>                1.149e-4
       6      0          <NA>              NaN       
       7     -0.00002663 <NA>               -2.663e-7
       8     -0.00002665 <NA>               -1.403e-8
       9     89.87       <NA>               -9.985e-1
      10      0          <NA>              NaN       
      11    -41.24       <NA>               -4.124e-1
      12    -91.79       <NA>               -4.831e-2

# sensitivityCalculation returns expected results with multiple custom functions

    Code
      customPKData
    Output
      # A tibble: 24 x 10
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
         ParameterFactor ParameterValue ParameterUnit PKParameter PKParameterValue
                   <dbl>          <dbl> <chr>         <chr>                  <dbl>
       1             0.1      -0.0097   Log Units     max_slope             5.847 
       2             1        -0.097    Log Units     max_slope             5.916 
       3             2        -0.194    Log Units     max_slope             5.979 
       4            20        -1.94     Log Units     max_slope             7.418 
       5             0.1      -0.0097   Log Units     minmax             1430.    
       6             1        -0.097    Log Units     minmax             1544.    
       7             2        -0.194    Log Units     minmax             1611.    
       8            20        -1.94     Log Units     minmax             1164.    
       9             0.1       0.000025 kg            max_slope             0.5916
      10             1         0.00025  kg            max_slope             5.916 
         PKPercentChange Unit  SensitivityPKParameter
                   <dbl> <chr>                  <dbl>
       1          -1.167 <NA>                 0.01297
       2           0     <NA>               NaN      
       3           1.057 <NA>                 0.01057
       4          25.38  <NA>                 0.01336
       5          -7.380 <NA>                 0.08200
       6           0     <NA>               NaN      
       7           4.338 <NA>                 0.04338
       8         -24.61  <NA>                -0.01295
       9         -90.00  <NA>                 1.000  
      10           0     <NA>               NaN      
      # i 14 more rows

# sensitivityCalculation applies custom PK parameter function correctly with multiple output paths

    Code
      customPKDataMultiple
    Output
      # A tibble: 27 x 10
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
         ParameterFactor ParameterValue ParameterUnit PKParameter PKParameterValue
                   <dbl>          <dbl> <chr>         <chr>                  <dbl>
       1               1       -0.097   Log Units     minmax             0.0003409
       2               1       -0.097   Log Units     minmax             0.0006476
       3               1       -0.097   Log Units     minmax             1        
       4               5       -0.485   Log Units     minmax             0.0002932
       5               5       -0.485   Log Units     minmax             0.0005992
       6               5       -0.485   Log Units     minmax             1        
       7              10       -0.97    Log Units     minmax             0.0005275
       8              10       -0.97    Log Units     minmax             0.0009463
       9              10       -0.97    Log Units     minmax             1        
      10               1        0.00025 kg            minmax             0.0003409
         PKPercentChange Unit  SensitivityPKParameter
                   <dbl> <chr>                  <dbl>
       1           0     <NA>               NaN      
       2           0     <NA>               NaN      
       3           0     <NA>               NaN      
       4         -14.01  <NA>                -0.03501
       5          -7.474 <NA>                -0.01868
       6           0     <NA>                 0      
       7          54.72  <NA>                 0.06080
       8          46.13  <NA>                 0.05126
       9           0     <NA>                 0      
      10           0     <NA>               NaN      
      # i 17 more rows

