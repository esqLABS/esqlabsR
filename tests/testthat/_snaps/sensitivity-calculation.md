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
      

