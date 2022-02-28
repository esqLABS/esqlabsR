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
        column                 Min.       `1st Qu.`  Median       Mean         
        <chr>                  <table>    <table>    <table>      <table>      
      1 ParameterFactor         0.1000000  0.7750000  1.500000000    5.77500000
      2 ParameterValue         -1.9400000 -0.6305000 -0.145500000   -0.56017500
      3 PKParameterValue        0.1833333  0.1833333 50.964733124 1382.72673700
      4 PercentChangePK        -2.6805738  0.0000000  0.000000000    3.31332295
      5 SensitivityPKParameter  0.0000000  0.0000000  0.003060453    0.04174034
        `3rd Qu.`     Max.         `NA's` 
        <table>       <table>      <table>
      1    6.50000000   20.0000000 NA     
      2   -0.07517500   -0.0097000 NA     
      3 4059.14495850 4154.5849609 NA     
      4    0.82553192   37.6058569 NA     
      5    0.02076496    0.2876703  3     
      

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
        column                 Min.        `1st Qu.`  Median    Mean         
        <chr>                  <table>     <table>    <table>   <table>      
      1 ParameterFactor          0.1000000 0.77500000  1.500000    5.77500000
      2 ParameterValue           0.0000250 0.00019375  0.000375    0.00144375
      3 PKParameterValue         0.1833333 0.18333333 75.379087 7936.60715705
      4 PercentChangePK        -90.0001121 0.00000000  0.000000  318.33333830
      5 SensitivityPKParameter   0.0000000 0.00000000  1.000000    0.66666806
        `3rd Qu.`   Max.         `NA's` 
        <table>     <table>      <table>
      1    6.500000    20.000000 NA     
      2    0.001625     0.005000 NA     
      3 1771.948044 81452.570312 NA     
      4  100.000008  1900.000213 NA     
      5    1.000000     1.000012  3     
      

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
        column                 Min.        `1st Qu.`  Median      Mean        
        <chr>                  <table>     <table>    <table>     <table>     
      1 ParameterFactor          0.1000000  0.7750000  1.50000000    5.7750000
      2 ParameterValue           0.1000000  0.7750000  1.50000000    5.7750000
      3 PKParameterValue         0.1666667  0.1833333 49.78161049 1210.6411784
      4 PercentChangePK        -52.4017643 -9.7478727  0.00000000   -5.9047718
      5 SensitivityPKParameter  -1.1588613 -0.1689445 -0.03821575   -0.1949252
        `3rd Qu.`      Max.      `NA's` 
        <table>        <table>   <table>
      1    6.500000000   20.0000 NA     
      2    6.500000000   20.0000 NA     
      3 2352.716278076 4726.3550 NA     
      4    0.000000000   16.0517 NA     
      5   -0.002186634    0.0000  3     
      

# sensitivityCalculation time series dataframe is as expected

    Code
      df1_ts
    Output
      $charColumnSummary
      # A tibble: 1 x 6
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath           Unit   Dimension             TimeUnit TimeDimension
        <chr>                   <chr>  <chr>                 <chr>    <chr>        
      1 Aciclovir|Lipophilicity µmol/l Concentration (molar) min      Time         
      
      $numericColumnSummary
      # A tibble: 5 x 7
        column          Min.    `1st Qu.`   Median      Mean       `3rd Qu.`  
        <chr>           <table> <table>     <table>     <table>    <table>    
      1 ParameterFactor   0.10    0.7750000   1.5000000   5.775000    6.500000
      2 ParameterValue   -1.94   -0.6305000  -0.1455000  -0.560175   -0.075175
      3 Time              0.00  336.0000000 705.0000000 705.488798 1074.000000
      4 Concentration     0.00    0.1159193   0.4692815   3.482360    2.797140
      5 molWeight       225.21  225.2100000 225.2100000 225.210000  225.210000
        Max.      
        <table>   
      1   20.00000
      2   -0.00970
      3 1440.00000
      4   69.15068
      5  225.21000
      

---

    Code
      df2_ts
    Output
      $charColumnSummary
      # A tibble: 1 x 6
        OutputPath                                                               
        <chr>                                                                    
      1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
        ParameterPath                                                     Unit  
        <chr>                                                             <chr> 
      1 Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose µmol/l
        Dimension             TimeUnit TimeDimension
        <chr>                 <chr>    <chr>        
      1 Concentration (molar) min      Time         
      
      $numericColumnSummary
      # A tibble: 5 x 7
        column          Min.       `1st Qu.`    Median      Mean         `3rd Qu.`  
        <chr>           <table>    <table>      <table>     <table>      <table>    
      1 ParameterFactor   0.100000   0.77500000   1.5000000   5.77500000    6.500000
      2 ParameterValue    0.000025   0.00019375   0.0003750   0.00144375    0.001625
      3 Time              0.000000 336.00000000 705.0000000 705.48879837 1074.000000
      4 Concentration     0.000000   0.13811989   0.9816535  19.74123948    6.778747
      5 molWeight       225.210000 225.21000000 225.2100000 225.21000000  225.210000
        Max.    
        <table> 
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
        Unit   Dimension             TimeUnit TimeDimension
        <chr>  <chr>                 <chr>    <chr>        
      1 µmol/l Concentration (molar) min      Time         
      
      $numericColumnSummary
      # A tibble: 5 x 7
        column          Min.    `1st Qu.`    Median     Mean       `3rd Qu.`  
        <chr>           <table> <table>      <table>    <table>    <table>    
      1 ParameterFactor   0.10    0.77500000   1.500000   5.775000    6.500000
      2 ParameterValue    0.10    0.77500000   1.500000   5.775000    6.500000
      3 Time              0.00  336.00000000 705.000000 705.488798 1074.000000
      4 Concentration     0.00    0.08300163   0.450944   3.065299    2.758069
      5 molWeight       225.21  225.21000000 225.210000 225.210000  225.210000
        Max.      
        <table>   
      1   20.00000
      2   20.00000
      3 1440.00000
      4   51.26153
      5  225.21000
      

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
      [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
      
      $group
      [1] "ParameterFactor"
      
      $alt
      [1] ""
      

# sensitivitySpiderPlot plots are as expected

    Code
      pb$plot$labels
    Output
      $x
      [1] "Input parameter value [% of reference]"
      
      $y
      [1] "PK-parameter value [% of reference]"
      
      $group
      [1] "Parameter"
      
      $colour
      [1] "Parameter"
      
      $title
      [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
      
      $yintercept
      [1] "yintercept"
      
      $xintercept
      [1] "xintercept"
      
      $alt
      [1] ""
      

