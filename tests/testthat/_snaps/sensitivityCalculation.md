# Check sensitivityCalculation dataframes and plots are as expected

    Code
      str(results$tsData)
    Output
      tibble [27,987 x 11] (S3: tbl_df/tbl/data.frame)
       $ OutputPath     : chr [1:27987] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" ...
       $ ParameterFactor: num [1:27987] 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
       $ ParameterPath  : chr [1:27987] "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" ...
       $ ParameterValue : num [1:27987] 2.5e-05 2.5e-05 2.5e-05 2.5e-05 2.5e-05 2.5e-05 2.5e-05 2.5e-05 2.5e-05 2.5e-05 ...
       $ Time           : num [1:27987] 0 1 2 3 4 5 6 7 8 9 ...
       $ Concentration  : num [1:27987] 0 0.325 0.91 1.502 2.073 ...
       $ Unit           : chr [1:27987] "µmol/l" "µmol/l" "µmol/l" "µmol/l" ...
       $ Dimension      : chr [1:27987] "Concentration (molar)" "Concentration (molar)" "Concentration (molar)" "Concentration (molar)" ...
       $ TimeUnit       : chr [1:27987] "min" "min" "min" "min" ...
       $ TimeDimension  : chr [1:27987] "Time" "Time" "Time" "Time" ...
       $ molWeight      : num [1:27987] 225 225 225 225 225 ...

---

    Code
      summary(results$tsData)
    Output
        OutputPath        ParameterFactor  ParameterPath      ParameterValue    
       Length:27987       Min.   : 0.100   Length:27987       Min.   :-0.97000  
       Class :character   1st Qu.: 0.500   Class :character   1st Qu.:-0.04850  
       Mode  :character   Median : 1.000   Mode  :character   Median : 0.00025  
                          Mean   : 3.132                      Mean   : 0.94287  
                          3rd Qu.: 6.000                      3rd Qu.: 0.50000  
                          Max.   :10.000                      Max.   :10.00000  
            Time        Concentration          Unit            Dimension        
       Min.   :   0.0   Min.   :  0.0000   Length:27987       Length:27987      
       1st Qu.: 336.0   1st Qu.:  0.1296   Class :character   Class :character  
       Median : 705.0   Median :  0.6745   Mode  :character   Mode  :character  
       Mean   : 705.5   Mean   :  5.7565                                        
       3rd Qu.:1074.0   3rd Qu.:  3.6232                                        
       Max.   :1440.0   Max.   :502.5271                                        
         TimeUnit         TimeDimension        molWeight    
       Length:27987       Length:27987       Min.   :225.2  
       Class :character   Class :character   1st Qu.:225.2  
       Mode  :character   Mode  :character   Median :225.2  
                                             Mean   :225.2  
                                             3rd Qu.:225.2  
                                             Max.   :225.2  

---

    Code
      str(results$pkData)
    Output
      tibble [171 x 9] (S3: tbl_df/tbl/data.frame)
       $ OutputPath            : chr [1:171] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" ...
       $ ParameterPath         : chr [1:171] "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" ...
       $ ParameterFactor       : num [1:171] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1 ...
       $ ParameterValue        : num [1:171] 0.000025 0.00005 0.000075 0.0001 0.000125 0.00015 0.000175 0.0002 0.000225 0.00025 ...
       $ PKParameter           : chr [1:171] "AUC_inf" "AUC_inf" "AUC_inf" "AUC_inf" ...
       $ PKParameterValue      : num [1:171] 407 815 1222 1629 2036 ...
       $ Unit                  : chr [1:171] "µmol*min/l" "µmol*min/l" "µmol*min/l" "µmol*min/l" ...
       $ PercentChangePK       : num [1:171] -90 -80 -70 -60 -50 ...
       $ SensitivityPKParameter: num [1:171] 0.1 0.2 0.3 0.4 0.5 ...

---

    Code
      summary(results$pkData)
    Output
        OutputPath        ParameterPath      ParameterFactor  ParameterValue    
       Length:171         Length:171         Min.   : 0.100   Min.   :-0.97000  
       Class :character   Class :character   1st Qu.: 0.500   1st Qu.:-0.04850  
       Mode  :character   Mode  :character   Median : 1.000   Median : 0.00025  
                                             Mean   : 3.132   Mean   : 0.94287  
                                             3rd Qu.: 6.000   3rd Qu.: 0.50000  
                                             Max.   :10.000   Max.   :10.00000  
                                                                                
       PKParameter        PKParameterValue       Unit           PercentChangePK   
       Length:171         Min.   :    0.18   Length:171         Min.   :-90.0001  
       Class :character   1st Qu.:    0.18   Class :character   1st Qu.: -0.7339  
       Mode  :character   Median :   50.25   Mode  :character   Median :  0.0000  
                          Mean   : 2304.06                      Mean   : 46.4676  
                          3rd Qu.: 3139.37                      3rd Qu.:  1.4960  
                          Max.   :40726.29                      Max.   :900.0000  
                                                                                  
       SensitivityPKParameter
       Min.   :-0.4795       
       1st Qu.: 0.0000       
       Median : 0.0000       
       Mean   : 0.7011       
       3rd Qu.: 0.1043       
       Max.   :10.0000       
       NA's   :9             

---

    Code
      unique(results$tsData$ParameterPath)
    Output
      [1] "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"                  
      [2] "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
      [3] "Aciclovir|Lipophilicity"                                                            

