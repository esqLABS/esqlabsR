# extendPopulationByUserDefinedParams works

    Code
      population$getParameterValuesForIndividual(4)
    Output
      $paths
       [1] "Organism|Weight"                                                          
       [2] "Organism|BMI"                                                             
       [3] "Organism|Gestational age"                                                 
       [4] "Organism|Age"                                                             
       [5] "Organism|Ontogeny factor (alpha1-acid glycoprotein)"                      
       [6] "Organism|Ontogeny factor (albumin)"                                       
       [7] "Organism|Height"                                                          
       [8] "Organism|Hematocrit"                                                      
       [9] "Organism|VenousBlood|Volume"                                              
      [10] "Organism|ArterialBlood|Volume"                                            
      [11] "Organism|Bone|Specific blood flow rate"                                   
      [12] "Organism|Bone|Volume"                                                     
      [13] "Organism|Brain|Volume"                                                    
      [14] "Organism|Brain|Specific blood flow rate"                                  
      [15] "Organism|Fat|Volume"                                                      
      [16] "Organism|Fat|Vf (neutral lipid)-PT"                                       
      [17] "Organism|Fat|Vf (lipid)"                                                  
      [18] "Organism|Fat|Vf (phospholipid)-PT"                                        
      [19] "Organism|Fat|Fraction interstitial"                                       
      [20] "Organism|Fat|Vf (water)"                                                  
      [21] "Organism|Fat|Vf (water)-PT"                                               
      [22] "Organism|Fat|Vf (neutral lipid)-RR"                                       
      [23] "Organism|Fat|Vf (neutral lipid)-WS"                                       
      [24] "Organism|Fat|Vf (neutral phospholipid)-RR"                                
      [25] "Organism|Fat|Vf (neutral phospholipid, plasma)-WS"                        
      [26] "Organism|Fat|Vf (extracellular water)-RR"                                 
      [27] "Organism|Fat|Vf (intracellular water)-RR"                                 
      [28] "Organism|Fat|Vf (water)-WS"                                               
      [29] "Organism|Fat|Specific blood flow rate"                                    
      [30] "Organism|Gonads|Volume"                                                   
      [31] "Organism|Gonads|Specific blood flow rate"                                 
      [32] "Organism|Heart|Volume"                                                    
      [33] "Organism|Heart|Specific blood flow rate"                                  
      [34] "Organism|Kidney|Volume"                                                   
      [35] "Organism|Kidney|Maximal decreasing rate factor"                           
      [36] "Organism|Kidney|TM50 for GFR"                                             
      [37] "Organism|Kidney|Age of aging onset"                                       
      [38] "Organism|Kidney|GFRmat"                                                   
      [39] "Organism|Kidney|Hill coefficient for GFR"                                 
      [40] "Organism|Kidney|Hill coefficient for aging GFR"                           
      [41] "Organism|Kidney|Aging half-time"                                          
      [42] "Organism|Kidney|Specific blood flow rate"                                 
      [43] "Organism|Lumen|Effective surface area variability factor"                 
      [44] "Organism|Lumen|Stomach|Distal radius"                                     
      [45] "Organism|Lumen|Stomach|Length"                                            
      [46] "Organism|Lumen|Stomach|GET_beta (Weibull function) variability factor"    
      [47] "Organism|Lumen|Stomach|Gastric emptying time"                             
      [48] "Organism|Lumen|Stomach|GET_alpha (Weibull function) variability factor"   
      [49] "Organism|Lumen|Stomach|Proximal radius"                                   
      [50] "Organism|Lumen|Duodenum|Effective surface area enhancement factor"        
      [51] "Organism|Lumen|UpperJejunum|Effective surface area enhancement factor"    
      [52] "Organism|Lumen|LowerJejunum|Effective surface area enhancement factor"    
      [53] "Organism|Lumen|UpperIleum|Effective surface area enhancement factor"      
      [54] "Organism|Lumen|LowerIleum|Effective surface area enhancement factor"      
      [55] "Organism|Lumen|Caecum|Effective surface area enhancement factor"          
      [56] "Organism|Lumen|ColonAscendens|Effective surface area enhancement factor"  
      [57] "Organism|Lumen|ColonTransversum|Effective surface area enhancement factor"
      [58] "Organism|Lumen|ColonDescendens|Effective surface area enhancement factor" 
      [59] "Organism|Lumen|ColonSigmoid|Effective surface area enhancement factor"    
      [60] "Organism|Lumen|Rectum|Effective surface area enhancement factor"          
      [61] "Organism|Stomach|Volume"                                                  
      [62] "Organism|Stomach|Specific blood flow rate"                                
      [63] "Organism|SmallIntestine|Small intestinal transit time"                    
      [64] "Organism|SmallIntestine|Volume"                                           
      [65] "Organism|SmallIntestine|Specific blood flow rate"                         
      [66] "Organism|LargeIntestine|Large intestinal transit time"                    
      [67] "Organism|LargeIntestine|Volume"                                           
      [68] "Organism|LargeIntestine|Specific blood flow rate"                         
      [69] "Organism|Liver|Volume"                                                    
      [70] "Organism|Liver|Specific blood flow rate"                                  
      [71] "Organism|Lung|Volume"                                                     
      [72] "Organism|Lung|Fraction vascular"                                          
      [73] "Organism|Muscle|Volume"                                                   
      [74] "Organism|Muscle|Vf (neutral lipid)-PT"                                    
      [75] "Organism|Muscle|Vf (lipid)"                                               
      [76] "Organism|Muscle|Vf (phospholipid)-PT"                                     
      [77] "Organism|Muscle|Vf (protein)"                                             
      [78] "Organism|Muscle|Vf (water)"                                               
      [79] "Organism|Muscle|Vf (water)-PT"                                            
      [80] "Organism|Muscle|Fraction interstitial"                                    
      [81] "Organism|Muscle|Vf (neutral lipid)-WS"                                    
      [82] "Organism|Muscle|Vf (neutral lipid)-RR"                                    
      [83] "Organism|Muscle|Vf (neutral phospholipid, plasma)-WS"                     
      [84] "Organism|Muscle|Vf (neutral phospholipid)-RR"                             
      [85] "Organism|Muscle|Vf (extracellular water)-RR"                              
      [86] "Organism|Muscle|Vf (protein)-WS"                                          
      [87] "Organism|Muscle|Vf (intracellular water)-RR"                              
      [88] "Organism|Muscle|Vf (water)-WS"                                            
      [89] "Organism|Muscle|Specific blood flow rate"                                 
      [90] "Organism|Pancreas|Volume"                                                 
      [91] "Organism|Pancreas|Specific blood flow rate"                               
      [92] "Organism|PortalVein|Volume"                                               
      [93] "Organism|Skin|Volume"                                                     
      [94] "Organism|Skin|Specific blood flow rate"                                   
      [95] "Organism|Spleen|Volume"                                                   
      [96] "Organism|Spleen|Specific blood flow rate"                                 
      [97] "Organism|Kidney|GFR"                                                      
      
      $values
       [1] 4.298250e+01 1.634378e-01 4.000000e+01 1.319338e+01 1.103233e+00
       [6] 8.542347e-01 1.621696e+01 4.300000e-01 6.127810e-01 2.826654e-01
      [11] 3.878819e-02 7.837624e+00 1.361570e+00 5.562814e-01 5.781605e+00
      [16] 7.173069e-01 7.263868e-01 1.815967e-03 2.236132e-01 2.236132e-01
      [21] 2.683359e-01 7.745099e-01 8.353448e-01 1.452774e-03 1.837758e-03
      [26] 2.012519e-01 1.341679e-02 4.472265e-02 3.232630e-02 1.193838e-02
      [31] 5.141249e-01 2.664711e-01 8.801586e-01 2.094086e-01 0.000000e+00
      [36] 4.378560e+01 3.000000e+01 1.589952e-01 1.490837e+01 1.500000e+00
      [41] 5.400000e+01 3.246394e+00 2.283943e+00 4.446641e-01 1.778643e+00
      [46] 1.116354e+00 2.617015e+01 4.802448e-01 4.446641e-01 2.926883e+02
      [51] 4.479877e+02 3.729358e+02 2.607527e+02 1.465650e+02 1.800000e+00
      [56] 2.500000e+00 2.500000e+00 2.500000e+00 2.500000e+00 3.560000e+00
      [61] 9.934962e-02 4.702104e-01 2.723064e+02 5.589256e-01 1.174030e+00
      [66] 2.505000e+03 2.861155e-01 8.214561e-01 1.352936e+00 2.598037e-01
      [71] 5.495688e-01 5.800000e-01 2.105661e+01 2.380000e-02 1.300000e-02
      [76] 7.200000e-03 1.770000e-01 8.110000e-01 7.600000e-01 2.016795e-01
      [81] 4.900000e-03 2.200000e-02 4.200000e-03 7.800000e-03 7.900000e-02
      [86] 1.900000e-01 6.660000e-01 7.600000e-01 3.178758e-02 1.492672e-01
      [91] 5.160901e-01 6.597635e-01 1.689268e+00 1.637035e-01 2.166255e-01
      [96] 9.584817e-01 1.204043e-01
      

# extendPopulationFromXLS works

    Code
      population$getParameterValuesForIndividual(4)
    Output
      $paths
       [1] "Organism|Weight"                                                          
       [2] "Organism|BMI"                                                             
       [3] "Organism|Gestational age"                                                 
       [4] "Organism|Age"                                                             
       [5] "Organism|Ontogeny factor (alpha1-acid glycoprotein)"                      
       [6] "Organism|Ontogeny factor (albumin)"                                       
       [7] "Organism|Height"                                                          
       [8] "Organism|Hematocrit"                                                      
       [9] "Organism|VenousBlood|Volume"                                              
      [10] "Organism|ArterialBlood|Volume"                                            
      [11] "Organism|Bone|Specific blood flow rate"                                   
      [12] "Organism|Bone|Volume"                                                     
      [13] "Organism|Brain|Volume"                                                    
      [14] "Organism|Brain|Specific blood flow rate"                                  
      [15] "Organism|Fat|Volume"                                                      
      [16] "Organism|Fat|Vf (neutral lipid)-PT"                                       
      [17] "Organism|Fat|Vf (lipid)"                                                  
      [18] "Organism|Fat|Vf (phospholipid)-PT"                                        
      [19] "Organism|Fat|Fraction interstitial"                                       
      [20] "Organism|Fat|Vf (water)"                                                  
      [21] "Organism|Fat|Vf (water)-PT"                                               
      [22] "Organism|Fat|Vf (neutral lipid)-RR"                                       
      [23] "Organism|Fat|Vf (neutral lipid)-WS"                                       
      [24] "Organism|Fat|Vf (neutral phospholipid)-RR"                                
      [25] "Organism|Fat|Vf (neutral phospholipid, plasma)-WS"                        
      [26] "Organism|Fat|Vf (extracellular water)-RR"                                 
      [27] "Organism|Fat|Vf (intracellular water)-RR"                                 
      [28] "Organism|Fat|Vf (water)-WS"                                               
      [29] "Organism|Fat|Specific blood flow rate"                                    
      [30] "Organism|Gonads|Volume"                                                   
      [31] "Organism|Gonads|Specific blood flow rate"                                 
      [32] "Organism|Heart|Volume"                                                    
      [33] "Organism|Heart|Specific blood flow rate"                                  
      [34] "Organism|Kidney|Volume"                                                   
      [35] "Organism|Kidney|Maximal decreasing rate factor"                           
      [36] "Organism|Kidney|TM50 for GFR"                                             
      [37] "Organism|Kidney|Age of aging onset"                                       
      [38] "Organism|Kidney|GFRmat"                                                   
      [39] "Organism|Kidney|Hill coefficient for GFR"                                 
      [40] "Organism|Kidney|Hill coefficient for aging GFR"                           
      [41] "Organism|Kidney|Aging half-time"                                          
      [42] "Organism|Kidney|Specific blood flow rate"                                 
      [43] "Organism|Lumen|Effective surface area variability factor"                 
      [44] "Organism|Lumen|Stomach|Distal radius"                                     
      [45] "Organism|Lumen|Stomach|Length"                                            
      [46] "Organism|Lumen|Stomach|GET_beta (Weibull function) variability factor"    
      [47] "Organism|Lumen|Stomach|Gastric emptying time"                             
      [48] "Organism|Lumen|Stomach|GET_alpha (Weibull function) variability factor"   
      [49] "Organism|Lumen|Stomach|Proximal radius"                                   
      [50] "Organism|Lumen|Duodenum|Effective surface area enhancement factor"        
      [51] "Organism|Lumen|UpperJejunum|Effective surface area enhancement factor"    
      [52] "Organism|Lumen|LowerJejunum|Effective surface area enhancement factor"    
      [53] "Organism|Lumen|UpperIleum|Effective surface area enhancement factor"      
      [54] "Organism|Lumen|LowerIleum|Effective surface area enhancement factor"      
      [55] "Organism|Lumen|Caecum|Effective surface area enhancement factor"          
      [56] "Organism|Lumen|ColonAscendens|Effective surface area enhancement factor"  
      [57] "Organism|Lumen|ColonTransversum|Effective surface area enhancement factor"
      [58] "Organism|Lumen|ColonDescendens|Effective surface area enhancement factor" 
      [59] "Organism|Lumen|ColonSigmoid|Effective surface area enhancement factor"    
      [60] "Organism|Lumen|Rectum|Effective surface area enhancement factor"          
      [61] "Organism|Stomach|Volume"                                                  
      [62] "Organism|Stomach|Specific blood flow rate"                                
      [63] "Organism|SmallIntestine|Small intestinal transit time"                    
      [64] "Organism|SmallIntestine|Volume"                                           
      [65] "Organism|SmallIntestine|Specific blood flow rate"                         
      [66] "Organism|LargeIntestine|Large intestinal transit time"                    
      [67] "Organism|LargeIntestine|Volume"                                           
      [68] "Organism|LargeIntestine|Specific blood flow rate"                         
      [69] "Organism|Liver|Volume"                                                    
      [70] "Organism|Liver|Specific blood flow rate"                                  
      [71] "Organism|Lung|Volume"                                                     
      [72] "Organism|Lung|Fraction vascular"                                          
      [73] "Organism|Muscle|Volume"                                                   
      [74] "Organism|Muscle|Vf (neutral lipid)-PT"                                    
      [75] "Organism|Muscle|Vf (lipid)"                                               
      [76] "Organism|Muscle|Vf (phospholipid)-PT"                                     
      [77] "Organism|Muscle|Vf (protein)"                                             
      [78] "Organism|Muscle|Vf (water)"                                               
      [79] "Organism|Muscle|Vf (water)-PT"                                            
      [80] "Organism|Muscle|Fraction interstitial"                                    
      [81] "Organism|Muscle|Vf (neutral lipid)-WS"                                    
      [82] "Organism|Muscle|Vf (neutral lipid)-RR"                                    
      [83] "Organism|Muscle|Vf (neutral phospholipid, plasma)-WS"                     
      [84] "Organism|Muscle|Vf (neutral phospholipid)-RR"                             
      [85] "Organism|Muscle|Vf (extracellular water)-RR"                              
      [86] "Organism|Muscle|Vf (protein)-WS"                                          
      [87] "Organism|Muscle|Vf (intracellular water)-RR"                              
      [88] "Organism|Muscle|Vf (water)-WS"                                            
      [89] "Organism|Muscle|Specific blood flow rate"                                 
      [90] "Organism|Pancreas|Volume"                                                 
      [91] "Organism|Pancreas|Specific blood flow rate"                               
      [92] "Organism|PortalVein|Volume"                                               
      [93] "Organism|Skin|Volume"                                                     
      [94] "Organism|Skin|Specific blood flow rate"                                   
      [95] "Organism|Spleen|Volume"                                                   
      [96] "Organism|Spleen|Specific blood flow rate"                                 
      [97] "Organism|Kidney|GFR"                                                      
      [98] "Organism|Kidney|eGFR"                                                     
      
      $values
       [1] 4.298250e+01 1.634378e-01 4.000000e+01 1.319338e+01 1.103233e+00
       [6] 8.542347e-01 1.621696e+01 4.300000e-01 6.127810e-01 2.826654e-01
      [11] 3.878819e-02 7.837624e+00 1.361570e+00 5.562814e-01 5.781605e+00
      [16] 7.173069e-01 7.263868e-01 1.815967e-03 2.236132e-01 2.236132e-01
      [21] 2.683359e-01 7.745099e-01 8.353448e-01 1.452774e-03 1.837758e-03
      [26] 2.012519e-01 1.341679e-02 4.472265e-02 3.232630e-02 1.193838e-02
      [31] 5.141249e-01 2.664711e-01 8.801586e-01 2.094086e-01 0.000000e+00
      [36] 4.378560e+01 3.000000e+01 1.589952e-01 1.490837e+01 1.500000e+00
      [41] 5.400000e+01 3.246394e+00 2.283943e+00 4.446641e-01 1.778643e+00
      [46] 1.116354e+00 2.617015e+01 4.802448e-01 4.446641e-01 2.926883e+02
      [51] 4.479877e+02 3.729358e+02 2.607527e+02 1.465650e+02 1.800000e+00
      [56] 2.500000e+00 2.500000e+00 2.500000e+00 2.500000e+00 3.560000e+00
      [61] 9.934962e-02 4.702104e-01 2.723064e+02 5.589256e-01 1.174030e+00
      [66] 2.505000e+03 2.861155e-01 8.214561e-01 1.352936e+00 2.598037e-01
      [71] 5.495688e-01 5.800000e-01 2.105661e+01 2.380000e-02 1.300000e-02
      [76] 7.200000e-03 1.770000e-01 8.110000e-01 7.600000e-01 2.016795e-01
      [81] 4.900000e-03 2.200000e-02 4.200000e-03 7.800000e-03 7.900000e-02
      [86] 1.900000e-01 6.660000e-01 7.600000e-01 3.178758e-02 1.492672e-01
      [91] 5.160901e-01 6.597635e-01 1.689268e+00 1.637035e-01 2.166255e-01
      [96] 9.584817e-01 1.204043e-01 1.198667e-01
      

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

