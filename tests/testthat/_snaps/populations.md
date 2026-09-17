# extendPopulationByUserDefinedParams works

    Code
      population$getParameterValuesForIndividual(4)
    Output
      $paths
        [1] "Organism|Weight"                                                          
        [2] "Organism|Ontogeny factor (albumin)"                                       
        [3] "Organism|Ontogeny factor (alpha1-acid glycoprotein)"                      
        [4] "Organism|BMI"                                                             
        [5] "Organism|BSA"                                                             
        [6] "Organism|Age"                                                             
        [7] "Organism|Gestational age"                                                 
        [8] "Organism|Height"                                                          
        [9] "Organism|Hematocrit"                                                      
       [10] "Organism|VenousBlood|Volume"                                              
       [11] "Organism|ArterialBlood|Volume"                                            
       [12] "Organism|Bone|Specific blood flow rate"                                   
       [13] "Organism|Bone|Volume"                                                     
       [14] "Organism|Brain|Volume"                                                    
       [15] "Organism|Brain|Specific blood flow rate"                                  
       [16] "Organism|Fat|Volume"                                                      
       [17] "Organism|Fat|Vf (lipid)"                                                  
       [18] "Organism|Fat|Vf (neutral lipid)-PT"                                       
       [19] "Organism|Fat|Vf (phospholipid)-PT"                                        
       [20] "Organism|Fat|Fraction interstitial"                                       
       [21] "Organism|Fat|Vf (water)"                                                  
       [22] "Organism|Fat|Vf (water)-PT"                                               
       [23] "Organism|Fat|Vf (neutral lipid)-RR"                                       
       [24] "Organism|Fat|Vf (neutral lipid)-WS"                                       
       [25] "Organism|Fat|Vf (neutral phospholipid)-RR"                                
       [26] "Organism|Fat|Vf (neutral phospholipid, plasma)-WS"                        
       [27] "Organism|Fat|Vf (extracellular water)-RR"                                 
       [28] "Organism|Fat|Vf (intracellular water)-RR"                                 
       [29] "Organism|Fat|Vf (water)-WS"                                               
       [30] "Organism|Fat|Specific blood flow rate"                                    
       [31] "Organism|Gonads|Volume"                                                   
       [32] "Organism|Gonads|Specific blood flow rate"                                 
       [33] "Organism|Heart|Volume"                                                    
       [34] "Organism|Heart|Specific blood flow rate"                                  
       [35] "Organism|Kidney|Volume"                                                   
       [36] "Organism|Kidney|Age of aging onset"                                       
       [37] "Organism|Kidney|Aging half-time"                                          
       [38] "Organism|Kidney|GFRmat"                                                   
       [39] "Organism|Kidney|Hill coefficient for aging GFR"                           
       [40] "Organism|Kidney|Hill coefficient for GFR"                                 
       [41] "Organism|Kidney|Maximal decreasing rate factor"                           
       [42] "Organism|Kidney|TM50 for GFR"                                             
       [43] "Organism|Kidney|Specific blood flow rate"                                 
       [44] "Organism|Lumen|Effective surface area variability factor"                 
       [45] "Organism|Lumen|Stomach|Distal radius"                                     
       [46] "Organism|Lumen|Stomach|Gastric emptying time"                             
       [47] "Organism|Lumen|Stomach|GET_alpha (Weibull function) variability factor"   
       [48] "Organism|Lumen|Stomach|GET_beta (Weibull function) variability factor"    
       [49] "Organism|Lumen|Stomach|Length"                                            
       [50] "Organism|Lumen|Stomach|Proximal radius"                                   
       [51] "Organism|Lumen|Stomach|Basal pH in fasted state"                          
       [52] "Organism|Lumen|Stomach|Basal Bile Salt concentration"                     
       [53] "Organism|Lumen|Duodenum|Effective surface area enhancement factor"        
       [54] "Organism|Lumen|Duodenum|pH in fasted state"                               
       [55] "Organism|Lumen|Duodenum|Bile Salt concentration in fasted state"          
       [56] "Organism|Lumen|Duodenum|Bile Salt concentration after meal"               
       [57] "Organism|Lumen|UpperJejunum|Effective surface area enhancement factor"    
       [58] "Organism|Lumen|UpperJejunum|pH in fasted state"                           
       [59] "Organism|Lumen|UpperJejunum|Bile Salt concentration in fasted state"      
       [60] "Organism|Lumen|UpperJejunum|Bile Salt concentration after meal"           
       [61] "Organism|Lumen|LowerJejunum|Effective surface area enhancement factor"    
       [62] "Organism|Lumen|LowerJejunum|pH"                                           
       [63] "Organism|Lumen|LowerJejunum|Bile Salt concentration"                      
       [64] "Organism|Lumen|UpperIleum|Effective surface area enhancement factor"      
       [65] "Organism|Lumen|UpperIleum|pH"                                             
       [66] "Organism|Lumen|UpperIleum|Bile Salt concentration"                        
       [67] "Organism|Lumen|LowerIleum|Effective surface area enhancement factor"      
       [68] "Organism|Lumen|LowerIleum|pH"                                             
       [69] "Organism|Lumen|LowerIleum|Bile Salt concentration"                        
       [70] "Organism|Lumen|Caecum|Effective surface area enhancement factor"          
       [71] "Organism|Lumen|Caecum|pH"                                                 
       [72] "Organism|Lumen|Caecum|Bile Salt concentration"                            
       [73] "Organism|Lumen|ColonAscendens|Effective surface area enhancement factor"  
       [74] "Organism|Lumen|ColonAscendens|pH"                                         
       [75] "Organism|Lumen|ColonAscendens|Bile Salt concentration"                    
       [76] "Organism|Lumen|ColonTransversum|Effective surface area enhancement factor"
       [77] "Organism|Lumen|ColonTransversum|pH"                                       
       [78] "Organism|Lumen|ColonDescendens|Effective surface area enhancement factor" 
       [79] "Organism|Lumen|ColonDescendens|pH"                                        
       [80] "Organism|Lumen|ColonSigmoid|Effective surface area enhancement factor"    
       [81] "Organism|Lumen|ColonSigmoid|pH"                                           
       [82] "Organism|Lumen|Rectum|Effective surface area enhancement factor"          
       [83] "Organism|Stomach|Volume"                                                  
       [84] "Organism|Stomach|Specific blood flow rate"                                
       [85] "Organism|SmallIntestine|Small intestinal transit time"                    
       [86] "Organism|SmallIntestine|Volume"                                           
       [87] "Organism|SmallIntestine|Specific blood flow rate"                         
       [88] "Organism|LargeIntestine|Large intestinal transit time"                    
       [89] "Organism|LargeIntestine|Volume"                                           
       [90] "Organism|LargeIntestine|Specific blood flow rate"                         
       [91] "Organism|Liver|Volume"                                                    
       [92] "Organism|Liver|Specific blood flow rate"                                  
       [93] "Organism|Lung|Volume"                                                     
       [94] "Organism|Lung|Fraction vascular"                                          
       [95] "Organism|Muscle|Volume"                                                   
       [96] "Organism|Muscle|Vf (lipid)"                                               
       [97] "Organism|Muscle|Vf (neutral lipid)-PT"                                    
       [98] "Organism|Muscle|Vf (phospholipid)-PT"                                     
       [99] "Organism|Muscle|Vf (protein)"                                             
      [100] "Organism|Muscle|Vf (water)"                                               
      [101] "Organism|Muscle|Vf (water)-PT"                                            
      [102] "Organism|Muscle|Fraction interstitial"                                    
      [103] "Organism|Muscle|Vf (neutral lipid)-RR"                                    
      [104] "Organism|Muscle|Vf (neutral lipid)-WS"                                    
      [105] "Organism|Muscle|Vf (neutral phospholipid)-RR"                             
      [106] "Organism|Muscle|Vf (neutral phospholipid, plasma)-WS"                     
      [107] "Organism|Muscle|Vf (extracellular water)-RR"                              
      [108] "Organism|Muscle|Vf (protein)-WS"                                          
      [109] "Organism|Muscle|Vf (intracellular water)-RR"                              
      [110] "Organism|Muscle|Vf (water)-WS"                                            
      [111] "Organism|Muscle|Specific blood flow rate"                                 
      [112] "Organism|Pancreas|Volume"                                                 
      [113] "Organism|Pancreas|Specific blood flow rate"                               
      [114] "Organism|PortalVein|Volume"                                               
      [115] "Organism|Skin|Volume"                                                     
      [116] "Organism|Skin|Specific blood flow rate"                                   
      [117] "Organism|Spleen|Volume"                                                   
      [118] "Organism|Spleen|Specific blood flow rate"                                 
      [119] "Organism|Kidney|GFR"                                                      
      
      $values
        [1] 1.060249e+02 8.142692e-01 1.773703e+00 2.993963e-01 2.354246e+02
        [6] 3.947424e+01 4.000000e+01 1.881832e+01 4.700000e-01 1.090007e+00
       [11] 4.425107e-01 2.788921e-02 1.377482e+01 1.452978e+00 5.044729e-01
       [16] 3.634885e+01 8.000000e-01 7.900000e-01 2.000000e-03 1.600000e-01
       [21] 1.500000e-01 1.800000e-01 8.530000e-01 9.200000e-01 1.600000e-03
       [26] 2.024000e-03 1.350000e-01 9.000000e-03 3.000000e-02 2.299644e-02
       [31] 4.242101e-02 8.299912e-02 4.257524e-01 6.606609e-01 5.599911e-01
       [36] 3.000000e+01 5.400000e+01 1.127481e-01 1.500000e+00 1.489588e+01
       [41] 9.000000e-01 4.422942e+01 2.622816e+00 1.374382e+00 5.000000e-01
       [46] 1.803728e+01 9.009122e-01 1.080061e+00 2.000000e+00 5.000000e-01
       [51] 1.806734e+00 1.458303e+02 2.926883e+02 7.367990e+00 2.669178e+03
       [56] 2.466168e+03 4.479877e+02 7.940346e+00 5.749614e+03 1.498223e+04
       [61] 3.729358e+02 6.567270e+00 4.715565e+03 2.607527e+02 8.190803e+00
       [66] 1.730926e+01 1.465650e+02 8.237844e+00 2.006806e+02 1.800000e+00
       [71] 7.410891e+00 1.898760e+02 2.500000e+00 7.806209e+00 7.855504e+01
       [76] 2.500000e+00 6.842263e+00 2.500000e+00 7.513535e+00 2.500000e+00
       [81] 5.509115e+00 3.560000e+00 1.981540e-01 4.068771e-01 1.758188e+02
       [86] 6.566184e-01 8.635180e-01 2.652000e+03 4.312771e-01 5.634231e-01
       [91] 2.631492e+00 1.900569e-01 9.976178e-01 5.800000e-01 4.084299e+01
       [96] 1.300000e-02 2.380000e-02 7.200000e-03 1.770000e-01 8.110000e-01
      [101] 7.600000e-01 1.600000e-01 2.200000e-02 4.900000e-03 7.800000e-03
      [106] 4.200000e-03 7.900000e-02 1.900000e-01 6.660000e-01 7.600000e-01
      [111] 3.364636e-02 1.447809e-01 3.323722e-01 9.808403e-01 4.912082e+00
      [116] 7.865499e-02 9.173923e-02 8.978489e-01 1.204043e-01
      

# extendPopulationFromXLS works

    Code
      population$getParameterValuesForIndividual(4)
    Output
      $paths
        [1] "Organism|Weight"                                                          
        [2] "Organism|Ontogeny factor (albumin)"                                       
        [3] "Organism|Ontogeny factor (alpha1-acid glycoprotein)"                      
        [4] "Organism|BMI"                                                             
        [5] "Organism|BSA"                                                             
        [6] "Organism|Age"                                                             
        [7] "Organism|Gestational age"                                                 
        [8] "Organism|Height"                                                          
        [9] "Organism|Hematocrit"                                                      
       [10] "Organism|VenousBlood|Volume"                                              
       [11] "Organism|ArterialBlood|Volume"                                            
       [12] "Organism|Bone|Specific blood flow rate"                                   
       [13] "Organism|Bone|Volume"                                                     
       [14] "Organism|Brain|Volume"                                                    
       [15] "Organism|Brain|Specific blood flow rate"                                  
       [16] "Organism|Fat|Volume"                                                      
       [17] "Organism|Fat|Vf (lipid)"                                                  
       [18] "Organism|Fat|Vf (neutral lipid)-PT"                                       
       [19] "Organism|Fat|Vf (phospholipid)-PT"                                        
       [20] "Organism|Fat|Fraction interstitial"                                       
       [21] "Organism|Fat|Vf (water)"                                                  
       [22] "Organism|Fat|Vf (water)-PT"                                               
       [23] "Organism|Fat|Vf (neutral lipid)-RR"                                       
       [24] "Organism|Fat|Vf (neutral lipid)-WS"                                       
       [25] "Organism|Fat|Vf (neutral phospholipid)-RR"                                
       [26] "Organism|Fat|Vf (neutral phospholipid, plasma)-WS"                        
       [27] "Organism|Fat|Vf (extracellular water)-RR"                                 
       [28] "Organism|Fat|Vf (intracellular water)-RR"                                 
       [29] "Organism|Fat|Vf (water)-WS"                                               
       [30] "Organism|Fat|Specific blood flow rate"                                    
       [31] "Organism|Gonads|Volume"                                                   
       [32] "Organism|Gonads|Specific blood flow rate"                                 
       [33] "Organism|Heart|Volume"                                                    
       [34] "Organism|Heart|Specific blood flow rate"                                  
       [35] "Organism|Kidney|Volume"                                                   
       [36] "Organism|Kidney|Age of aging onset"                                       
       [37] "Organism|Kidney|Aging half-time"                                          
       [38] "Organism|Kidney|GFRmat"                                                   
       [39] "Organism|Kidney|Hill coefficient for aging GFR"                           
       [40] "Organism|Kidney|Hill coefficient for GFR"                                 
       [41] "Organism|Kidney|Maximal decreasing rate factor"                           
       [42] "Organism|Kidney|TM50 for GFR"                                             
       [43] "Organism|Kidney|Specific blood flow rate"                                 
       [44] "Organism|Lumen|Effective surface area variability factor"                 
       [45] "Organism|Lumen|Stomach|Distal radius"                                     
       [46] "Organism|Lumen|Stomach|Gastric emptying time"                             
       [47] "Organism|Lumen|Stomach|GET_alpha (Weibull function) variability factor"   
       [48] "Organism|Lumen|Stomach|GET_beta (Weibull function) variability factor"    
       [49] "Organism|Lumen|Stomach|Length"                                            
       [50] "Organism|Lumen|Stomach|Proximal radius"                                   
       [51] "Organism|Lumen|Stomach|Basal pH in fasted state"                          
       [52] "Organism|Lumen|Stomach|Basal Bile Salt concentration"                     
       [53] "Organism|Lumen|Duodenum|Effective surface area enhancement factor"        
       [54] "Organism|Lumen|Duodenum|pH in fasted state"                               
       [55] "Organism|Lumen|Duodenum|Bile Salt concentration in fasted state"          
       [56] "Organism|Lumen|Duodenum|Bile Salt concentration after meal"               
       [57] "Organism|Lumen|UpperJejunum|Effective surface area enhancement factor"    
       [58] "Organism|Lumen|UpperJejunum|pH in fasted state"                           
       [59] "Organism|Lumen|UpperJejunum|Bile Salt concentration in fasted state"      
       [60] "Organism|Lumen|UpperJejunum|Bile Salt concentration after meal"           
       [61] "Organism|Lumen|LowerJejunum|Effective surface area enhancement factor"    
       [62] "Organism|Lumen|LowerJejunum|pH"                                           
       [63] "Organism|Lumen|LowerJejunum|Bile Salt concentration"                      
       [64] "Organism|Lumen|UpperIleum|Effective surface area enhancement factor"      
       [65] "Organism|Lumen|UpperIleum|pH"                                             
       [66] "Organism|Lumen|UpperIleum|Bile Salt concentration"                        
       [67] "Organism|Lumen|LowerIleum|Effective surface area enhancement factor"      
       [68] "Organism|Lumen|LowerIleum|pH"                                             
       [69] "Organism|Lumen|LowerIleum|Bile Salt concentration"                        
       [70] "Organism|Lumen|Caecum|Effective surface area enhancement factor"          
       [71] "Organism|Lumen|Caecum|pH"                                                 
       [72] "Organism|Lumen|Caecum|Bile Salt concentration"                            
       [73] "Organism|Lumen|ColonAscendens|Effective surface area enhancement factor"  
       [74] "Organism|Lumen|ColonAscendens|pH"                                         
       [75] "Organism|Lumen|ColonAscendens|Bile Salt concentration"                    
       [76] "Organism|Lumen|ColonTransversum|Effective surface area enhancement factor"
       [77] "Organism|Lumen|ColonTransversum|pH"                                       
       [78] "Organism|Lumen|ColonDescendens|Effective surface area enhancement factor" 
       [79] "Organism|Lumen|ColonDescendens|pH"                                        
       [80] "Organism|Lumen|ColonSigmoid|Effective surface area enhancement factor"    
       [81] "Organism|Lumen|ColonSigmoid|pH"                                           
       [82] "Organism|Lumen|Rectum|Effective surface area enhancement factor"          
       [83] "Organism|Stomach|Volume"                                                  
       [84] "Organism|Stomach|Specific blood flow rate"                                
       [85] "Organism|SmallIntestine|Small intestinal transit time"                    
       [86] "Organism|SmallIntestine|Volume"                                           
       [87] "Organism|SmallIntestine|Specific blood flow rate"                         
       [88] "Organism|LargeIntestine|Large intestinal transit time"                    
       [89] "Organism|LargeIntestine|Volume"                                           
       [90] "Organism|LargeIntestine|Specific blood flow rate"                         
       [91] "Organism|Liver|Volume"                                                    
       [92] "Organism|Liver|Specific blood flow rate"                                  
       [93] "Organism|Lung|Volume"                                                     
       [94] "Organism|Lung|Fraction vascular"                                          
       [95] "Organism|Muscle|Volume"                                                   
       [96] "Organism|Muscle|Vf (lipid)"                                               
       [97] "Organism|Muscle|Vf (neutral lipid)-PT"                                    
       [98] "Organism|Muscle|Vf (phospholipid)-PT"                                     
       [99] "Organism|Muscle|Vf (protein)"                                             
      [100] "Organism|Muscle|Vf (water)"                                               
      [101] "Organism|Muscle|Vf (water)-PT"                                            
      [102] "Organism|Muscle|Fraction interstitial"                                    
      [103] "Organism|Muscle|Vf (neutral lipid)-RR"                                    
      [104] "Organism|Muscle|Vf (neutral lipid)-WS"                                    
      [105] "Organism|Muscle|Vf (neutral phospholipid)-RR"                             
      [106] "Organism|Muscle|Vf (neutral phospholipid, plasma)-WS"                     
      [107] "Organism|Muscle|Vf (extracellular water)-RR"                              
      [108] "Organism|Muscle|Vf (protein)-WS"                                          
      [109] "Organism|Muscle|Vf (intracellular water)-RR"                              
      [110] "Organism|Muscle|Vf (water)-WS"                                            
      [111] "Organism|Muscle|Specific blood flow rate"                                 
      [112] "Organism|Pancreas|Volume"                                                 
      [113] "Organism|Pancreas|Specific blood flow rate"                               
      [114] "Organism|PortalVein|Volume"                                               
      [115] "Organism|Skin|Volume"                                                     
      [116] "Organism|Skin|Specific blood flow rate"                                   
      [117] "Organism|Spleen|Volume"                                                   
      [118] "Organism|Spleen|Specific blood flow rate"                                 
      [119] "Organism|Kidney|GFR"                                                      
      [120] "Organism|Kidney|eGFR"                                                     
      
      $values
        [1] 1.060249e+02 8.142692e-01 1.773703e+00 2.993963e-01 2.354246e+02
        [6] 3.947424e+01 4.000000e+01 1.881832e+01 4.700000e-01 1.090007e+00
       [11] 4.425107e-01 2.788921e-02 1.377482e+01 1.452978e+00 5.044729e-01
       [16] 3.634885e+01 8.000000e-01 7.900000e-01 2.000000e-03 1.600000e-01
       [21] 1.500000e-01 1.800000e-01 8.530000e-01 9.200000e-01 1.600000e-03
       [26] 2.024000e-03 1.350000e-01 9.000000e-03 3.000000e-02 2.299644e-02
       [31] 4.242101e-02 8.299912e-02 4.257524e-01 6.606609e-01 5.599911e-01
       [36] 3.000000e+01 5.400000e+01 1.127481e-01 1.500000e+00 1.489588e+01
       [41] 9.000000e-01 4.422942e+01 2.622816e+00 1.374382e+00 5.000000e-01
       [46] 1.803728e+01 9.009122e-01 1.080061e+00 2.000000e+00 5.000000e-01
       [51] 1.806734e+00 1.458303e+02 2.926883e+02 7.367990e+00 2.669178e+03
       [56] 2.466168e+03 4.479877e+02 7.940346e+00 5.749614e+03 1.498223e+04
       [61] 3.729358e+02 6.567270e+00 4.715565e+03 2.607527e+02 8.190803e+00
       [66] 1.730926e+01 1.465650e+02 8.237844e+00 2.006806e+02 1.800000e+00
       [71] 7.410891e+00 1.898760e+02 2.500000e+00 7.806209e+00 7.855504e+01
       [76] 2.500000e+00 6.842263e+00 2.500000e+00 7.513535e+00 2.500000e+00
       [81] 5.509115e+00 3.560000e+00 1.981540e-01 4.068771e-01 1.758188e+02
       [86] 6.566184e-01 8.635180e-01 2.652000e+03 4.312771e-01 5.634231e-01
       [91] 2.631492e+00 1.900569e-01 9.976178e-01 5.800000e-01 4.084299e+01
       [96] 1.300000e-02 2.380000e-02 7.200000e-03 1.770000e-01 8.110000e-01
      [101] 7.600000e-01 1.600000e-01 2.200000e-02 4.900000e-03 7.800000e-03
      [106] 4.200000e-03 7.900000e-02 1.900000e-01 6.660000e-01 7.600000e-01
      [111] 3.364636e-02 1.447809e-01 3.323722e-01 9.808403e-01 4.912082e+00
      [116] 7.865499e-02 9.173923e-02 8.978489e-01 1.204043e-01 1.198667e-01
      

# extendPopulationFromXLS throws an error if specified sheet is empty or data is missing

    Code
      extendPopulationFromXLS(population, PopulationParameters, sheet = "UserDefinedVariability")
    Condition
      Warning:
      x The specified excel sheet contains uncomplete row(s) i Using only complete rows to define population parameters
      Error in `extendPopulationFromXLS()`:
      ! x The specified excel sheet does not contain any complete row * Please fill all the columns and try again.

# proteinOntogenies refuses a value it cannot store, at the call

    Code
      addPopulation(project, "onto_r6", species = "Human", numberOfIndividuals = 5,
        proteinOntogenies = onto)
    Condition
      Error in `addPopulation()`:
      ! `proteinOntogenies` must be a character vector of "Protein:Ontogeny" entries. x It is a <MoleculeOntogeny> object. i One entry per ontogeny, e.g. `c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")`, or the same pairs as one comma-joined string.

---

    Code
      setPopulation(project, "testpopulation", proteinOntogenies = list(onto))
    Condition
      Error in `setPopulation()`:
      ! `proteinOntogenies` must be a character vector of "Protein:Ontogeny" entries. x It is a <MoleculeOntogeny> object. i One entry per ontogeny, e.g. `c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")`, or the same pairs as one comma-joined string.

---

    Code
      setPopulation(project, "testpopulation", proteinOntogenies = c("CYP3A4:CYP3A4",
        NA))
    Condition
      Error in `setPopulation()`:
      ! `proteinOntogenies` must be a character vector of "Protein:Ontogeny" entries. x It is a character vector with a missing entry. i One entry per ontogeny, e.g. `c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")`, or the same pairs as one comma-joined string.

# setPopulation aborts on a non-existent population

    Code
      setPopulation(project, "Ghost", numberOfIndividuals = 10)
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `setPopulation()`:
      ! Cannot modify population "ghost": it does not exist.
      i Use `addPopulation()` to create it first.

# setPopulation rejects a non-positive numberOfIndividuals

    Code
      setPopulation(project, "testpopulation", numberOfIndividuals = 0)
    Condition
      Error in `setPopulation()`:
      ! `numberOfIndividuals` must be a positive whole number

# setPopulation rejects a non-numeric range field

    Code
      setPopulation(project, "testpopulation", weightMin = "heavy")
    Condition
      Error in `setPopulation()`:
      ! weightMin must be a single finite number

# setPopulation rejects a non-integer numberOfIndividuals

    Code
      setPopulation(project, "testpopulation", numberOfIndividuals = 2.5)
    Condition
      Error in `setPopulation()`:
      ! `numberOfIndividuals` must be a positive whole number

# addPopulation aborts on a mismatched scalar field length

    Code
      addPopulation(project, c("a", "b", "c"), species = "Human",
      numberOfIndividuals = c(5, 7))
    Condition
      Error in `addPopulation()`:
      ! `numberOfIndividuals` must be length 1 or length 3 (the number of ids).
      x It is length 2.

# addPopulation aborts on a duplicate id in the batch

    Code
      addPopulation(project, c("a", "a"), species = "Human", numberOfIndividuals = 5)
    Condition
      Error in `addPopulation()`:
      ! duplicate population id in the batch: "a"

# addPopulation aborts on an existing id, replaces it with overwrite

    Code
      addPopulation(project, "pop", species = "Human", numberOfIndividuals = 9)
    Condition
      Error in `addPopulation()`:
      ! population "pop" already exists.
      i Pass `overwrite = TRUE` to replace it.

# removePopulation warns when still referenced by a scenario, removes anyway

    Code
      removePopulation(project, "testpopulation")
    Condition
      Warning:
      Removed population "testpopulation" is still referenced by 2 scenarios:
      * populationscenario and populationscenariofromcsv
      i These scenarios now have a dangling reference. Update or remove them.

# print.Population renders the configured fields

    Code
      print(project$definitions$populations[["testpopulation"]])
    Output
      <Population>
        * Species: Human
        * Number of Individuals: 2
        * Proportion of Females: 0
        * Age Range: 18 - 65
        * Weight Range: <empty string>
        * Height Range: <empty string>

# print.Population renders a minimal population

    Code
      print(project$definitions$populations[["minimal"]])
    Output
      <Population>
        * Species: Human
        * Number of Individuals: 10
        * Proportion of Females: <empty string>
        * Age Range: <empty string>
        * Weight Range: <empty string>
        * Height Range: <empty string>

# print.PopulationSource renders a csv and a programmatic source

    Code
      print(csv)
    Output
      <PopulationSource>
        * Type: csv
        * File: p.csv
    Code
      print(prog)
    Output
      <PopulationSource>
        * Type: programmatic
        * File: resolved from the runtime store at run time

# addPopulation aborts on a NaN numeric field

    Code
      addPopulation(project, "nanweight", "Human", numberOfIndividuals = 10,
        weightMin = NaN)
    Condition
      Error in `addPopulation()`:
      ! Cannot add population "nanweight":
      x weightMin must be a single finite number

