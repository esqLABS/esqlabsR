# PITaskConfiguration prints as expected

    Code
      piTaskConfig$print()
    Output
      <PITaskConfiguration>
      
      -- PI Task Configuration -------------------------------------------------------
        * Task Name: AciclovirSimple
        * Scenario(s): PITestScenario
        * Model File(s): Aciclovir.pkml
        * Algorithm: BOBYQA
        * CI Method: hessian
        * Number of Parameters: 1
        * Number of Output Mappings: 1

---

    Code
      print(piTaskConfig$piConfiguration)
    Output
      
      -- PI Configuration --
      
        * Algorithm: BOBYQA
        * CIMethod: hessian
        * PrintEvaluationFeedback: TRUE
        * AutoEstimateCI: FALSE
      
      -- Algorithm Options 
        * maxeval: 100
        * ftol_rel: 0
      
      -- CI Options 
        * confLevel: 0.95

---

    Code
      print(piTaskConfig$piParameters)
    Output
      
      -- PI Parameters --
      
        * Container Path: Aciclovir
        * Parameter Name: Lipophilicity
        * Value: -0.1
        * Units: Log Units
        * MinValue: -10
        * MaxValue: 10
        * StartValue: 1

---

    Code
      print(piTaskConfig$piOutputMappings)
    Output
      
      -- PI Output Mappings --
      
        * ObservedDataSheet: Laskin 1982.Group A
        * DataSet: Laskin 1982.Group
        A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_
        * Scaling: lin

# PITaskConfiguration prints multiple scenarios, parameters, and mappings

    Code
      piTaskConfig$print()
    Output
      <PITaskConfiguration>
      
      -- PI Task Configuration -------------------------------------------------------
        * Task Name: multiTest
        * Scenario(s): Scenario1 | Scenario2
        * Model File(s): model1.pkml | model2.pkml
        * Algorithm: NULL
        * CI Method: NULL
        * Number of Parameters: 2
        * Number of Output Mappings: 2

---

    Code
      print(piTaskConfig$piParameters)
    Output
      
      -- PI Parameters --
      
      Parameter 1:
        * Container Path: A
        * Parameter Name: P1
      Parameter 2:
        * Container Path: B
        * Parameter Name: P2

---

    Code
      print(piTaskConfig$piOutputMappings)
    Output
      
      -- PI Output Mappings --
      
      Output Mapping 1:
        * ObservedDataSheet: Sheet1
        * Scaling: lin
      Output Mapping 2:
        * ObservedDataSheet: Sheet2
        * Scaling: log

