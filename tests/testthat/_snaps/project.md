# Project$print() renders the example project through ospPrint*

    Code
      print(project)
    Output
      <Project>
        * Name: Example
        * Description: Aciclovir IV PK example project
        * Schema Version: 2.0
        * esqlabsR Version: 6.0.0
        * JSON File: Project.json
      
      -- Paths (relative to the project folder) --------------------------------------
        * Simulations Folder: Models/Simulations
        * Data Folder: Data
        * Populations Folder: Populations
        * Output Folder: Results
        * Definitions Folder: definitions
      
      -- Definitions -----------------------------------------------------------------
        * Scenarios: 3
        * Individuals: 1
        * Populations: 1
        * Parameter Sets: 4
        * Initial Conditions: 1
        * Applications: 1
        * Output Paths: 2
        * Observed Data: 1
        * Data Combined: 1
        * Plots: 1
        * Plot Grids: 1
        * Parameter Identification: 1
      
      -- Excel -----------------------------------------------------------------------
        * Configurations Folder: Configurations/
        * Model Parameters File: ModelParameters.xlsx
        * Individuals File: Individuals.xlsx
        * Populations File: Populations.xlsx
        * Scenarios File: Scenarios.xlsx
        * Applications File: Applications.xlsx
        * Plots File: Plots.xlsx

# Project$print() omits zero-count definition sections

    Code
      print(project)
    Output
      <Project>
    Message
      [unsaved changes]
    Output
      
      -- Paths (relative to the project folder) --------------------------------------
        * Definitions Folder: definitions
      
      -- Definitions -----------------------------------------------------------------
        * Scenarios: 1

# Project$print() hides the Excel section and empty sections

    Code
      print(project)
    Output
      <Project>
      
      -- Paths (relative to the project folder) --------------------------------------
        * Definitions Folder: definitions

# a whole-section assignment through a section accessor is rejected

    Code
      project$definitions$scenarios <- list()
    Condition
      Error:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# a subscript assignment through a section accessor is rejected

    Code
      project$definitions$scenarios[["aciclovir_iv"]] <- Scenario(scenarioName = "aciclovir_iv",
        modelFile = "m.pkml")
    Condition
      Error in `[[<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# a nested field assignment through a section accessor is rejected

    Code
      project$definitions$scenarios[["testscenario"]]$individualId <- "indiv1"
    Condition
      Error in `[[<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# a negative-index assignment through a section accessor is rejected

    Code
      project$definitions$scenarios[-1] <- list()
    Condition
      Error in `[<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# info$projectFilePath is the loaded path and is read-only

    Code
      project$info$projectFilePath <- "elsewhere.json"
    Condition
      Error:
      ! info$projectFilePath is read-only and cannot be assigned into.

# project$status is read-only

    Code
      project$status <- list()
    Condition
      Error:
      ! status is readonly

# print() shows the unsaved-changes marker after an edit

    Code
      print(project)
    Output
      <Project>
    Message
      [unsaved changes]
    Output
        * Name: TestProject
        * Schema Version: 2.0
        * esqlabsR Version: 6.0.0
        * JSON File: Project.json
      
      -- Paths (relative to the project folder) --------------------------------------
        * Simulations Folder: Models/Simulations
        * Data Folder: Data
        * Populations Folder: Populations
        * Output Folder: Results
        * Definitions Folder: definitions
      
      -- Definitions -----------------------------------------------------------------
        * Scenarios: 4
        * Individuals: 1
        * Populations: 1
        * Parameter Sets: 4
        * Initial Conditions: 1
        * Applications: 1
        * Output Paths: 3
        * Observed Data: 1
        * Parameter Identification: 1
      
      -- Excel -----------------------------------------------------------------------
        * Configurations Folder: Configurations/
        * Model Parameters File: ModelParameters.xlsx
        * Individuals File: Individuals.xlsx
        * Populations File: Populations.xlsx
        * Scenarios File: Scenarios.xlsx
        * Applications File: Applications.xlsx
        * Plots File: Plots.xlsx

# print() shows no marker on a freshly loaded or saved project

    Code
      print(project)
    Output
      <Project>
        * Name: TestProject
        * Schema Version: 2.0
        * esqlabsR Version: 6.0.0
        * JSON File: Project.json
      
      -- Paths (relative to the project folder) --------------------------------------
        * Simulations Folder: Models/Simulations
        * Data Folder: Data
        * Populations Folder: Populations
        * Output Folder: Results
        * Definitions Folder: definitions
      
      -- Definitions -----------------------------------------------------------------
        * Scenarios: 4
        * Individuals: 1
        * Populations: 1
        * Parameter Sets: 4
        * Initial Conditions: 1
        * Applications: 1
        * Output Paths: 2
        * Observed Data: 1
        * Parameter Identification: 1
      
      -- Excel -----------------------------------------------------------------------
        * Configurations Folder: Configurations/
        * Model Parameters File: ModelParameters.xlsx
        * Individuals File: Individuals.xlsx
        * Populations File: Populations.xlsx
        * Scenarios File: Scenarios.xlsx
        * Applications File: Applications.xlsx
        * Plots File: Plots.xlsx

---

    Code
      print(project)
    Output
      <Project>
        * Name: TestProject
        * Schema Version: 2.0
        * esqlabsR Version: <version>
        * JSON File: Project.json
      
      -- Paths (relative to the project folder) --------------------------------------
        * Simulations Folder: Models/Simulations
        * Data Folder: Data
        * Populations Folder: Populations
        * Output Folder: Results
        * Definitions Folder: definitions
      
      -- Definitions -----------------------------------------------------------------
        * Scenarios: 4
        * Individuals: 1
        * Populations: 1
        * Parameter Sets: 4
        * Initial Conditions: 1
        * Applications: 1
        * Output Paths: 3
        * Observed Data: 1
        * Parameter Identification: 1
      
      -- Excel -----------------------------------------------------------------------
        * Configurations Folder: Configurations/
        * Model Parameters File: ModelParameters.xlsx
        * Individuals File: Individuals.xlsx
        * Populations File: Populations.xlsx
        * Scenarios File: Scenarios.xlsx
        * Applications File: Applications.xlsx
        * Plots File: Plots.xlsx

# a section accessor prints a count and the definition names

    Code
      print(project$definitions$individuals)
    Output
      <DefinitionList>
      individuals (1 definition):
        * indiv1

---

    Code
      print(project$definitions$parameterSets)
    Output
      <DefinitionList>
      parameterSets (4 definitions):
        * aciclovir
        * aciclovir_iv_250mg_default
        * global
        * indiv1_default

# an empty section accessor prints zero definitions

    Code
      print(project$definitions$individuals)
    Output
      <DefinitionList>
      individuals (0 definitions):

# the three plots sections each print a count and ids

    Code
      print(project$definitions$plots)
    Output
      <DefinitionList>
      plots (1 definition):
        * p1

---

    Code
      print(project$definitions$plotGrids)
    Output
      <DefinitionList>
      plotGrids (1 definition):
        * individual_diagnostics

---

    Code
      print(project$definitions$dataCombined)
    Output
      <DefinitionList>
      dataCombined (1 definition):
        * aciclovir_individual

