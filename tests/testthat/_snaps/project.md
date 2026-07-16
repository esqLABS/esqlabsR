# Project$print() renders the example project through ospPrint*

    Code
      print(project)
    Output
      <Project>
        * Name: Example
        * Description: Aciclovir IV PK example project
        * Schema Version: 2.0
        * esqlabsR Version: 6.0.0
        * JSON Path:
      <tmp>/Project.json
      
      -- Paths -----------------------------------------------------------------------
        * Simulations Folder:
      <tmp>/Models/Simulations
        * Data Folder:
      <tmp>/Data
        * Populations Folder:
      <tmp>/Populations
        * Output Folder:
      <tmp>/Results
      
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

# changing definitionsFolder on a project whose tree exists is refused

    Code
      project$definitionsFolder <- "other-defs"
    Condition
      Error:
      ! Cannot change definitionsFolder from "definitions" to "other-defs" while the tree exists on disk.
      x Re-pointing a materialized project would leave the old 'definitions/' tree orphaned and unreferenced.
      i To relocate the tree deliberately, snapshot the project with `saveSnapshot()` and reload it into a fresh directory under the new folder with `loadSnapshot()`.

# Project$print() omits zero-count definition sections

    Code
      print(project)
    Output
      <Project>
      
      -- Definitions -----------------------------------------------------------------
        * Scenarios: 1

# Project$print() hides the Excel section and empty sections

    Code
      print(project)
    Output
      <Project>

# a whole-section assignment through a section accessor is rejected

    Code
      project$scenarios <- list()
    Condition
      Error:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# a subscript assignment through a section accessor is rejected

    Code
      project$scenarios[["aciclovir_iv"]] <- Scenario(scenarioName = "aciclovir_iv",
        modelFile = "m.pkml")
    Condition
      Error in `[[<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# a nested field assignment through a section accessor is rejected

    Code
      project$scenarios[["testscenario"]]$individualId <- "indiv1"
    Condition
      Error in `[[<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# a negative-index assignment through a section accessor is rejected

    Code
      project$scenarios[-1] <- list()
    Condition
      Error in `[<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# jsonPath is read-only and aliases projectFilePath

    Code
      project$jsonPath <- "elsewhere.json"
    Condition
      Error:
      ! jsonPath is readonly

# a section accessor prints a count and the definition names

    Code
      print(project$individuals)
    Output
      <DefinitionList>
      individuals (1 definition):
        * indiv1

---

    Code
      print(project$parameterSets)
    Output
      <DefinitionList>
      parameterSets (4 definitions):
        * aciclovir
        * aciclovir_iv_250mg_default
        * global
        * indiv1_default

# an empty section accessor prints zero definitions

    Code
      print(project$individuals)
    Output
      <DefinitionList>
      individuals (0 definitions):

# the three plots sections each print a count and ids

    Code
      print(project$plots)
    Output
      <DefinitionList>
      plots (1 definition):
        * p1

---

    Code
      print(project$plotGrids)
    Output
      <DefinitionList>
      plotGrids (1 definition):
        * individual_diagnostics

---

    Code
      print(project$dataCombined)
    Output
      <DefinitionList>
      dataCombined (1 definition):
        * aciclovir_individual

