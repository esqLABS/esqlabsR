# printing a section shows its kind, count, and ids

    Code
      print(.asDefinitionList(list(one = 1, two = 2), "scenarios"))
    Output
      <DefinitionList>
      scenarios (2 definitions):
        * one
        * two

# printing an empty section still shows the header

    Code
      print(.asDefinitionList(list(), "populations"))
    Output
      <DefinitionList>
      populations (0 definitions):

# [[<- into a section aborts and names the section

    Code
      wrapped[["a"]] <- 2
    Condition
      Error in `[[<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# $<- into a section aborts

    Code
      wrapped$a <- 2
    Condition
      Error in `$<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# [<- into a section aborts

    Code
      wrapped[1] <- list(2)
    Condition
      Error in `[<-`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# assigning into a section with no kind still aborts

    Code
      bare[["a"]] <- 2
    Condition
      Error in `[[<-`:
      ! This project section is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

