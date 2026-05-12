# addScenario aborts when a referenced individualId is unknown

    Code
      addScenario(project, scenarioName = "Bad", modelFile = "Aciclovir.pkml",
        individualId = "Ghost")
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "Bad":
      x individualId 'Ghost' not found in individuals

# addIndividual aborts when individualId already exists

    Code
      addIndividual(project, "Indiv1", species = "Human", gender = "MALE")
    Condition
      Error in `addIndividual()`:
      ! Cannot add individual "Indiv1":
      x individual 'Indiv1' already exists

# addOutputPath aborts on a duplicate id

    Code
      addOutputPath(project, existing, "Organism|other|Concentration in container")
    Condition
      Error in `addOutputPath()`:
      ! Cannot add outputPath:
      x outputPath id already exists: Aciclovir_PVB

# addScenario rejects NA-valued FK args

    Code
      addScenario(project, scenarioName = "S", modelFile = "Aciclovir.pkml",
        individualId = NA_character_)
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "S":
      x individualId must be a non-empty string or NULL

---

    Code
      addScenario(project, scenarioName = "S", modelFile = "Aciclovir.pkml",
        outputPathIds = c("Output1", NA_character_))
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "S":
      x outputPathIds must be a non-empty character vector with no NA or empty entries

# addPlotGrid aborts when no plots are defined

    Code
      addPlotGrid(project, "G1", plotIDs = "MissingPlot")
    Condition
      Error in `addPlotGrid()`:
      ! no plots are defined; add plots before creating a plot grid.
      i use `addPlot()` to add plots referenced by `plotIDs`.

