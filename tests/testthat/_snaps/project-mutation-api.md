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

