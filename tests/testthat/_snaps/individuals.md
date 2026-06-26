# addIndividual aborts when individualId already exists

    Code
      addIndividual(project, "Indiv1", species = "Human", gender = "MALE")
    Condition
      Error in `addIndividual()`:
      ! Cannot add individual "Indiv1":
      x individual 'Indiv1' already exists

# addIndividual aborts when gender is missing

    Code
      addIndividual(project, "NewI", species = "Human")
    Condition
      Error in `addIndividual()`:
      ! Cannot add individual "NewI":
      x gender must be a non-empty string

