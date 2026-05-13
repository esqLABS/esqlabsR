# addScenario aborts when a referenced individualId is unknown

    Code
      addScenario(project, scenarioName = "Bad", modelFile = "Aciclovir.pkml",
        individualId = "Ghost")
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "Bad":
      x individualId 'Ghost' not found in individuals

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

