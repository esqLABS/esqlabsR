test_that("addScenario aborts when a referenced individualId is unknown", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      scenarioName = "Bad",
      modelFile = "Aciclovir.pkml",
      individualId = "Ghost"
    )
  )
})

test_that("addScenario rejects NA-valued FK args", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      scenarioName = "S",
      modelFile = "Aciclovir.pkml",
      individualId = NA_character_
    )
  )
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      scenarioName = "S",
      modelFile = "Aciclovir.pkml",
      outputPathIds = c("Output1", NA_character_)
    )
  )
})
