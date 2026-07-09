test_that("addApplication + removeApplication round-trip leaves the section unchanged", {
  project <- testProject()
  before <- project$applications

  addApplication(project, "newapp")
  expect_true("newapp" %in% names(project$applications))

  removeApplication(project, "newapp")
  expect_identical(project$applications, before)
})

test_that("removeApplication warns on missing key and is a no-op", {
  project <- testProject()
  expect_warning(removeApplication(project, "Ghost"), "not found")
})

test_that("removeApplication warns when still referenced by a scenario, removes anyway", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  # `aciclovir_iv_250mg` is the application referenced by every scenario in
  # the fixture, so removing it leaves those scenarios with a dangling ref.
  expect_snapshot(removeApplication(project, "aciclovir_iv_250mg"))
  expect_false("aciclovir_iv_250mg" %in% names(project$applications))
})

# setApplicationParameterSets ----

test_that("setApplicationParameterSets replaces the refs and persists to file", {
  project <- testProject()
  # The fixture application references one set; point it at a different
  # existing set instead.
  setApplicationParameterSets(project, "aciclovir_iv_250mg", "global")
  expect_identical(
    project$applications[["aciclovir_iv_250mg"]]$parameterSets,
    "global"
  )

  # The write-through must reach disk.
  reloaded <- loadProject(project$jsonPath)
  expect_identical(
    reloaded$applications[["aciclovir_iv_250mg"]]$parameterSets,
    "global"
  )
})

test_that("setApplicationParameterSets aborts on an undefined parameter set", {
  project <- testProject()
  before <- project$applications[["aciclovir_iv_250mg"]]
  expect_snapshot(
    error = TRUE,
    setApplicationParameterSets(project, "aciclovir_iv_250mg", "Ghost")
  )
  expect_identical(project$applications[["aciclovir_iv_250mg"]], before)
})

# Vectorized authoring ----

test_that("addApplication adds N protocols in one call equal to N scalar adds", {
  vectorized <- testProject()
  addApplication(vectorized, c("p1", "p2"), parameterSets = "global")

  scalar <- testProject()
  addApplication(scalar, "p1", parameterSets = "global")
  addApplication(scalar, "p2", parameterSets = "global")

  expect_identical(
    vectorized$applications[c("p1", "p2")],
    scalar$applications[c("p1", "p2")]
  )
})

test_that("addApplication applies parameterSets whole to every protocol", {
  project <- testProject()
  addApplication(
    project,
    c("p1", "p2"),
    parameterSets = c("global", "aciclovir")
  )
  expect_identical(
    project$applications$p1$parameterSets,
    c("global", "aciclovir")
  )
  expect_identical(
    project$applications$p2$parameterSets,
    c("global", "aciclovir")
  )
})

test_that("addApplication aborts the whole batch and writes nothing on a bad reference", {
  project <- testProject()
  before <- names(project$applications)
  expect_error(
    addApplication(project, c("p1", "p2"), parameterSets = "ghost")
  )
  expect_identical(names(project$applications), before)
  reloaded <- loadProject(project$jsonPath)
  expect_identical(names(reloaded$applications), before)
})

test_that("addApplication aborts on a duplicate id in the batch", {
  project <- testProject()
  expect_snapshot(error = TRUE, addApplication(project, c("p1", "p1")))
})

test_that("removeApplication removes a vector of ids in one write-through", {
  project <- testProject()
  addApplication(project, c("p1", "p2"))
  removeApplication(project, c("p1", "p2"))
  expect_false(any(c("p1", "p2") %in% names(project$applications)))
  reloaded <- loadProject(project$jsonPath)
  expect_false(any(c("p1", "p2") %in% names(reloaded$applications)))
})

test_that("setApplicationParameterSets vectorizes whole across N ids", {
  project <- testProject()
  addApplication(project, c("p1", "p2"))
  setApplicationParameterSets(project, c("p1", "p2"), c("global", "aciclovir"))
  expect_identical(
    project$applications$p1$parameterSets,
    c("global", "aciclovir")
  )
  expect_identical(
    project$applications$p2$parameterSets,
    c("global", "aciclovir")
  )
})

# Print method ----

test_that("print.Application renders its parameter-set references", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$applications[["aciclovir_iv_250mg"]]))
})

test_that("print.Application renders an empty protocol", {
  project <- testProject()
  addApplication(project, "empty")
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$applications[["empty"]]))
})
