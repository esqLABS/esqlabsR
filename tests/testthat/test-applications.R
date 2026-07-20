test_that("addApplication + removeApplication round-trip leaves the section unchanged", {
  project <- testProject()
  before <- project$definitions$applications

  addApplication(project, "newapp")
  expect_true("newapp" %in% names(project$definitions$applications))

  removeApplication(project, "newapp")
  expect_identical(project$definitions$applications, before)
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
  expect_false("aciclovir_iv_250mg" %in% names(project$definitions$applications))
})

# setApplicationParameterSets ----

test_that("setApplicationParameterSets replaces the refs and persists on save", {
  project <- testProject()
  # The fixture application references one set; point it at a different
  # existing set instead.
  setApplicationParameterSets(project, "aciclovir_iv_250mg", "global")
  expect_identical(
    project$definitions$applications[["aciclovir_iv_250mg"]]$parameterSets,
    "global"
  )

  # The edit reaches disk on save.
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(
    reloaded$definitions$applications[["aciclovir_iv_250mg"]]$parameterSets,
    "global"
  )
})

test_that("setApplicationParameterSets aborts on an undefined parameter set", {
  project <- testProject()
  before <- project$definitions$applications[["aciclovir_iv_250mg"]]
  expect_snapshot(
    error = TRUE,
    setApplicationParameterSets(project, "aciclovir_iv_250mg", "Ghost")
  )
  expect_identical(project$definitions$applications[["aciclovir_iv_250mg"]], before)
})

test_that("addApplication and setApplicationParameterSets reject a non-character parameterSets with the same message", {
  # Both paths route through the shared `.resolveParameterSetRefs()`, so the
  # "must be a character vector of set ids" type-check message no longer drifts
  # between them.
  project <- testProject()
  expect_snapshot(error = TRUE, addApplication(project, "p", parameterSets = 1))
  expect_snapshot(
    error = TRUE,
    setApplicationParameterSets(project, "aciclovir_iv_250mg", 1)
  )
})

# Vectorized authoring ----

test_that("addApplication adds N protocols in one call equal to N scalar adds", {
  vectorized <- testProject()
  addApplication(vectorized, c("p1", "p2"), parameterSets = "global")

  scalar <- testProject()
  addApplication(scalar, "p1", parameterSets = "global")
  addApplication(scalar, "p2", parameterSets = "global")

  expect_identical(
    vectorized$definitions$applications[c("p1", "p2")],
    scalar$definitions$applications[c("p1", "p2")]
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
    project$definitions$applications$p1$parameterSets,
    c("global", "aciclovir")
  )
  expect_identical(
    project$definitions$applications$p2$parameterSets,
    c("global", "aciclovir")
  )
})

test_that("addApplication aborts the whole batch and writes nothing on a bad reference", {
  project <- testProject()
  before <- names(project$definitions$applications)
  expect_error(
    addApplication(project, c("p1", "p2"), parameterSets = "ghost")
  )
  expect_identical(names(project$definitions$applications), before)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(names(reloaded$definitions$applications), before)
})

test_that("addApplication aborts on a duplicate id in the batch", {
  project <- testProject()
  expect_snapshot(error = TRUE, addApplication(project, c("p1", "p1")))
})

test_that("removeApplication removes a vector of ids in one write-through", {
  project <- testProject()
  addApplication(project, c("p1", "p2"))
  removeApplication(project, c("p1", "p2"))
  expect_false(any(c("p1", "p2") %in% names(project$definitions$applications)))
  reloaded <- loadProject(project$info$projectFilePath)
  expect_false(any(c("p1", "p2") %in% names(reloaded$definitions$applications)))
})

test_that("setApplicationParameterSets vectorizes whole across N ids", {
  project <- testProject()
  addApplication(project, c("p1", "p2"))
  setApplicationParameterSets(project, c("p1", "p2"), c("global", "aciclovir"))
  expect_identical(
    project$definitions$applications$p1$parameterSets,
    c("global", "aciclovir")
  )
  expect_identical(
    project$definitions$applications$p2$parameterSets,
    c("global", "aciclovir")
  )
})

# Print method ----

test_that("print.Application renders its parameter-set references", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$applications[["aciclovir_iv_250mg"]]))
})

test_that("print.Application renders an empty protocol", {
  project <- testProject()
  addApplication(project, "empty")
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$applications[["empty"]]))
})
