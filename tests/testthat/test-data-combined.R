# .validateDataCombined() tests ----
#
# The validator is static and spec-only: it reads the nested `simulated` /
# `observed` entry lists of the `dataCombined` section and reports missing
# required fields. It does not resolve references against scenario results or
# observed data (that is runtime work done in createDataCombined()) nor against
# other sections (the crossReferences phase owns that).

test_that(".validateDataCombined is empty-section-friendly", {
  expect_false(.validateDataCombined(NULL)$hasCriticalErrors())
  expect_false(.validateDataCombined(list())$hasCriticalErrors())
})

test_that(".validateDataCombined accepts a well-formed section", {
  dc <- list(
    DC = list(
      simulated = list(list(
        label = "sim",
        scenario = "s",
        path = "Organism|A|B"
      )),
      observed = list(list(label = "obs", dataSet = "d"))
    )
  )
  expect_false(.validateDataCombined(dc)$hasCriticalErrors())
})

test_that(".validateDataCombined flags a simulated entry missing label", {
  dc <- list(DC = list(simulated = list(list(scenario = "s", path = "p"))))
  result <- .validateDataCombined(dc)
  expect_true(result$hasCriticalErrors())
})

test_that(".validateDataCombined flags a simulated entry missing scenario", {
  dc <- list(DC = list(simulated = list(list(label = "l", path = "p"))))
  expect_true(.validateDataCombined(dc)$hasCriticalErrors())
})

test_that(".validateDataCombined flags a simulated entry missing path", {
  dc <- list(DC = list(simulated = list(list(label = "l", scenario = "s"))))
  expect_true(.validateDataCombined(dc)$hasCriticalErrors())
})

test_that(".validateDataCombined flags an observed entry missing dataSet", {
  dc <- list(DC = list(observed = list(list(label = "l"))))
  expect_true(.validateDataCombined(dc)$hasCriticalErrors())
})

test_that(".validateDataCombined treats an empty string as missing", {
  dc <- list(
    DC = list(
      simulated = list(list(
        label = "",
        scenario = "s",
        path = "p"
      ))
    )
  )
  expect_true(.validateDataCombined(dc)$hasCriticalErrors())
})

test_that(".validateDataCombined flags duplicate dataCombined ids", {
  # A duplicate key is only reachable by constructing the list by hand; the
  # authoring API canonicalizes and de-duplicates ids.
  dc <- stats::setNames(
    list(
      list(simulated = list(list(label = "l", scenario = "s", path = "p"))),
      list(simulated = list(list(label = "l", scenario = "s", path = "p")))
    ),
    c("DC", "DC")
  )
  expect_true(.validateDataCombined(dc)$hasCriticalErrors())
})

test_that("validateProject runs the dataCombined adapter", {
  project <- .fakeProject(
    dataCombined = list(
      Bad = list(simulated = list(list(scenario = "s", path = "p")))
    )
  )
  results <- .runProjectValidation(
    project,
    sections = "dataCombined"
  )
  expect_true(results$dataCombined$hasCriticalErrors())
})

test_that(".validateDataCombined flags an empty observed dataSet reference", {
  project <- .fakeProject()
  addDataCombined(
    project,
    id = "dc1",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = "Organism|A"
    ))
  )
  # Inject an empty observed dataSet directly, mimicking a hand-edited
  # Project.json that bypassed the addDataCombined() guard. The section
  # accessor is read-only; an in-memory project writes through .setSection()
  # without validating, so the malformed record survives for the validator.
  dc <- .getSection(project, "dataCombined")
  dc$dc1$observed <- list(list(label = "obs", dataSet = ""))
  .setSection(project, "dataCombined", dc)
  result <- .dataCombinedValidatorAdapter(project)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "dataSet", all = FALSE)
})


# createDataCombined(project, ...) tests ----

test_that("createDataCombined errors on non-Project input", {
  expect_error(createDataCombined("not a project"), "expected <Project>")
})

test_that("createDataCombined returns empty list when no names given", {
  project <- testProject()
  expect_identical(createDataCombined(project), list())
})

test_that("createDataCombined errors when requested name not in project", {
  project <- testProject()
  # TestProject has plots = NULL, so any requested name is missing
  expect_error(
    createDataCombined(project, dataCombined = "Nonexistent"),
    "The following DataCombined names are not defined"
  )
})

test_that("createDataCombined errors when a requested plotGrids name is unknown", {
  project <- exampleProject()
  # An unknown plot grid name must abort rather than being silently dropped
  # (the intersection would otherwise yield an incomplete or empty result).
  expect_snapshot(
    error = TRUE,
    createDataCombined(project, plotGrids = "DoesNotExist")
  )
})

test_that("createDataCombined builds DataCombined for Example project", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")
  dcName <- names(project$definitions$dataCombined)[[1]]

  result <- createDataCombined(
    project,
    dataCombined = dcName,
    scenarioResults = simulated
  )

  expect_named(result, dcName)
  expect_s3_class(result[[dcName]], "DataCombined")
  df <- result[[dcName]]$toDataFrame()
  expect_setequal(unique(df$dataType), c("simulated", "observed"))
})

test_that("createDataCombined aborts cleanly on a dataCombined missing a required field", {
  project <- .fakeProject()
  addDataCombined(
    project,
    id = "dc1",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = "Organism|A"
    ))
  )
  # Corrupt the stored entry (dropping its scenario), mimicking a hand-edited
  # definition file that bypassed the addDataCombined() write-gate.
  dc <- .getSection(project, "dataCombined")
  dc$dc1$simulated[[1]]$scenario <- NULL
  .setSection(project, "dataCombined", dc)

  # The validate pre-flight must abort with a clean message, not crash mid-build
  # on the NULL field.
  expect_error(
    createDataCombined(project, dataCombined = "dc1"),
    "scenario"
  )
})

test_that("createDataCombined errors when dataCombined is not a string", {
  project <- testProject()
  # The leading call-context in the validator message is context-dependent,
  # so match only the stable type-mismatch portion.
  expect_error(
    createDataCombined(project, dataCombined = 123),
    "is of type <numeric>, but expected <character>"
  )
})

test_that("createDataCombined resolves an output-path id in a simulated path", {
  project <- testProject()
  # `aciclovir_pvb` is an outputPaths id; passing it as `path` must resolve to
  # the literal model path when the DataCombined is built.
  addDataCombined(
    project,
    "dc_byid",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = "aciclovir_pvb",
      group = "g"
    ))
  )
  simulated <- runScenarios(project, scenarios = "testscenario")

  result <- createDataCombined(
    project,
    dataCombined = "dc_byid",
    scenarioResults = simulated
  )

  # The id is stored verbatim in the spec, not expanded to the literal path.
  storedPath <- project$definitions$dataCombined$dc_byid$simulated[[1]]$path
  expect_identical(storedPath, "aciclovir_pvb")
  # The built DataCombined nonetheless carries the resolved simulated data.
  df <- result$dc_byid$toDataFrame()
  expect_true("simulated" %in% df$dataType)
})

test_that("createDataCombined applies declared offsets and scale factors", {
  project <- testProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_plain",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = path,
      group = "g"
    ))
  )
  addDataCombined(
    project,
    "dc_offset",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = path,
      group = "g",
      xOffsets = 1,
      xOffsetsUnits = "h",
      yScaleFactors = 2
    ))
  )
  simulated <- runScenarios(project, scenarios = "testscenario")

  result <- createDataCombined(
    project,
    dataCombined = c("dc_plain", "dc_offset"),
    scenarioResults = simulated
  )
  plain <- result$dc_plain$toDataFrame()
  offset <- result$dc_offset$toDataFrame()

  # 1 h x-offset shifts time by 60 (base unit minutes); yScaleFactor doubles y.
  expect_equal(min(offset$xValues), min(plain$xValues) + 60)
  expect_equal(offset$yValues, plain$yValues * 2)
})

test_that("createDataCombined(stopIfNotFound = FALSE) drops a wrong-path entry with offsets", {
  project <- testProject()
  addDataCombined(
    project,
    "dc_wrong",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = "Organism|NotAReal|Path",
      group = "g",
      xOffsets = 1,
      xOffsetsUnits = "h"
    ))
  )
  simulated <- runScenarios(project, scenarios = "testscenario")

  expect_warning(
    result <- createDataCombined(
      project,
      dataCombined = "dc_wrong",
      scenarioResults = simulated,
      stopIfNotFound = FALSE
    ),
    "has not been simulated"
  )
  expect_s3_class(result$dc_wrong, "DataCombined")
  # The skipped row must not reach the transform block.
  expect_null(result$dc_wrong$toDataFrame())
})

test_that("createDataCombined(stopIfNotFound = FALSE) drops a missing-dataSet observed entry with offsets", {
  project <- testProject()
  addDataCombined(
    project,
    "dc_obs_wrong",
    observed = list(list(
      label = "obs",
      dataSet = "NotARealDataSet",
      group = "g",
      xOffsets = 1,
      xOffsetsUnits = "h"
    ))
  )
  simulated <- runScenarios(project, scenarios = "testscenario")

  # The missing dataSet is dropped (warn), and its label must not reach the
  # transform step, which would otherwise convert offsets against an absent row.
  expect_warning(
    result <- createDataCombined(
      project,
      dataCombined = "dc_obs_wrong",
      scenarioResults = simulated,
      stopIfNotFound = FALSE
    ),
    "not present in"
  )
  expect_s3_class(result$dc_obs_wrong, "DataCombined")
  expect_null(result$dc_obs_wrong$toDataFrame())
})

test_that("createDataCombined reports a scenario absent from results distinctly", {
  project <- testProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_missing_scenario",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = path,
      group = "g"
    ))
  )
  # The scenario was never run, so it is absent from scenarioResults: the error
  # must name the scenario reference, not blame the output path.
  expect_error(
    createDataCombined(
      project,
      dataCombined = "dc_missing_scenario",
      scenarioResults = list()
    ),
    "not present in"
  )
})

test_that("createDataCombined reports a failed scenario run distinctly", {
  project <- testProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_failed",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = path,
      group = "g"
    ))
  )
  # A failed run is present in scenarioResults but carries results = NULL. The
  # key must match the scenario casing so the lookup resolves and the code
  # reaches the results = NULL failed-run branch (not the missing-scenario one).
  failedRun <- list(
    testscenario = list(
      simulation = NULL,
      results = NULL,
      outputValues = NULL,
      population = NULL
    )
  )

  expect_snapshot(
    error = TRUE,
    createDataCombined(
      project,
      dataCombined = "dc_failed",
      scenarioResults = failedRun
    )
  )
})

test_that("createDataCombined returns empty DataCombined when spec has no entries", {
  project <- .fakeProject(
    dataCombined = list(
      EmptyDC = list(name = "EmptyDC", simulated = list(), observed = list())
    )
  )

  result <- createDataCombined(project, dataCombined = "EmptyDC")

  expect_named(result, "EmptyDC")
  expect_s3_class(result$EmptyDC, "DataCombined")
  expect_null(result$EmptyDC$toDataFrame())
})
