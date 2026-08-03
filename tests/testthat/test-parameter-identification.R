# tests/testthat/test-parameter-identification.R

test_that("PIParameter() builds a plain-data record with the expected shape", {
  p <- PIParameter(
    id = "k_liver",
    scenarios = c("S1", "S2"),
    path = "Organism|Liver|Volume",
    units = "l",
    minValue = 0.5,
    maxValue = 2.0,
    startValue = 1.0
  )
  expect_s3_class(p, "PIParameter")
  expect_s3_class(p, "list")
  expect_named(
    p,
    c("id", "scenarios", "path", "units", "minValue", "maxValue", "startValue")
  )
  expect_identical(p$id, "k_liver")
  expect_identical(p$scenarios, c("S1", "S2"))
  expect_identical(p$path, "Organism|Liver|Volume")
})

test_that("PIParameter() errors on inverted bounds", {
  expect_snapshot(
    error = TRUE,
    PIParameter(
      id = "x",
      scenarios = "S1",
      path = "Organism|x|y",
      minValue = 5,
      maxValue = 1,
      startValue = 3
    )
  )
})

test_that("PIParameter() errors when start is outside [min, max]", {
  expect_snapshot(
    error = TRUE,
    PIParameter(
      id = "x",
      scenarios = "S1",
      path = "Organism|x|y",
      minValue = 0,
      maxValue = 1,
      startValue = 10
    )
  )
})

test_that("PIParameter() errors on empty scenarios", {
  expect_snapshot(
    error = TRUE,
    PIParameter(
      id = "x",
      scenarios = character(0),
      path = "Organism|x|y",
      minValue = 0,
      maxValue = 1,
      startValue = 0.5
    )
  )
})

test_that("PIParameter() errors on NA units", {
  expect_snapshot(
    error = TRUE,
    PIParameter(
      id = "x",
      scenarios = "S1",
      path = "Organism|x|y",
      units = NA_character_,
      minValue = 0,
      maxValue = 1,
      startValue = 0.5
    )
  )
})

test_that("PIParameter() errors on non-scalar units", {
  expect_snapshot(
    error = TRUE,
    PIParameter(
      id = "x",
      scenarios = "S1",
      path = "Organism|x|y",
      units = c("l", "ml"),
      minValue = 0,
      maxValue = 1,
      startValue = 0.5
    )
  )
})

test_that("PIParameter() accepts an unitless parameter (NULL or empty string)", {
  empty <- PIParameter(
    id = "x",
    scenarios = "S1",
    path = "Organism|x|y",
    units = "",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5
  )
  null <- PIParameter(
    id = "x",
    scenarios = "S1",
    path = "Organism|x|y",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5
  )
  expect_identical(empty$units, "")
  expect_null(null$units)
})

test_that("PIOutputMapping() builds a plain-data record with the expected shape", {
  m <- PIOutputMapping(
    id = "PVB_obs",
    scenarios = "S1",
    outputPath = "aciclovir_pvb",
    observedData = "Laskin_GroupA",
    scaling = "lin",
    xOffset = 0,
    yOffset = 0,
    xFactor = 1,
    yFactor = 1,
    weight = NULL
  )
  expect_s3_class(m, "PIOutputMapping")
  expect_named(
    m,
    c(
      "id",
      "scenarios",
      "outputPathId",
      "observedDataId",
      "scaling",
      "xOffset",
      "yOffset",
      "xFactor",
      "yFactor",
      "weight"
    )
  )
  expect_identical(m$id, "PVB_obs")
  expect_identical(m$outputPathId, "aciclovir_pvb")
  expect_identical(m$observedDataId, "Laskin_GroupA")
})

test_that("PIOutputMapping() errors on missing required fields", {
  expect_snapshot(
    error = TRUE,
    PIOutputMapping(
      id = "x",
      scenarios = "S1",
      outputPath = "",
      observedData = "Laskin"
    )
  )
})

test_that("PITask() builds a plain-data record with the expected shape", {
  t <- PITask(
    id = "aciclovirsimple",
    scenarios = "S1",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "S1",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "S1",
        outputPath = "PVB",
        observedData = "Laskin"
      )
    ),
    configuration = list(algorithm = "Monte-Carlo")
  )
  expect_s3_class(t, "PITask")
  expect_named(
    t,
    c("id", "scenarios", "parameters", "outputMappings", "configuration")
  )
  expect_length(t$parameters, 1L)
  expect_length(t$outputMappings, 1L)
})

test_that("PITask() allows empty parameters and outputMappings so a task can be seeded", {
  t <- PITask(
    id = "x",
    scenarios = "S1",
    parameters = list(),
    outputMappings = list()
  )
  expect_s3_class(t, "PITask")
  expect_length(t$parameters, 0L)
  expect_length(t$outputMappings, 0L)
})

test_that("PITask() errors when parameters or outputMappings is not a list", {
  expect_snapshot(
    error = TRUE,
    PITask(
      id = "x",
      scenarios = "S1",
      parameters = "nope",
      outputMappings = list()
    )
  )
  expect_snapshot(
    error = TRUE,
    PITask(
      id = "x",
      scenarios = "S1",
      parameters = list(),
      outputMappings = "nope"
    )
  )
})

test_that("PITask() takes an empty scenarios reference list, and a list of ids", {
  # `scenarios` is a reference list, so an empty one means "there are none" and
  # a JSON array read as a list of strings flattens: a task's own written field
  # goes straight back in, and a task is creatable with nothing in it at all.
  expect_length(PITask(id = "x")$scenarios, 0L)
  expect_length(PITask(id = "x", scenarios = character(0))$scenarios, 0L)
  expect_length(PITask(id = "x", scenarios = list())$scenarios, 0L)
  expect_equal(
    PITask(id = "x", scenarios = list("S1", "S2"))$scenarios,
    c("S1", "S2")
  )
})

test_that("PITask() errors on a malformed scenarios entry", {
  expect_snapshot(error = TRUE, PITask(id = "x", scenarios = c("S1", NA)))
  expect_snapshot(error = TRUE, PITask(id = "x", scenarios = ""))
  expect_snapshot(error = TRUE, PITask(id = "x", scenarios = 1))
})

test_that("PITask() errors when parameters contains non-PIParameter elements", {
  expect_snapshot(
    error = TRUE,
    PITask(
      id = "x",
      scenarios = "S1",
      parameters = list("not a record"),
      outputMappings = list(
        PIOutputMapping(
          id = "m",
          scenarios = "S1",
          outputPath = "PVB",
          observedData = "Laskin"
        )
      )
    )
  )
})

test_that("PITask() errors when outputMappings contains non-PIOutputMapping elements", {
  expect_snapshot(
    error = TRUE,
    PITask(
      id = "x",
      scenarios = "S1",
      parameters = list(
        PIParameter(
          id = "k",
          scenarios = "S1",
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(42)
    )
  )
})

test_that("print(PIParameter) renders a compact summary", {
  p <- PIParameter(
    id = "k_liver",
    scenarios = c("S1", "S2"),
    path = "Organism|Liver|Volume",
    units = "l",
    minValue = 0.5,
    maxValue = 2.0,
    startValue = 1.0
  )
  expect_snapshot(print(p))
})

test_that("print(PIOutputMapping) renders a compact summary", {
  m <- PIOutputMapping(
    id = "PVB_obs",
    scenarios = "S1",
    outputPath = "aciclovir_pvb",
    observedData = "Laskin_GroupA",
    scaling = "lin",
    weight = c(1, 2, 3)
  )
  expect_snapshot(print(m))
})

test_that("print(PITask) renders header, scenarios, parameter count, mapping count, algorithm", {
  t <- PITask(
    id = "aciclovirsimple",
    scenarios = c("S1", "S2"),
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "S1",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "S1",
        outputPath = "PVB",
        observedData = "Laskin"
      )
    ),
    configuration = list(
      algorithm = "Monte-Carlo",
      ciMethod = "hessian"
    )
  )
  expect_snapshot(print(t))
})

test_that("Project parses parameterIdentification field from JSON", {
  project <- testProject()
  expect_named(project$definitions$parameterIdentification, "aciclovirsimple")
})

test_that(".parsePITasks(NULL) returns an empty list", {
  expect_identical(.parsePITasks(NULL), list())
})

test_that(".parsePITasks(list()) returns an empty list", {
  expect_identical(.parsePITasks(list()), list())
})

test_that(".parsePITasks() builds PITask records keyed by id", {
  raw <- list(
    list(
      id = "aciclovirsimple",
      scenarios = list("Aciclovir_500mg"),
      parameters = list(
        list(
          id = "k",
          scenarios = list("Aciclovir_500mg"),
          path = "Organism|Liver|Volume",
          units = "l",
          minValue = 0.5,
          maxValue = 2.0,
          startValue = 1.0
        )
      ),
      outputMappings = list(
        list(
          id = "PVB_500",
          scenarios = list("Aciclovir_500mg"),
          outputPath = "aciclovir_pvb",
          observedData = "Laskin_GroupA"
        )
      ),
      configuration = list(algorithm = "Monte-Carlo")
    )
  )
  parsed <- .parsePITasks(raw)
  expect_named(parsed, "aciclovirsimple")
  expect_s3_class(parsed[["aciclovirsimple"]], "PITask")
  expect_s3_class(parsed[["aciclovirsimple"]]$parameters[[1]], "PIParameter")
  expect_s3_class(
    parsed[["aciclovirsimple"]]$outputMappings[[1]],
    "PIOutputMapping"
  )
  expect_identical(
    parsed[["aciclovirsimple"]]$configuration$algorithm,
    "Monte-Carlo"
  )
})

test_that(".parsePITasks() auto-generates parameter and outputMapping ids when absent", {
  raw <- list(
    list(
      id = "T1",
      scenarios = list("S1"),
      parameters = list(
        list(
          scenarios = list("S1"),
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        ),
        list(
          scenarios = list("S1"),
          path = "a|b",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(
        list(
          scenarios = list("S1"),
          outputPath = "P",
          observedData = "D"
        )
      )
    )
  )
  parsed <- .parsePITasks(raw)
  paramIds <- vapply(parsed[["T1"]]$parameters, `[[`, character(1), "id")
  expect_identical(paramIds, c("T1_param_1", "T1_param_2"))

  mappingIds <- vapply(parsed[["T1"]]$outputMappings, `[[`, character(1), "id")
  expect_identical(mappingIds, "T1_mapping_1")
})

test_that(".parsePITasks() preserves length-1 vector fields as length-1", {
  raw <- list(
    list(
      id = "T1",
      scenarios = list("OnlyOne"),
      parameters = list(
        list(
          scenarios = list("OnlyOne"),
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(
        list(
          scenarios = list("OnlyOne"),
          outputPath = "P",
          observedData = "D"
        )
      )
    )
  )
  parsed <- .parsePITasks(raw)
  expect_length(parsed[["T1"]]$scenarios, 1L)
  expect_length(parsed[["T1"]]$parameters[[1]]$scenarios, 1L)
})

test_that(".parsePITasks() injects defaults for outputMapping offset/factor fields", {
  raw <- list(
    list(
      id = "T1",
      scenarios = list("S1"),
      parameters = list(
        list(
          scenarios = list("S1"),
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(
        list(
          scenarios = list("S1"),
          outputPath = "P",
          observedData = "D"
        )
      )
    )
  )
  parsed <- .parsePITasks(raw)
  m <- parsed[["T1"]]$outputMappings[[1]]
  expect_identical(m$xOffset, 0)
  expect_identical(m$yOffset, 0)
  expect_identical(m$xFactor, 1)
  expect_identical(m$yFactor, 1)
})

test_that(".parameterIdentificationToJson() emits NULL for empty input", {
  proj <- .fakeProject(parameterIdentification = list())
  expect_null(.parameterIdentificationToJson(proj))
})

# #1213 item 26: a task with no solver settings has to describe its configuration
# the way a task with them does. An empty unnamed list serializes as `[]`, so such
# a task read as having an array where every other task has an object.
test_that("a PI task with no configuration serializes as an empty object", {
  raw <- list(
    list(
      id = "t",
      scenarios = list("S1"),
      parameters = list(
        list(
          id = "k",
          scenarios = list("S1"),
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(
        list(
          id = "m",
          scenarios = list("S1"),
          outputPath = "P",
          observedData = "D"
        )
      )
    )
  )
  proj <- .fakeProject(parameterIdentification = .parsePITasks(raw))
  serialized <- .parameterIdentificationToJson(proj)

  expect_identical(
    serialized[[1]]$configuration,
    structure(list(), names = character(0L))
  )
  expect_match(
    jsonlite::toJSON(serialized[[1]]$configuration, auto_unbox = TRUE),
    "{}",
    fixed = TRUE
  )
})

test_that(".parsePITasks |> .parameterIdentificationToJson |> .parsePITasks is identity", {
  raw <- list(
    list(
      id = "aciclovirsimple",
      scenarios = list("S1"),
      parameters = list(
        list(
          id = "k",
          scenarios = list("S1"),
          path = "x|y",
          units = "l",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(
        list(
          id = "m",
          scenarios = list("S1"),
          outputPath = "P",
          observedData = "D",
          scaling = "lin",
          xOffset = 0,
          yOffset = 0,
          xFactor = 1,
          yFactor = 1
        )
      ),
      configuration = list(algorithm = "Monte-Carlo")
    )
  )
  parsed <- .parsePITasks(raw)
  proj <- .fakeProject(parameterIdentification = parsed)
  serialized <- .parameterIdentificationToJson(proj)
  reparsed <- .parsePITasks(serialized)
  expect_identical(reparsed, parsed)
})

test_that(".validatePI returns no errors on a well-formed task", {
  task <- PITask(
    id = "T1",
    scenarios = "S1",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "S1",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "S1",
        outputPath = "P",
        observedData = "D"
      )
    )
  )
  result <- .validatePI(list(T1 = task))
  expect_false(result$hasCriticalErrors())
})

test_that(".validatePI surfaces duplicate parameter ids within a task", {
  task <- PITask(
    id = "t",
    scenarios = "S",
    parameters = list(
      PIParameter(
        id = "x",
        scenarios = "S",
        path = "a|b",
        minValue = 0,
        maxValue = 1,
        startValue = 0
      ),
      PIParameter(
        id = "x",
        scenarios = "S",
        path = "c|d",
        minValue = 0,
        maxValue = 1,
        startValue = 0
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "S",
        outputPath = "P",
        observedData = "D"
      )
    )
  )
  result <- .validatePI(list(t = task))
  expect_true(result$hasCriticalErrors())
})

test_that(".validatePI is empty-section-friendly", {
  result <- .validatePI(list())
  expect_false(result$hasCriticalErrors())
})

test_that(".validatePI flags a task left with no scenarios, parameters or output mappings", {
  # A task may be created empty and seeded later, but one still empty at
  # validation time cannot run and is a critical error. All three parts are
  # reported at once, so an empty task takes one report to fix rather than three.
  task <- PITask(id = "t")
  result <- .validatePI(list(t = task))
  expect_true(result$hasCriticalErrors())
  expect_length(result$critical_errors, 3L)

  withScenarios <- PITask(id = "t", scenarios = "S1")
  expect_length(.validatePI(list(t = withScenarios))$critical_errors, 2L)
})

test_that("validateProject() flags PI parameters that reference unknown scenarios", {
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "Ghost",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "Ghost",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "Ghost",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin"
      )
    )
  )
  .setSection(project, "parameterIdentification", list(t = task))
  results <- validateProject(project)
  expect_true(esqlabsR::isAnyCriticalErrors(results))
})

test_that("validateProject() flags PI outputMappings that reference unknown outputPaths", {
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "DoesNotExist",
        observedData = "Laskin"
      )
    )
  )
  .setSection(project, "parameterIdentification", list(t = task))
  results <- validateProject(project)
  expect_true(esqlabsR::isAnyCriticalErrors(results))
})

test_that(".createSinglePITask builds a ParameterIdentification with the expected counts", {
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  observedData <- loadObservedData(project)

  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = observedData
  )
  expect_s3_class(pi, "ParameterIdentification")
  expect_length(pi$parameters, 1L)
  expect_length(pi$outputMappings, 1L)
})

# #1213 item 14: a legacy 5.x `PIParameters` sheet carries a `Units` column that
# esqlabsR 5.x never applied, so a workbook that ran under 5.x routinely names a
# unit of another dimension. Enforcing it as the display unit aborted such a task
# (`Unit "mg" is not supported by dimension Inversed time!`) over a cell that has
# never had any effect, so it is reported and left unapplied instead, and the
# bounds stay in the parameter's own unit.
test_that(".createSinglePITask reports a unit of another dimension instead of aborting", {
  project <- testProject()
  task <- PITask(
    id = "WrongUnit",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        units = "mg",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  expect_warning(
    pi <- .createSinglePITask(
      project = project,
      piTask = task,
      observedData = loadObservedData(project)
    ),
    "is not applied"
  )

  # The bounds are the ones the record declares, read in the parameter's own unit.
  expect_equal(pi$parameters[[1]]$minValue, 0.5)
  expect_equal(pi$parameters[[1]]$maxValue, 1.0)
  expect_equal(pi$parameters[[1]]$startValue, 0.8)
})

test_that(".createSinglePITask shares one optimisation variable across scenarios for a multi-scenario PIParameter", {
  # Replaces the Excel "Group" column: a single PIParameter whose `scenarios`
  # lists several scenarios is built into one PIParameters runtime holding one
  # underlying parameter object per simulation, i.e. one estimated value fit
  # across all listed scenarios simultaneously.
  project <- testProject()
  sharedScenarios <- c("testscenario", "testscenario_steadystate")
  task <- PITask(
    id = "Shared",
    scenarios = sharedScenarios,
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = sharedScenarios,
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = sharedScenarios,
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )

  # One PIParameters object (one optimisation variable) spanning both
  # simulations: its `parameters` list holds one Parameter per scenario.
  expect_length(pi$parameters, 1L)
  expect_length(pi$parameters[[1]]$parameters, length(sharedScenarios))
})

test_that(".createSinglePITask keeps the same path independent across scenarios when split into separate PIParameters", {
  # The "different group, same path" Excel case: two PIParameter records over
  # the same path, each scoped to one scenario, build two independent
  # optimisation variables (one underlying Parameter each).
  project <- testProject()
  path <- "Organism|Liver|EHC continuous fraction"
  task <- PITask(
    id = "Split",
    scenarios = c("testscenario", "testscenario_steadystate"),
    parameters = list(
      PIParameter(
        id = "EHC_a",
        scenarios = "testscenario",
        path = path,
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      ),
      PIParameter(
        id = "EHC_b",
        scenarios = "testscenario_steadystate",
        path = path,
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = c("testscenario", "testscenario_steadystate"),
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )

  # Two independent optimisation variables, each over a single simulation.
  expect_length(pi$parameters, 2L)
  expect_length(pi$parameters[[1]]$parameters, 1L)
  expect_length(pi$parameters[[2]]$parameters, 1L)
})

test_that(".createSinglePITask errors when observedData is not in observedData", {
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "NonExistentDataSet"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  expect_error(
    .createSinglePITask(
      project = project,
      piTask = task,
      observedData = loadObservedData(project)
    ),
    regexp = "NonExistentDataSet"
  )
})

test_that(".createSinglePITask errors when a parameter path is not in the simulation", {
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Nonexistent|Parameter",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  expect_error(
    .createSinglePITask(
      project = project,
      piTask = task,
      observedData = loadObservedData(project)
    ),
    regexp = "Organism\\|Nonexistent\\|Parameter"
  )
})

test_that(".createSinglePITask applies objectiveFunctionOptions from the configuration block", {
  project <- testProject()
  observedData <- loadObservedData(project)
  mkTask <- function(configuration) {
    PITask(
      id = "t",
      scenarios = "testscenario",
      parameters = list(
        PIParameter(
          id = "EHC",
          scenarios = "testscenario",
          path = "Organism|Liver|EHC continuous fraction",
          minValue = 0.5,
          maxValue = 1.0,
          startValue = 0.8
        )
      ),
      outputMappings = list(
        PIOutputMapping(
          id = "PVB",
          scenarios = "testscenario",
          outputPath = "aciclovir_pvb",
          observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
        )
      ),
      configuration = configuration
    )
  }

  # Empty configuration: runtime defaults are preserved.
  pi <- .createSinglePITask(project, mkTask(list()), observedData)
  ofo <- pi$configuration$objectiveFunctionOptions
  expect_equal(ofo$objectiveFunctionType, "lsq")
  expect_equal(ofo$residualWeightingMethod, "none")
  expect_equal(ofo$robustMethod, "none")

  # String fields land.
  pi <- .createSinglePITask(
    project,
    mkTask(list(
      objectiveFunction = list(
        type = "m3",
        residualWeightingMethod = "error"
      )
    )),
    observedData
  )
  ofo <- pi$configuration$objectiveFunctionOptions
  expect_equal(ofo$objectiveFunctionType, "m3")
  expect_equal(ofo$residualWeightingMethod, "error")
  expect_equal(ofo$robustMethod, "none")

  # Numeric fields land.
  pi <- .createSinglePITask(
    project,
    mkTask(list(objectiveFunction = list(linScaleCV = 0.3, logScaleSD = 0.1))),
    observedData
  )
  ofo <- pi$configuration$objectiveFunctionOptions
  expect_equal(ofo$linScaleCV, 0.3)
  expect_equal(ofo$logScaleSD, 0.1)
})

test_that(".createSinglePITask applies simulationRunOptions from the configuration block", {
  project <- testProject()
  observedData <- loadObservedData(project)
  mkTask <- function(configuration) {
    PITask(
      id = "t",
      scenarios = "testscenario",
      parameters = list(
        PIParameter(
          id = "EHC",
          scenarios = "testscenario",
          path = "Organism|Liver|EHC continuous fraction",
          minValue = 0.5,
          maxValue = 1.0,
          startValue = 0.8
        )
      ),
      outputMappings = list(
        PIOutputMapping(
          id = "PVB",
          scenarios = "testscenario",
          outputPath = "aciclovir_pvb",
          observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
        )
      ),
      configuration = configuration
    )
  }

  # No block: simulationRunOptions stays NULL.
  pi <- .createSinglePITask(project, mkTask(list()), observedData)
  expect_null(pi$configuration$simulationRunOptions)

  # numberOfCores only.
  pi <- .createSinglePITask(
    project,
    mkTask(list(simulationRunOptions = list(numberOfCores = 2))),
    observedData
  )
  opts <- pi$configuration$simulationRunOptions
  expect_s3_class(opts, "SimulationRunOptions")
  expect_equal(opts$numberOfCores, 2L)

  # checkForNegativeValues only.
  pi <- .createSinglePITask(
    project,
    mkTask(list(simulationRunOptions = list(checkForNegativeValues = FALSE))),
    observedData
  )
  expect_false(pi$configuration$simulationRunOptions$checkForNegativeValues)

  # Both set.
  pi <- .createSinglePITask(
    project,
    mkTask(list(
      simulationRunOptions = list(
        numberOfCores = 4,
        checkForNegativeValues = FALSE
      )
    )),
    observedData
  )
  opts <- pi$configuration$simulationRunOptions
  expect_equal(opts$numberOfCores, 4L)
  expect_false(opts$checkForNegativeValues)
})

test_that(".createSinglePITask overwrites scenario output paths with the PI-specified paths", {
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )

  outputSelections <- vapply(
    pi$simulations[[1]]$outputSelections$allOutputs,
    function(x) x$path,
    character(1)
  )
  expect_equal(
    outputSelections,
    project$definitions$outputPaths[["aciclovir_pvb"]]
  )
})

test_that(".createSinglePITask applies a scalar weight to the runtime dataWeights", {
  project <- testProject()
  observedDataId <- "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = observedDataId,
        weight = 2
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )

  expect_all_equal(pi$outputMappings[[1]]$dataWeights[[observedDataId]], 2)
})

test_that(".createSinglePITask applies xOffset/yOffset/xFactor/yFactor to the runtime dataTransformations", {
  project <- testProject()
  observedDataId <- "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = observedDataId,
        xOffset = 0.5,
        yOffset = 1.0,
        xFactor = 2.0,
        yFactor = 0.5
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )

  transformations <- pi$outputMappings[[1]]$dataTransformations
  expect_equal(transformations$xOffsets[[observedDataId]], 0.5)
  expect_equal(transformations$yOffsets[[observedDataId]], 1.0)
  expect_equal(transformations$xFactors[[observedDataId]], 2.0)
  expect_equal(transformations$yFactors[[observedDataId]], 0.5)
})

test_that("runPI(project) refuses to run when validation has critical errors", {
  project <- testProject()
  bad <- PITask(
    id = "t",
    scenarios = "DoesNotExist",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "DoesNotExist",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "DoesNotExist",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin"
      )
    )
  )
  .setSection(project, "parameterIdentification", list(t = bad))
  expect_snapshot(error = TRUE, runPI(project))
})

test_that("runPI(project) runs a task end to end and returns a PIResult", {
  project <- testProject()
  invisible(capture.output(suppressMessages(suppressWarnings(
    results <- runPI(project)
  ))))

  expect_named(results, "aciclovirsimple")
  entry <- results[["aciclovirsimple"]]
  expect_s3_class(entry$task, "ParameterIdentification")
  expect_s3_class(entry$result, "PIResult")
  expect_null(entry$error)
})

test_that("runPI(project) warns when a parameter's uncertainty cannot be quantified", {
  # The bundled task converges (estimate == start value) but the Hessian-based
  # CI step yields no SD/CV/CI for the parameter, so the "converged" status is
  # misleading. The warning must name the parameter and the task. The exact
  # warning text is asserted by the .warnUnquantifiedUncertainty snapshot
  # tests; here we only confirm the end-to-end run surfaces it (and that the
  # result is still returned, the warning does not abort the run).
  project <- testProject()
  invisible(capture.output(suppressMessages(
    expect_warning(
      results <- runPI(project),
      "EHC continuous fraction"
    )
  )))
  expect_s3_class(results[["aciclovirsimple"]]$result, "PIResult")
})

test_that(".warnUnquantifiedUncertainty fires once per NA-uncertainty parameter", {
  # Drive the warning helper directly with a minimal toList()-shaped result so
  # the per-parameter warning text is exercised without a full optimisation run.
  fakeResult <- list(
    toList = function() {
      list(
        convergence = TRUE,
        paramNames = c("k_clear", "k_bound"),
        finalParameters = c(0.5, 1.2),
        sd = c(NA_real_, 0.1),
        cv = c(NA_real_, 8.3),
        lowerCI = c(NA_real_, 1.0),
        upperCI = c(NA_real_, 1.4)
      )
    }
  )
  expect_snapshot(
    .warnUnquantifiedUncertainty("myTask", fakeResult)
  )
})

test_that(".warnUnquantifiedUncertainty is silent when uncertainty is quantified", {
  fakeResult <- list(
    toList = function() {
      list(
        convergence = TRUE,
        paramNames = "k_clear",
        finalParameters = 0.5,
        sd = 0.05,
        cv = 10,
        lowerCI = 0.4,
        upperCI = 0.6
      )
    }
  )
  expect_no_warning(.warnUnquantifiedUncertainty(
    "myTask",
    fakeResult
  ))
})

test_that(".warnUnquantifiedUncertainty tolerates short or absent uncertainty vectors", {
  # A well-formed `toList()` keeps sd/cv/lowerCI/upperCI parallel to
  # `paramNames`, but a degenerate result can carry a shorter vector or omit one
  # entirely (NULL). Such a vector must be treated as all-NA of the right length
  # so the elementwise combination stays well-formed: no recycling warning, no
  # out-of-range indexing, one warning per genuinely unquantified parameter.
  fakeResult <- list(
    toList = function() {
      list(
        convergence = TRUE,
        paramNames = c("k_clear", "k_bound"),
        finalParameters = c(0.5, 1.2),
        sd = NA_real_, # short: length 1 against 2 parameters
        cv = NULL, # absent entirely
        lowerCI = c(NA_real_, NA_real_),
        upperCI = c(NA_real_, NA_real_)
      )
    }
  )

  warnings <- character()
  withCallingHandlers(
    .warnUnquantifiedUncertainty("myTask", fakeResult),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )

  # Both parameters are unquantified (sd/cv unavailable, CI all NA); each is
  # named exactly once, and no recycling warning is raised.
  expect_length(warnings, 2L)
  expect_match(warnings[[1]], "k_clear", fixed = TRUE)
  expect_match(warnings[[2]], "k_bound", fixed = TRUE)
  expect_false(any(grepl("not a multiple", warnings)))
})

test_that("runPI(project) hard-fails when the build phase errors", {
  project <- testProject()
  local_mocked_bindings(
    .createSinglePITask = function(
      project,
      piTask,
      observedData,
      stopIfParameterNotFound = TRUE
    ) {
      stop("Parameter |Organism|Live|EHC| not found in simulation")
    }
  )
  expect_error(
    runPI(project),
    "not found in simulation"
  )
})

test_that("runPI(project) soft-fails when the optimisation phase errors", {
  skip_if_not_installed("ospsuite.parameteridentification")
  project <- testProject()
  fakeRuntime <- structure(
    list(run = function() stop("optimiser diverged")),
    class = "ParameterIdentification"
  )
  local_mocked_bindings(
    .createSinglePITask = function(
      project,
      piTask,
      observedData,
      stopIfParameterNotFound = TRUE
    ) {
      fakeRuntime
    }
  )
  expect_warning(
    results <- runPI(project),
    "optimiser diverged"
  )
  expect_null(results[[1]]$result)
  expect_identical(results[[1]]$task, fakeRuntime)
})

test_that("runPI(project) soft-fails when the optimiser error message contains braces", {
  skip_if_not_installed("ospsuite.parameteridentification")
  project <- testProject()
  fakeRuntime <- structure(
    list(run = function() stop("solver failed at x={k}")),
    class = "ParameterIdentification"
  )
  local_mocked_bindings(
    .createSinglePITask = function(
      project,
      piTask,
      observedData,
      stopIfParameterNotFound = TRUE
    ) {
      fakeRuntime
    }
  )
  # A literal `{`/`}` in the optimiser message must not be re-evaluated as a cli
  # glue expression: the loop should still degrade to a soft-fail, not crash.
  expect_warning(
    results <- runPI(project),
    "solver failed at x={k}",
    fixed = TRUE
  )
  expect_null(results[[1]]$result)
  expect_identical(results[[1]]$task, fakeRuntime)
  expect_identical(results[[1]]$error, "solver failed at x={k}")
})

test_that("createPITasks() is defunct and aborts", {
  expect_snapshot(
    error = TRUE,
    createPITasks()
  )
})

test_that("runPI() with the legacy first-arg shape (non-Project) emits a soft-deprecation warning", {
  expect_snapshot(
    error = TRUE,
    runPI(list(SomeTask = "fake"))
  )
})

test_that("addPITask() adds a task and clears the validation flag", {
  project <- testProject()
  validateProject(project)
  expect_true(.isValidated(project))

  addPITask(
    project,
    id = "manual",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin"
      )
    )
  )
  expect_named(
    project$definitions$parameterIdentification,
    c("aciclovirsimple", "manual")
  )
  expect_false(.isValidated(project))
})

test_that("addPITask() can create an empty task that addPIParameter()/addPIOutputMapping() then seed", {
  project <- testProject()
  addPITask(
    project,
    id = "seeded",
    scenarios = "testscenario",
    parameters = list(),
    outputMappings = list()
  )
  task <- project$definitions$parameterIdentification$seeded
  expect_length(task$parameters, 0L)
  expect_length(task$outputMappings, 0L)

  addPIParameter(
    project,
    task = "seeded",
    path = "x|y",
    scenarios = "testscenario",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5
  )
  addPIOutputMapping(
    project,
    task = "seeded",
    outputPath = "aciclovir_pvb",
    observedData = "Laskin",
    scenarios = "testscenario"
  )

  seeded <- project$definitions$parameterIdentification$seeded
  expect_length(seeded$parameters, 1L)
  expect_length(seeded$outputMappings, 1L)
})

test_that("addPITask() creates a wholly empty task, which setPITask() then gives its scenarios", {
  # `addPITask(project, id)` is the create-then-seed entry point: nothing but the
  # id is required, and the parts arrive afterwards.
  project <- testProject()
  addPITask(project, id = "empty")
  task <- project$definitions$parameterIdentification$empty
  expect_length(task$scenarios, 0L)
  expect_length(task$parameters, 0L)
  expect_length(task$outputMappings, 0L)

  setPITask(project, "empty", scenarios = "testscenario")
  expect_equal(
    project$definitions$parameterIdentification$empty$scenarios,
    "testscenario"
  )
})

test_that("setPITask() sets the configuration, clears it, and leaves the other field alone", {
  project <- testProject()
  addPITask(project, id = "cfg", scenarios = "testscenario")

  setPITask(project, "cfg", configuration = list(algorithm = "BOBYQA"))
  task <- project$definitions$parameterIdentification$cfg
  expect_equal(task$configuration$algorithm, "BOBYQA")
  # The untouched field keeps its value.
  expect_equal(task$scenarios, "testscenario")

  setPITask(project, "cfg", configuration = NULL)
  expect_length(
    project$definitions$parameterIdentification$cfg$configuration,
    0L
  )
})

test_that("setPITask() errors on an unknown task, an unknown scenario, and a field it cannot set", {
  project <- testProject()
  addPITask(project, id = "cfg", scenarios = "testscenario")

  expect_snapshot(error = TRUE, setPITask(project, "ghost", scenarios = "x"))
  expect_snapshot(error = TRUE, setPITask(project, "cfg", scenarios = "ghost"))
  expect_snapshot(
    error = TRUE,
    project$setPITask("cfg", parameters = list())
  )
})

test_that("addPITask() errors on unknown scenario id", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addPITask(
      project,
      id = "Bad",
      scenarios = "Ghost",
      parameters = list(
        PIParameter(
          id = "k",
          scenarios = "Ghost",
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(
        PIOutputMapping(
          id = "m",
          scenarios = "Ghost",
          outputPath = "aciclovir_pvb",
          observedData = "Laskin"
        )
      )
    )
  )
})

test_that("addPITask() errors on unknown outputPath", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addPITask(
      project,
      id = "Bad",
      scenarios = "testscenario",
      parameters = list(
        PIParameter(
          id = "k",
          scenarios = "testscenario",
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(
        PIOutputMapping(
          id = "m",
          scenarios = "testscenario",
          outputPath = "DoesNotExist",
          observedData = "Laskin"
        )
      )
    )
  )
})

test_that("addPITask() errors on duplicate id", {
  project <- testProject()
  args <- list(
    id = "Dup",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin"
      )
    )
  )
  do.call(addPITask, c(list(project = project), args))
  expect_snapshot(
    error = TRUE,
    do.call(addPITask, c(list(project = project), args))
  )
})

test_that("addPITask() overwrite = TRUE replaces an existing task", {
  project <- testProject()
  args <- list(
    id = "dup",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin"
      )
    )
  )
  do.call(addPITask, c(list(project = project), args))
  before <- length(project$definitions$parameterIdentification)
  # A second call with overwrite = TRUE and a different parameter path replaces
  # the task rather than aborting: the task count is unchanged and the stored
  # task carries the new path.
  args$parameters[[1]]$path <- "x|z"
  do.call(
    addPITask,
    c(list(project = project), args, list(overwrite = TRUE))
  )
  expect_length(project$definitions$parameterIdentification, before)
  expect_identical(
    project$definitions$parameterIdentification$dup$parameters[[1]]$path,
    "x|z"
  )
})

test_that("removePITask() warns and no-ops on missing id", {
  project <- testProject()
  expect_snapshot(removePITask(project, "NotThere"))
})

test_that("removePITask() removes the task and marks modified", {
  project <- testProject()
  addPITask(
    project,
    id = "x",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  expect_named(
    project$definitions$parameterIdentification,
    c("aciclovirsimple", "x")
  )
  removePITask(project, "x")
  expect_named(project$definitions$parameterIdentification, "aciclovirsimple")
})

test_that("addPIParameter() appends a parameter and marks modified", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p1",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  validateProject(project)
  addPIParameter(
    project,
    task = "t",
    id = "p2",
    scenarios = "testscenario",
    path = "a|b",
    minValue = 0,
    maxValue = 10,
    startValue = 1
  )
  expect_length(project$definitions$parameterIdentification$t$parameters, 2L)
  expect_false(.isValidated(project))
})

test_that("addPIParameter() errors on unknown task", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addPIParameter(
      project,
      task = "Ghost",
      id = "p",
      scenarios = "testscenario",
      path = "x|y",
      minValue = 0,
      maxValue = 1,
      startValue = 0.5
    )
  )
})

test_that("addPIParameter() errors on unknown scenario id", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    addPIParameter(
      project,
      task = "t",
      id = "ghost-param",
      scenarios = "Ghost",
      path = "a|b",
      minValue = 0,
      maxValue = 1,
      startValue = 0.5
    )
  )
})

test_that("addPIParameter() auto-generates id when absent", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p1",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  addPIParameter(
    project,
    task = "t",
    scenarios = "testscenario",
    path = "a|b",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5
  )
  ids <- vapply(
    project$definitions$parameterIdentification$t$parameters,
    `[[`,
    character(1),
    "id"
  )
  expect_length(ids, 2L)
  # The auto-id scans for the first free "T_param_<N>" slot starting at 1; the
  # explicit "p1" id does not occupy "t_param_1".
  expect_identical(ids[[2]], "t_param_1")
})

test_that("removePIParameter() warns and no-ops on missing id", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p1",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  expect_snapshot(removePIParameter(project, task = "t", id = "ghost"))
})

test_that("addPIOutputMapping() / removePIOutputMapping() round-trip", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m1",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  addPIOutputMapping(
    project,
    task = "t",
    id = "m2",
    scenarios = "testscenario",
    outputPath = "aciclovir_fat_cell",
    observedData = "L"
  )
  expect_length(
    project$definitions$parameterIdentification$t$outputMappings,
    2L
  )
  removePIOutputMapping(project, task = "t", id = "m1")
  expect_length(
    project$definitions$parameterIdentification$t$outputMappings,
    1L
  )
})

test_that("outputPath accepts either an id or the literal model path of a defined output path", {
  project <- testProject()
  literalPath <- project$definitions$outputPaths[["aciclovir_pvb"]]

  # Inline in addPITask(): a literal model path resolves to the id.
  addPITask(
    project,
    id = "bypath",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = literalPath,
        observedData = "L"
      )
    )
  )
  expect_identical(
    project$definitions$parameterIdentification$bypath$outputMappings[[
      1
    ]]$outputPathId,
    "aciclovir_pvb"
  )

  # Via addPIOutputMapping(): a literal model path also resolves to the id.
  addPIOutputMapping(
    project,
    task = "bypath",
    id = "m2",
    scenarios = "testscenario",
    outputPath = literalPath,
    observedData = "L"
  )
  expect_identical(
    project$definitions$parameterIdentification$bypath$outputMappings[[
      2
    ]]$outputPathId,
    "aciclovir_pvb"
  )
})

test_that("addPIOutputMapping() errors with a did-you-mean hint on an undefined output path", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(),
    outputMappings = list()
  )
  expect_error(
    addPIOutputMapping(
      project,
      task = "t",
      scenarios = "testscenario",
      outputPath = "aciclovir_pv",
      observedData = "L"
    ),
    regexp = "aciclovir_pvb"
  )
})

test_that("addPIOutputMapping() aborts cleanly on a NULL / non-scalar outputPath", {
  # A malformed outputPath resolves to no output path and raises the typed
  # outputPathRefNotFound abort, not a raw zero/multi-length condition error.
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(),
    outputMappings = list()
  )
  expect_error(
    addPIOutputMapping(
      project,
      task = "t",
      scenarios = "testscenario",
      outputPath = NULL,
      observedData = "L"
    ),
    regexp = "neither a defined output-path id nor the model path"
  )
  expect_error(
    addPIOutputMapping(
      project,
      task = "t",
      scenarios = "testscenario",
      outputPath = c("aciclovir_pvb", "aciclovir_fat_cell"),
      observedData = "L"
    ),
    regexp = "neither a defined output-path id nor the model path"
  )
})

# PI sub-mutator write-through (on-disk) ----

test_that("removePIParameter / removePIOutputMapping update the task file on disk", {
  project <- testProject()
  # Grow the on-disk fixture task with a second parameter and mapping, then
  # remove one of each.
  addPIParameter(
    project,
    task = "aciclovirsimple",
    id = "extra",
    scenarios = "testscenario",
    path = "x|y",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5
  )
  addPIOutputMapping(
    project,
    task = "aciclovirsimple",
    id = "extramap",
    scenarios = "testscenario",
    outputPath = "aciclovir_fat_cell",
    observedData = "L"
  )

  removePIParameter(project, task = "aciclovirsimple", id = "extra")
  removePIOutputMapping(project, task = "aciclovirsimple", id = "extramap")

  # The task file persists the nested-record edits; a fresh load matches.
  reloaded <- loadProject(project$info$projectFilePath)
  task <- reloaded$definitions$parameterIdentification$aciclovirsimple
  paramIds <- vapply(task$parameters, `[[`, character(1), "id")
  mapIds <- vapply(task$outputMappings, `[[`, character(1), "id")
  expect_false("extra" %in% paramIds)
  expect_false("extramap" %in% mapIds)
  expect_setequal(paramIds, "ehc")
  expect_setequal(mapIds, "pvb")
})

test_that("removing the last PI parameter and mapping deletes the task file on save", {
  project <- testProject()
  saveProject(project)
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "parameter-identification"
  )
  expect_true(file.exists(file.path(dir, "aciclovirsimple.json")))

  # Removing the last mapping then the last parameter empties the task, which
  # auto-removes it; on save the definition file is deleted.
  suppressWarnings(
    removePIOutputMapping(project, task = "aciclovirsimple", id = "pvb")
  )
  suppressWarnings(
    removePIParameter(project, task = "aciclovirsimple", id = "ehc")
  )

  expect_false(
    "aciclovirsimple" %in% names(project$definitions$parameterIdentification)
  )
  saveProject(project)
  expect_false(file.exists(file.path(dir, "aciclovirsimple.json")))
  reloaded <- loadProject(project$info$projectFilePath)
  expect_length(reloaded$definitions$parameterIdentification, 0L)
})

test_that("addPIOutputMapping() errors on unknown outputPath", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m1",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    addPIOutputMapping(
      project,
      task = "t",
      id = "m2",
      scenarios = "testscenario",
      outputPath = "DoesNotExist",
      observedData = "L"
    )
  )
})

test_that("loadProject() parses TestProject's parameterIdentification section", {
  project <- testProject()
  expect_named(project$definitions$parameterIdentification, "aciclovirsimple")
  expect_s3_class(
    project$definitions$parameterIdentification[["aciclovirsimple"]],
    "PITask"
  )
})

test_that("removePIParameter() auto-removes the task when it becomes empty", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p1",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m1",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  removePIOutputMapping(project, task = "t", id = "m1")
  expect_length(
    project$definitions$parameterIdentification$t$outputMappings,
    0L
  )
  expect_warning(
    removePIParameter(project, task = "t", id = "p1"),
    "empty"
  )
  expect_null(project$definitions$parameterIdentification[["t"]])
})

test_that("removePIOutputMapping() auto-removes the task when it becomes empty", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p1",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m1",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "L"
      )
    )
  )
  removePIParameter(project, task = "t", id = "p1")
  expect_length(project$definitions$parameterIdentification$t$parameters, 0L)
  expect_warning(
    removePIOutputMapping(project, task = "t", id = "m1"),
    "empty"
  )
  expect_null(project$definitions$parameterIdentification[["t"]])
})

test_that("Project save / load round-trip preserves the parameterIdentification section", {
  tmp <- withr::local_tempfile(fileext = ".json")
  source <- test_path("data", "TestProject", "Project.json")
  project <- loadProject(source)
  .saveProjectJson(project, tmp)
  project2 <- loadProject(tmp)
  expect_identical(
    project2$definitions$parameterIdentification,
    project$definitions$parameterIdentification
  )
})

# Regression tests for #1053 ----

test_that(".createSinglePITask builds when declared bounds do not bracket the model default", {
  # The EHC parameter has a model default of 1; bounds 0.2/0.8 do not bracket
  # it. The runtime builder must assign startValue before min/max so the
  # upstream PIParameters setters validate against the user start value, not
  # the stale model default.
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.2,
        maxValue = 0.8,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )
  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )
  runtime <- pi$parameters[[1]]
  expect_equal(runtime$startValue, 0.5)
  expect_equal(runtime$minValue, 0.2)
  expect_equal(runtime$maxValue, 0.8)
})

test_that(".createSinglePITask applies the declared PIParameter units to the runtime", {
  # Liver Volume is in `l`; declaring `ml` must reach the runtime so bounds
  # and start value are interpreted in the display unit.
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "V",
        scenarios = "testscenario",
        path = "Organism|Liver|Volume",
        units = "ml",
        minValue = 1000,
        maxValue = 5000,
        startValue = 2000
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )
  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )
  runtime <- pi$parameters[[1]]
  expect_identical(runtime$unit, "ml")
  expect_equal(runtime$startValue, 2000)
  expect_equal(runtime$minValue, 1000)
  expect_equal(runtime$maxValue, 5000)
})

test_that(".createSinglePITask leaves the model default unit for an unitless PIParameter", {
  # An empty `units` means "no display unit"; the builder must not overwrite
  # the runtime unit, so it stays at the model default (Liver Volume is `l`).
  project <- testProject()
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "V",
        scenarios = "testscenario",
        path = "Organism|Liver|Volume",
        units = "",
        minValue = 0.5,
        maxValue = 5,
        startValue = 1
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )
  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )
  expect_identical(pi$parameters[[1]]$unit, "l")
})

test_that("PIOutputMapping weight survives a Project save / load round trip", {
  # Built through the public mutators and the real serializer so the
  # vector-to-list and scalar-to-integer drift is exercised end to end.
  project <- testProject()
  addPITask(
    project,
    id = "wt",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "vecWeight",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "D",
        weight = c(1, 2, 3)
      )
    )
  )
  addPIOutputMapping(
    project,
    task = "wt",
    id = "scalarWeight",
    outputPath = "aciclovir_pvb",
    observedData = "D2",
    scenarios = "testscenario",
    weight = 5
  )

  tmp <- withr::local_tempfile(fileext = ".json")
  .saveProjectJson(project, tmp)
  reloaded <- loadProject(tmp)
  mappings <- reloaded$definitions$parameterIdentification[[
    "wt"
  ]]$outputMappings

  expect_identical(mappings[[1]]$weight, c(1, 2, 3))
  expect_identical(mappings[[2]]$weight, 5)
})

test_that("addPIParameter() scans for a free id and does not collide after a removal", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "p_explicit",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "D"
      )
    )
  )
  addPIParameter(
    project,
    task = "t",
    path = "a|b",
    scenarios = "testscenario",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5
  )
  removePIParameter(project, "T", "p_explicit")
  # Auto-add again: must not abort with a colliding "T_param_2".
  addPIParameter(
    project,
    task = "t",
    path = "c|d",
    scenarios = "testscenario",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5
  )
  ids <- vapply(
    project$definitions$parameterIdentification[["t"]]$parameters,
    `[[`,
    character(1),
    "id"
  )
  expect_equal(anyDuplicated(ids), 0L)
  expect_length(ids, 2L)
})

test_that("addPIOutputMapping() scans for a free id and does not collide after a removal", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m_explicit",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "D"
      )
    )
  )
  addPIOutputMapping(
    project,
    task = "t",
    outputPath = "aciclovir_pvb",
    observedData = "D2",
    scenarios = "testscenario"
  )
  removePIOutputMapping(project, "T", "m_explicit")
  addPIOutputMapping(
    project,
    task = "t",
    outputPath = "aciclovir_pvb",
    observedData = "D3",
    scenarios = "testscenario"
  )
  ids <- vapply(
    project$definitions$parameterIdentification[["t"]]$outputMappings,
    `[[`,
    character(1),
    "id"
  )
  expect_equal(anyDuplicated(ids), 0L)
  expect_length(ids, 2L)
})

test_that("addPIParameter() errors on an explicit duplicate id", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "dup",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "D"
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    addPIParameter(
      project,
      task = "t",
      id = "dup",
      path = "a|b",
      scenarios = "testscenario",
      minValue = 0,
      maxValue = 1,
      startValue = 0.5
    )
  )
})

test_that("addPIParameter() overwrite = TRUE replaces the existing parameter", {
  project <- testProject()
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "dup",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "D"
      )
    )
  )
  addPIParameter(
    project,
    task = "t",
    id = "dup",
    path = "a|b",
    scenarios = "testscenario",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5,
    overwrite = TRUE
  )
  params <- project$definitions$parameterIdentification$t$parameters
  expect_length(params, 1L)
  expect_identical(params[[1]]$id, "dup")
  expect_identical(params[[1]]$path, "a|b")
})

test_that(".validatePI surfaces duplicate output mapping ids within a task", {
  task <- PITask(
    id = "t1",
    scenarios = "S1",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "S1",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "dup",
        scenarios = "S1",
        outputPath = "P",
        observedData = "D"
      ),
      PIOutputMapping(
        id = "dup",
        scenarios = "S1",
        outputPath = "P",
        observedData = "D"
      )
    )
  )
  result <- .validatePI(list(T1 = task))
  expect_false(result$isValid())
  expect_match(
    paste(result$critical_errors, collapse = " "),
    "Duplicate PIOutputMapping id within task 'T1'"
  )
})

test_that("removePITask() warns and no-ops on an unknown task", {
  project <- testProject()
  before <- names(project$definitions$parameterIdentification)
  expect_warning(removePITask(project, "Ghost"), "not found")
  expect_identical(names(project$definitions$parameterIdentification), before)
})

test_that("PIOutputMapping() validates scaling and the offset / factor / weight fields", {
  expect_snapshot(
    error = TRUE,
    PIOutputMapping(
      id = "m",
      scenarios = "S1",
      outputPath = "P",
      observedData = "D",
      xOffset = "not a number"
    )
  )
  expect_snapshot(
    error = TRUE,
    PIOutputMapping(
      id = "m",
      scenarios = "S1",
      outputPath = "P",
      observedData = "D",
      weight = "heavy"
    )
  )
  expect_snapshot(
    error = TRUE,
    PIOutputMapping(
      id = "m",
      scenarios = "S1",
      outputPath = "P",
      observedData = "D",
      scaling = ""
    )
  )
})

test_that("addPITask() rejects malformed outputMappings with a typed error", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addPITask(
      project,
      id = "t",
      scenarios = "testscenario",
      parameters = list(
        PIParameter(
          id = "k",
          scenarios = "testscenario",
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list("not a mapping")
    )
  )
})

test_that(".buildPIConfiguration() maps type to objectiveFunctionType and builds simulationRunOptions", {
  skip_if_not_installed("ospsuite.parameteridentification")
  cfg <- list(
    algorithm = "BOBYQA",
    objectiveFunction = list(type = "lsq"),
    simulationRunOptions = list(
      numberOfCores = 2,
      checkForNegativeValues = FALSE
    )
  )
  piConfig <- .buildPIConfiguration(cfg)
  expect_identical(piConfig$algorithm, "BOBYQA")
  expect_identical(
    piConfig$objectiveFunctionOptions$objectiveFunctionType,
    "lsq"
  )
  expect_s3_class(piConfig$simulationRunOptions, "SimulationRunOptions")
  expect_identical(piConfig$simulationRunOptions$numberOfCores, 2L)
  expect_false(piConfig$simulationRunOptions$checkForNegativeValues)
})

test_that(".buildPIConfiguration() merges partial algorithmOptions and ciOptions with per-algorithm defaults", {
  skip_if_not_installed("ospsuite.parameteridentification")
  cfg <- list(
    algorithm = "BOBYQA",
    algorithmOptions = list(maxeval = 500L),
    ciMethod = "PL",
    ciOptions = list(confLevel = 0.9)
  )
  piConfig <- .buildPIConfiguration(cfg)
  # User-supplied value overrides the default.
  expect_identical(piConfig$algorithmOptions$maxeval, 500L)
  # Remaining BOBYQA defaults are still filled in.
  expect_identical(
    piConfig$algorithmOptions$xtol_rel,
    ospsuite.parameteridentification::AlgorithmDefaults$BOBYQA$xtol_rel
  )
  # User-supplied ciOptions value overrides the default.
  expect_identical(piConfig$ciOptions$confLevel, 0.9)
  # Remaining PL defaults are still filled in.
  expect_identical(
    piConfig$ciOptions$maxIter,
    ospsuite.parameteridentification::CIDefaults$PL$maxIter
  )
})

test_that(".buildPIConfiguration() fills all algorithm defaults when algorithmOptions is absent", {
  skip_if_not_installed("ospsuite.parameteridentification")
  cfg <- list(algorithm = "HJKB")
  piConfig <- .buildPIConfiguration(cfg)
  expect_equal(
    piConfig$algorithmOptions,
    ospsuite.parameteridentification::AlgorithmDefaults$HJKB
  )
})

test_that(".createSinglePITask honours non-default transforms and weights", {
  project <- testProject()
  obsId <- "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
  task <- PITask(
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = obsId,
        xOffset = 1,
        yFactor = 2,
        weight = 3
      )
    )
  )
  pi <- .createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )
  expect_s3_class(pi, "ParameterIdentification")
  expect_length(pi$outputMappings, 1L)
  # The transform block and weight must land on the runtime mapping, keyed by
  # the observed dataset id; a silent drop would leave defaults (0/1) and NULL.
  runtime <- pi$outputMappings[[1]]
  transforms <- runtime$dataTransformations
  expect_identical(unname(transforms$xOffsets[obsId]), 1)
  expect_identical(unname(transforms$yOffsets[obsId]), 0)
  expect_identical(unname(transforms$xFactors[obsId]), 1)
  expect_identical(unname(transforms$yFactors[obsId]), 2)
  expect_equal(unique(runtime$dataWeights[[obsId]]), 3)
})

test_that("runPI(tasks = ) runs only the requested subset", {
  project <- testProject()
  addPITask(
    project,
    id = "second",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )
  invisible(capture.output(suppressMessages(suppressWarnings(
    results <- runPI(project, tasks = "aciclovirsimple")
  ))))
  expect_named(results, "aciclovirsimple")
})

test_that("runPI(tasks = ) aborts on an unknown task name", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    runPI(project, tasks = "Ghost")
  )
})

test_that("runPI(tasks = ) canonicalizes the referenced task ids", {
  # `addPITask()` canonicalizes its id, so the task is filed under the
  # canonical form. Referencing it by the originally typed (un-canonicalized)
  # name must still resolve, like every other id reference in the package.
  project <- testProject()
  addPITask(
    project,
    id = "MixedCase",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )
  expect_named(
    project$definitions$parameterIdentification,
    c("aciclovirsimple", "mixedcase")
  )
  invisible(capture.output(suppressMessages(suppressWarnings(
    results <- runPI(project, tasks = "MixedCase")
  ))))
  expect_named(results, "mixedcase")
})

test_that("runPI() accepts an explicit observedData argument", {
  project <- testProject()
  observedData <- loadObservedData(project)
  invisible(capture.output(suppressMessages(suppressWarnings(
    results <- runPI(project, observedData = observedData)
  ))))
  expect_named(results, "aciclovirsimple")
  expect_s3_class(results[["aciclovirsimple"]]$result, "PIResult")
})

test_that("runPI() builds every task before optimising any (fail fast on a build error)", {
  # Two tasks; the second has an unresolvable parameter path. The build phase
  # must abort before any task is optimised, so no completed result leaks out.
  project <- testProject()
  addPITask(
    project,
    id = "broken",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "ghost",
        scenarios = "testscenario",
        path = "Organism|DoesNotExist|Nope",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )
  runCount <- 0L
  local_mocked_bindings(
    .createSinglePITask = function(
      project,
      piTask,
      observedData,
      stopIfParameterNotFound = TRUE
    ) {
      if (identical(piTask$id, "broken")) {
        stop("Parameter not found in simulation")
      }
      structure(
        list(run = function() {
          runCount <<- runCount + 1L
          NULL
        }),
        class = "ParameterIdentification"
      )
    }
  )
  expect_error(
    suppressMessages(runPI(project)),
    "not found in simulation"
  )
  expect_identical(runCount, 0L)
})

test_that("runPI() returns an empty list on a project with zero PI tasks", {
  project <- testProject()
  .setSection(project, "parameterIdentification", list())
  results <- suppressMessages(runPI(project))
  expect_identical(results, list())
})

test_that("runPI(stopIfParameterNotFound = FALSE) reaches initializeSimulation", {
  # The argument was previously dead. It must now thread through to
  # .prepareScenario / initializeSimulation. The mock records the forwarded
  # value and short-circuits the rest of the build.
  project <- testProject()
  seen <- NULL
  local_mocked_bindings(
    initializeSimulation = function(..., stopIfParameterNotFound = TRUE) {
      seen <<- stopIfParameterNotFound
      stop("short-circuit build")
    }
  )
  expect_error(
    suppressMessages(suppressWarnings(
      runPI(project, stopIfParameterNotFound = FALSE)
    )),
    "short-circuit build"
  )
  expect_false(seen)
})

test_that("removeOutputPath() warns when the path is referenced only by a PI mapping", {
  # An output path used by no scenario but referenced by a PIOutputMapping:
  # the scenario branch of .warnIfReferenced() is silent, so this is the PI
  # inbound-reference path specifically.
  project <- testProject()
  addOutputPath(
    project,
    id = "pionlypath",
    path = "Organism|Liver|Volume"
  )
  addPITask(
    project,
    id = "t",
    scenarios = "testscenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "testscenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "testscenario",
        outputPath = "PIOnlyPath",
        observedData = "D"
      )
    )
  )
  expect_warning(
    removeOutputPath(project, "PIOnlyPath"),
    "PIOnlyPath"
  )
})

# Public authoring API: the three record constructors are exported ----

test_that("PITask / PIParameter / PIOutputMapping are exported from the namespace", {
  exported <- getNamespaceExports("esqlabsR")
  expect_true(all(
    c("PITask", "PIParameter", "PIOutputMapping") %in% exported
  ))
})

test_that("a complete PI task can be authored from scratch through exported functions only", {
  # A user with only library(esqlabsR) must be able to compose the records and
  # add the task without reaching into an already-loaded project's internals.
  project <- testProject()

  parameter <- PIParameter(
    id = "EHC",
    scenarios = "testscenario",
    path = "Organism|Liver|EHC continuous fraction",
    minValue = 0.5,
    maxValue = 1.0,
    startValue = 0.8
  )
  outputMapping <- PIOutputMapping(
    id = "PVB",
    scenarios = "testscenario",
    outputPath = "aciclovir_pvb",
    observedData = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
  )
  task <- PITask(
    id = "fromScratch",
    scenarios = "testscenario",
    parameters = list(parameter),
    outputMappings = list(outputMapping),
    configuration = list(algorithm = "BOBYQA")
  )
  expect_s3_class(task, "PITask")

  addPITask(
    project,
    id = task$id,
    scenarios = task$scenarios,
    parameters = task$parameters,
    outputMappings = task$outputMappings,
    configuration = task$configuration
  )
  expect_s3_class(
    project$definitions$parameterIdentification[["fromscratch"]],
    "PITask"
  )

  invisible(capture.output(suppressMessages(suppressWarnings(
    results <- runPI(project, tasks = "fromScratch")
  ))))
  expect_named(results, "fromscratch")
  expect_s3_class(results[["fromscratch"]]$result, "PIResult")
})

test_that("removePITask removes a vector of task ids in one write-through", {
  project <- testProject()
  parameter <- PIParameter(
    id = "p",
    scenarios = "testscenario",
    path = "Aciclovir|Lipophilicity",
    minValue = -2,
    maxValue = 2,
    startValue = 0
  )
  mapping <- PIOutputMapping(
    id = "m",
    scenarios = "testscenario",
    outputPath = "aciclovir_pvb",
    observedData = "ds"
  )
  addPITask(project, "t1", "testscenario", list(parameter), list(mapping))
  addPITask(project, "t2", "testscenario", list(parameter), list(mapping))

  removePITask(project, c("t1", "t2"))
  expect_false(any(
    c("t1", "t2") %in% names(project$definitions$parameterIdentification)
  ))
  reloaded <- loadProject(project$info$projectFilePath)
  expect_false(
    any(c("t1", "t2") %in% names(reloaded$definitions$parameterIdentification))
  )
})

# A hand-maintained 5.x PI sheet routinely lacks a column the parser reads, or
# leaves a cell blank. `is.na(x) || x == ""` is only safe on a cell that exists:
# on an absent one it evaluates to `NA` and `if (NA)` aborted the whole restore
# with `missing value where TRUE/FALSE needed`, naming neither sheet nor column
# (#1190).
test_that(".uniqueImportedId falls back instead of aborting on an empty cell", {
  expect_identical(.uniqueImportedId(character(0), character()), "item")
  expect_identical(.uniqueImportedId(NULL, character()), "item")
  expect_identical(.uniqueImportedId(NA, character()), "item")
  expect_identical(.uniqueImportedId("   ", character()), "item")
  # A usable cell is unaffected, and a clash still gets a suffix.
  expect_identical(.uniqueImportedId("Conc", character()), "conc")
  expect_identical(.uniqueImportedId("Conc", "conc"), "conc_2")
})

test_that(".pi5xPath tolerates an absent container or parameter cell", {
  expect_null(.pi5xPath(NULL, NULL))
  expect_null(.pi5xPath(character(0), character(0)))
  expect_null(.pi5xPath("Organism", NA))
  # A blank container yields the bare parameter name.
  expect_identical(.pi5xPath(NA, "Param"), "Param")
  expect_identical(.pi5xPath("   ", "Param"), "Param")
  expect_identical(.pi5xPath("Organism", "Param"), "Organism|Param")
})

test_that(".pi5xOptionRows tolerates a sheet without the option columns", {
  # `df[["Missing"]][[i]]` would abort on the index rather than yield an absent
  # value, so the columns are read through `.cellValue()`.
  noColumns <- data.frame(PITaskName = "t1", stringsAsFactors = FALSE)
  expect_identical(.pi5xOptionRows(noColumns, "t1"), list())

  blankName <- data.frame(
    PITaskName = c("t1", "t1"),
    OptionName = c("MaxIterations", NA),
    OptionValue = c("50", "ignored"),
    stringsAsFactors = FALSE
  )
  expect_identical(.pi5xOptionRows(blankName, "t1"), list(MaxIterations = 50))
})

# The reported failure: a mapping sheet whose `OutputPath` cell is blank, or
# whose column is missing entirely, parses instead of aborting. The mapping
# keeps no `outputPath`, so validation reports the incomplete mapping rather
# than the parse dying with an unattributable error (#1190).
test_that(".parseExcelPI5xMappings parses a row with no output path", {
  blankCell <- data.frame(
    PITaskName = c("t1", "t1"),
    OutputPath = c("Organism|A|Conc", NA),
    Scenarios = c("s1", "s1"),
    DataSet = c("d1", "d2"),
    stringsAsFactors = FALSE
  )
  mappings <- .parseExcelPI5xMappings(blankCell, "t1")
  expect_length(mappings, 2L)
  expect_identical(mappings[[1]]$outputPath, "Organism|A|Conc")
  expect_null(mappings[[2]]$outputPath)
  # Still gets a usable, unique id.
  expect_identical(mappings[[2]]$id, "item")
})

# The sheet's oldest revision has no `OutputPath` column at all: it predates the
# column and identified a mapping's output through the scenario, one mapping per
# output path the scenario declares. Reproducing that lets such a workbook
# restore instead of failing on every one of its mappings (#1192).
test_that(".parseExcelPI5xMappings derives a missing OutputPath column", {
  noColumn <- data.frame(
    PITaskName = "t1",
    Scenarios = "s1, s2",
    DataSet = "d1",
    Scaling = "log",
    stringsAsFactors = FALSE
  )
  scenarios <- list(
    list(name = "s1", outputPaths = list("pvb", "urine")),
    list(name = "s2", outputPaths = list("pvb"))
  )

  mappings <- .parseExcelPI5xMappings(noColumn, "t1", scenarios)

  # One mapping per output path, not per scenario: a path both scenarios declare
  # becomes one mapping naming both.
  expect_length(mappings, 2L)
  expect_identical(mappings[[1]]$outputPath, "pvb")
  expect_identical(unlist(mappings[[1]]$scenarios), c("s1", "s2"))
  expect_identical(mappings[[2]]$outputPath, "urine")
  expect_identical(unlist(mappings[[2]]$scenarios), "s1")
  # Every other cell of the original row rides along.
  expect_identical(mappings[[1]]$observedData, "d1")
  expect_identical(mappings[[1]]$scaling, "log")
})

test_that(".parseExcelPI5xMappings keeps and reports an underivable path", {
  noColumn <- data.frame(
    PITaskName = "t1",
    Scenarios = "s1",
    DataSet = "d1",
    stringsAsFactors = FALSE
  )

  # A scenario that declares no output path, and no scenarios section at all,
  # both leave the mapping unidentifiable. The mapping is kept without a path
  # (validation reports it) rather than dropped from the task.
  expect_warning(
    mappings <- .parseExcelPI5xMappings(
      noColumn,
      "t1",
      list(list(name = "s1"))
    ),
    class = "esqlabsR_importIncompletePIOutputMappings"
  )
  expect_length(mappings, 1L)
  expect_null(mappings[[1]]$outputPath)
  expect_identical(mappings[[1]]$observedData, "d1")

  expect_warning(
    expect_length(.parseExcelPI5xMappings(noColumn, "t1", NULL), 1L),
    class = "esqlabsR_importIncompletePIOutputMappings"
  )
})

# The two sheets are hand-maintained separately, so they routinely spell one
# scenario differently. Every other cross-sheet reference in the import resolves
# on the canonical id, and this one has to as well, or the mapping is dropped and
# the warning blames a scenario that does have an output path.
test_that(".parseExcelPI5xMappings matches a scenario on its canonical id", {
  noColumn <- data.frame(
    PITaskName = "t1",
    Scenarios = "aciclovir iv",
    DataSet = "d1",
    stringsAsFactors = FALSE
  )
  scenarios <- list(list(name = "Aciclovir_IV", outputPaths = list("pvb")))

  mappings <- .parseExcelPI5xMappings(noColumn, "t1", scenarios)

  expect_length(mappings, 1L)
  expect_identical(mappings[[1]]$outputPath, "pvb")
})

# `observedData` is as required as `outputPath` on a built mapping, so a row with
# no DataSet is incomplete too. It is kept and reported, not dropped: the load
# path is lenient about both fields and validation names them.
test_that(".parseExcelPI5xMappings keeps a derived row with no DataSet", {
  noColumn <- data.frame(
    PITaskName = "t1",
    Scenarios = c("s1", "s1"),
    DataSet = c(NA, "d1"),
    stringsAsFactors = FALSE
  )
  scenarios <- list(list(name = "s1", outputPaths = list("pvb")))

  mappings <- .parseExcelPI5xMappings(noColumn, "t1", scenarios)

  expect_length(mappings, 2L)
  expect_null(mappings[[1]]$observedData)
  expect_identical(mappings[[1]]$outputPath, "pvb")
  expect_identical(mappings[[2]]$observedData, "d1")
})

# The load path is deliberately more tolerant than the constructor: a project
# file carrying an incomplete mapping has to open so validateProject() can report
# it, while authoring one through PIOutputMapping() is still rejected at the call.
test_that(".parsePIOutputMappings loads a mapping the constructor would reject", {
  mappings <- .parsePIOutputMappings(
    list(list(id = "m1", scenarios = list("s1"), observedData = "d1")),
    "t1"
  )

  expect_length(mappings, 1L)
  expect_s3_class(mappings[[1]], "PIOutputMapping")
  expect_null(mappings[[1]]$outputPathId)
  expect_identical(mappings[[1]]$observedDataId, "d1")

  expect_snapshot(
    error = TRUE,
    PIOutputMapping(id = "m1", scenarios = "s1", observedData = "d1")
  )
})

# A hand-maintained 5.x `PIParameters` sheet routinely leaves a bounds cell
# blank. The parameter loads and validation names the gap, rather than the whole
# project failing to open, which is what made such a workbook unfixable.
test_that(".parsePIParameters loads a parameter the constructor would reject", {
  parameters <- .parsePIParameters(
    list(list(
      id = "p1",
      scenarios = list("s1"),
      path = "Aciclovir|Lipophilicity",
      maxValue = 2,
      startValue = 0
    )),
    "t1"
  )

  expect_length(parameters, 1L)
  expect_s3_class(parameters[[1]], "PIParameter")
  # Absent, not NA: the validator tells "no value" from "an unusable one".
  expect_null(parameters[[1]]$minValue)
  expect_identical(parameters[[1]]$maxValue, 2)

  expect_snapshot(
    error = TRUE,
    PIParameter(
      id = "p1",
      scenarios = "s1",
      path = "Aciclovir|Lipophilicity",
      maxValue = 2,
      startValue = 0
    )
  )
})

# Every gap in one report. The cross-reference phase skips itself once a section
# has a critical error, so a mapping's missing field is reported by the section
# validator; otherwise a user fixing a parameter would only then discover the
# mapping. The bounds comparison must also not run against an absent bound.
test_that(".validatePI reports every incomplete PI record at once", {
  task <- PITask(
    id = "t1",
    scenarios = "s1",
    parameters = list(PIParameter(
      id = "p1",
      scenarios = "s1",
      path = "A|B",
      minValue = 0,
      maxValue = 2,
      startValue = 1
    )),
    outputMappings = list(PIOutputMapping(
      id = "m1",
      scenarios = "s1",
      outputPath = "pvb",
      observedData = "d1"
    ))
  )
  # Hollow out one field of each record, as a blank workbook cell would.
  task$parameters[[1]]$minValue <- NULL
  task$outputMappings[[1]]$observedDataId <- NULL

  msgs <- vapply(
    .validatePI(list(t1 = task))$critical_errors,
    \(e) e$message,
    character(1)
  )

  expect_match(msgs, "missing required field: minValue", all = FALSE)
  expect_match(msgs, "does not define an observedData", all = FALSE)
  # No spurious bounds complaint: the comparison is skipped, not run on NULL.
  expect_false(any(grepl("invalid bounds", msgs, ignore.case = TRUE)))
})
