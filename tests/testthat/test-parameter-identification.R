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

test_that("PIOutputMapping() builds a plain-data record with the expected shape", {
  m <- PIOutputMapping(
    id = "PVB_obs",
    scenarios = "S1",
    outputPathId = "Aciclovir_PVB",
    observedDataId = "Laskin_GroupA",
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
  expect_identical(m$outputPathId, "Aciclovir_PVB")
  expect_identical(m$observedDataId, "Laskin_GroupA")
})

test_that("PIOutputMapping() errors on missing required fields", {
  expect_snapshot(
    error = TRUE,
    PIOutputMapping(
      id = "x",
      scenarios = "S1",
      outputPathId = "",
      observedDataId = "Laskin"
    )
  )
})

test_that("PITask() builds a plain-data record with the expected shape", {
  t <- PITask(
    id = "AciclovirSimple",
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
        outputPathId = "PVB",
        observedDataId = "Laskin"
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

test_that("PITask() errors when parameters is empty", {
  expect_snapshot(
    error = TRUE,
    PITask(
      id = "x",
      scenarios = "S1",
      parameters = list(),
      outputMappings = list(
        PIOutputMapping(
          id = "m",
          scenarios = "S1",
          outputPathId = "PVB",
          observedDataId = "Laskin"
        )
      )
    )
  )
})

test_that("PITask() errors when outputMappings is empty", {
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
      outputMappings = list()
    )
  )
})

test_that("PITask() errors on empty scenarios", {
  expect_snapshot(
    error = TRUE,
    PITask(
      id = "x",
      scenarios = character(0),
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
          outputPathId = "PVB",
          observedDataId = "Laskin"
        )
      )
    )
  )
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
          outputPathId = "PVB",
          observedDataId = "Laskin"
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
    outputPathId = "Aciclovir_PVB",
    observedDataId = "Laskin_GroupA",
    scaling = "lin",
    weight = c(1, 2, 3)
  )
  expect_snapshot(print(m))
})

test_that("print(PITask) renders header, scenarios, parameter count, mapping count, algorithm", {
  t <- PITask(
    id = "AciclovirSimple",
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
        outputPathId = "PVB",
        observedDataId = "Laskin"
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
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  expect_named(project$parameterIdentification, "AciclovirSimple")
})

test_that(".parsePITasks(NULL) returns an empty list", {
  expect_identical(esqlabsR:::.parsePITasks(NULL), list())
})

test_that(".parsePITasks(list()) returns an empty list", {
  expect_identical(esqlabsR:::.parsePITasks(list()), list())
})

test_that(".parsePITasks() builds PITask records keyed by id", {
  raw <- list(
    list(
      id = "AciclovirSimple",
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
          outputPathId = "Aciclovir_PVB",
          observedDataId = "Laskin_GroupA"
        )
      ),
      configuration = list(algorithm = "Monte-Carlo")
    )
  )
  parsed <- esqlabsR:::.parsePITasks(raw)
  expect_named(parsed, "AciclovirSimple")
  expect_s3_class(parsed[["AciclovirSimple"]], "PITask")
  expect_s3_class(parsed[["AciclovirSimple"]]$parameters[[1]], "PIParameter")
  expect_s3_class(
    parsed[["AciclovirSimple"]]$outputMappings[[1]],
    "PIOutputMapping"
  )
  expect_identical(
    parsed[["AciclovirSimple"]]$configuration$algorithm,
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
          outputPathId = "P",
          observedDataId = "D"
        )
      )
    )
  )
  parsed <- esqlabsR:::.parsePITasks(raw)
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
          outputPathId = "P",
          observedDataId = "D"
        )
      )
    )
  )
  parsed <- esqlabsR:::.parsePITasks(raw)
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
          outputPathId = "P",
          observedDataId = "D"
        )
      )
    )
  )
  parsed <- esqlabsR:::.parsePITasks(raw)
  m <- parsed[["T1"]]$outputMappings[[1]]
  expect_identical(m$xOffset, 0)
  expect_identical(m$yOffset, 0)
  expect_identical(m$xFactor, 1)
  expect_identical(m$yFactor, 1)
})

test_that(".parameterIdentificationToJson() emits NULL for empty input", {
  proj <- structure(
    list(parameterIdentification = list()),
    class = "Project"
  )
  expect_null(esqlabsR:::.parameterIdentificationToJson(proj))
})

test_that(".parsePITasks |> .parameterIdentificationToJson |> .parsePITasks is identity", {
  raw <- list(
    list(
      id = "AciclovirSimple",
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
          outputPathId = "P",
          observedDataId = "D",
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
  parsed <- esqlabsR:::.parsePITasks(raw)
  proj <- structure(
    list(parameterIdentification = parsed),
    class = "Project"
  )
  serialized <- esqlabsR:::.parameterIdentificationToJson(proj)
  reparsed <- esqlabsR:::.parsePITasks(serialized)
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
        outputPathId = "P",
        observedDataId = "D"
      )
    )
  )
  result <- esqlabsR:::.validatePI(list(T1 = task))
  expect_false(result$has_critical_errors())
})

test_that(".validatePI surfaces duplicate parameter ids within a task", {
  task <- PITask(
    id = "T",
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
        outputPathId = "P",
        observedDataId = "D"
      )
    )
  )
  result <- esqlabsR:::.validatePI(list(T = task))
  expect_true(result$has_critical_errors())
})

test_that(".validatePI is empty-section-friendly", {
  result <- esqlabsR:::.validatePI(list())
  expect_false(result$has_critical_errors())
})

test_that("validateProject() flags PI parameters that reference unknown scenarios", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  task <- PITask(
    id = "T",
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
        outputPathId = "Aciclovir_PVB",
        observedDataId = "Laskin"
      )
    )
  )
  project$parameterIdentification <- list(T = task)
  results <- validateProject(project)
  expect_true(esqlabsR::isAnyCriticalErrors(results))
})

test_that("validateProject() flags PI outputMappings that reference unknown outputPathIds", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  task <- PITask(
    id = "T",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "TestScenario",
        outputPathId = "DoesNotExist",
        observedDataId = "Laskin"
      )
    )
  )
  project$parameterIdentification <- list(T = task)
  results <- validateProject(project)
  expect_true(esqlabsR::isAnyCriticalErrors(results))
})

test_that(".createSinglePITask builds a ParameterIdentification with the expected counts", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  task <- PITask(
    id = "T",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "EHC",
        scenarios = "TestScenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  observedData <- loadObservedData(project)

  pi <- esqlabsR:::.createSinglePITask(
    project = project,
    piTask = task,
    observedData = observedData
  )
  expect_s3_class(pi, "ParameterIdentification")
  expect_length(pi$parameters, 1L)
  expect_length(pi$outputMappings, 1L)
})

test_that(".createSinglePITask shares one optimisation variable across scenarios for a multi-scenario PIParameter", {
  # Replaces the Excel "Group" column: a single PIParameter whose `scenarios`
  # lists several scenarios is built into one PIParameters runtime holding one
  # underlying parameter object per simulation, i.e. one estimated value fit
  # across all listed scenarios simultaneously.
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  sharedScenarios <- c("TestScenario", "TestScenario_steadystate")
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
        outputPathId = "Aciclovir_PVB",
        observedDataId = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  pi <- esqlabsR:::.createSinglePITask(
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
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  path <- "Organism|Liver|EHC continuous fraction"
  task <- PITask(
    id = "Split",
    scenarios = c("TestScenario", "TestScenario_steadystate"),
    parameters = list(
      PIParameter(
        id = "EHC_a",
        scenarios = "TestScenario",
        path = path,
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      ),
      PIParameter(
        id = "EHC_b",
        scenarios = "TestScenario_steadystate",
        path = path,
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "PVB",
        scenarios = c("TestScenario", "TestScenario_steadystate"),
        outputPathId = "Aciclovir_PVB",
        observedDataId = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
      )
    ),
    configuration = list(algorithm = "BOBYQA")
  )

  pi <- esqlabsR:::.createSinglePITask(
    project = project,
    piTask = task,
    observedData = loadObservedData(project)
  )

  # Two independent optimisation variables, each over a single simulation.
  expect_length(pi$parameters, 2L)
  expect_length(pi$parameters[[1]]$parameters, 1L)
  expect_length(pi$parameters[[2]]$parameters, 1L)
})

test_that("runPI(project) refuses to run when validation has critical errors", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  bad <- PITask(
    id = "T",
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
        outputPathId = "Aciclovir_PVB",
        observedDataId = "Laskin"
      )
    )
  )
  project$parameterIdentification <- list(T = bad)
  expect_snapshot(error = TRUE, runPI(project))
})

test_that("runPI(project) runs a task end to end and returns a PIResult", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  invisible(capture.output(suppressMessages(suppressWarnings(
    results <- runPI(project)
  ))))

  expect_named(results, "AciclovirSimple")
  entry <- results[["AciclovirSimple"]]
  expect_s3_class(entry$task, "ParameterIdentification")
  expect_s3_class(entry$result, "PIResult")
  expect_null(entry$error)
})

test_that("runPI(project) hard-fails when the build phase errors", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  local_mocked_bindings(
    .createSinglePITask = function(project, piTask, observedData) {
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
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  fakeRuntime <- structure(
    list(run = function() stop("optimiser diverged")),
    class = "ParameterIdentification"
  )
  local_mocked_bindings(
    .createSinglePITask = function(project, piTask, observedData) {
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

test_that("createPITasks() emits a soft-deprecation warning", {
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

test_that("addPITask() adds a task and clears validatedSinceMutation", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  validateProject(project)
  expect_true(project$validatedSinceMutation)

  addPITask(
    project,
    id = "Manual",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "Laskin"
      )
    )
  )
  expect_named(project$parameterIdentification, c("AciclovirSimple", "Manual"))
  expect_false(project$validatedSinceMutation)
})

test_that("addPITask() errors on unknown scenario id", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
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
          outputPathId = "Aciclovir_PVB",
          observedDataId = "Laskin"
        )
      )
    )
  )
})

test_that("addPITask() errors on unknown outputPathId", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  expect_snapshot(
    error = TRUE,
    addPITask(
      project,
      id = "Bad",
      scenarios = "TestScenario",
      parameters = list(
        PIParameter(
          id = "k",
          scenarios = "TestScenario",
          path = "x|y",
          minValue = 0,
          maxValue = 1,
          startValue = 0.5
        )
      ),
      outputMappings = list(
        PIOutputMapping(
          id = "m",
          scenarios = "TestScenario",
          outputPathId = "DoesNotExist",
          observedDataId = "Laskin"
        )
      )
    )
  )
})

test_that("addPITask() errors on duplicate id", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  args <- list(
    id = "Dup",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "Laskin"
      )
    )
  )
  do.call(addPITask, c(list(project = project), args))
  expect_snapshot(
    error = TRUE,
    do.call(addPITask, c(list(project = project), args))
  )
})

test_that("removePITask() warns and no-ops on missing id", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  expect_snapshot(removePITask(project, "NotThere"))
})

test_that("removePITask() removes the task and marks modified", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  addPITask(
    project,
    id = "X",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "k",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "L"
      )
    )
  )
  expect_named(project$parameterIdentification, c("AciclovirSimple", "X"))
  removePITask(project, "X")
  expect_named(project$parameterIdentification, "AciclovirSimple")
})

test_that("addPIParameter() appends a parameter and marks modified", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  addPITask(
    project,
    id = "T",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "p1",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "L"
      )
    )
  )
  validateProject(project)
  addPIParameter(
    project,
    taskId = "T",
    id = "p2",
    scenarios = "TestScenario",
    path = "a|b",
    minValue = 0,
    maxValue = 10,
    startValue = 1
  )
  expect_length(project$parameterIdentification$T$parameters, 2L)
  expect_false(project$validatedSinceMutation)
})

test_that("addPIParameter() errors on unknown taskId", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  expect_snapshot(
    error = TRUE,
    addPIParameter(
      project,
      taskId = "Ghost",
      id = "p",
      scenarios = "TestScenario",
      path = "x|y",
      minValue = 0,
      maxValue = 1,
      startValue = 0.5
    )
  )
})

test_that("addPIParameter() errors on unknown scenario id", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  addPITask(
    project,
    id = "T",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "L"
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    addPIParameter(
      project,
      taskId = "T",
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
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  addPITask(
    project,
    id = "T",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "p1",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "L"
      )
    )
  )
  addPIParameter(
    project,
    taskId = "T",
    scenarios = "TestScenario",
    path = "a|b",
    minValue = 0,
    maxValue = 1,
    startValue = 0.5
  )
  ids <- vapply(
    project$parameterIdentification$T$parameters,
    `[[`,
    character(1),
    "id"
  )
  expect_length(ids, 2L)
  expect_identical(ids[[2]], "T_param_2")
})

test_that("removePIParameter() warns and no-ops on missing id", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  addPITask(
    project,
    id = "T",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "p1",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "L"
      )
    )
  )
  expect_snapshot(removePIParameter(project, taskId = "T", id = "ghost"))
})

test_that("addPIOutputMapping() / removePIOutputMapping() round-trip", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  addPITask(
    project,
    id = "T",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m1",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "L"
      )
    )
  )
  addPIOutputMapping(
    project,
    taskId = "T",
    id = "m2",
    scenarios = "TestScenario",
    outputPathId = "Aciclovir_fat_cell",
    observedDataId = "L"
  )
  expect_length(project$parameterIdentification$T$outputMappings, 2L)
  removePIOutputMapping(project, taskId = "T", id = "m1")
  expect_length(project$parameterIdentification$T$outputMappings, 1L)
})

test_that("addPIOutputMapping() errors on unknown outputPathId", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  addPITask(
    project,
    id = "T",
    scenarios = "TestScenario",
    parameters = list(
      PIParameter(
        id = "p",
        scenarios = "TestScenario",
        path = "x|y",
        minValue = 0,
        maxValue = 1,
        startValue = 0.5
      )
    ),
    outputMappings = list(
      PIOutputMapping(
        id = "m1",
        scenarios = "TestScenario",
        outputPathId = "Aciclovir_PVB",
        observedDataId = "L"
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    addPIOutputMapping(
      project,
      taskId = "T",
      id = "m2",
      scenarios = "TestScenario",
      outputPathId = "DoesNotExist",
      observedDataId = "L"
    )
  )
})

test_that("loadProject() parses TestProject's parameterIdentification section", {
  project <- loadProject(test_path("data", "TestProject", "Project.json"))
  expect_named(project$parameterIdentification, "AciclovirSimple")
  expect_s3_class(
    project$parameterIdentification[["AciclovirSimple"]],
    "PITask"
  )
})

test_that("Project save / load round-trip preserves the parameterIdentification section", {
  tmp <- withr::local_tempfile(fileext = ".json")
  source <- test_path("data", "TestProject", "Project.json")
  project <- loadProject(source)
  esqlabsR:::.saveProjectJson(project, tmp)
  project2 <- loadProject(tmp)
  expect_identical(
    project2$parameterIdentification,
    project$parameterIdentification
  )
})
