test_that("It can read an empty sheet", {
  paramsXLSpath <- getTestDataFilePath("Parameters.xlsx")
  sheets <- c("EmptySheet")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

  expect_equal(names(params), c("paths", "values", "units"))
  expect_type(params$values, "double")
  expect_type(params$units, "character")
})

test_that("It can read a properly defined file", {
  paramsXLSpath <- getTestDataFilePath("Parameters.xlsx")
  sheets <- c("ValidSheet")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

  expect_named(params, c("paths", "values", "units"))
})

test_that("It can read a properly defined file with extra columns", {
  paramsXLSpath <- getTestDataFilePath("Parameters.xlsx")
  sheets <- c("ValidSheed_extraColumns")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

  expect_named(params, c("paths", "values", "units"))
})

test_that("It throws an error when a sheet has wrong structure", {
  paramsXLSpath <- getTestDataFilePath("Parameters.xlsx")
  sheets <- "InvalidSheet"
  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")
  expect_error(
    readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets),
    "Loading from XLS failed"
  )
})

test_that("It overwrites the value if the path is present in multiple sheets", {
  paramsXLSpath <- getTestDataFilePath("Parameters.xlsx")
  sheets <- c("ValidSheet", "SecondSheet")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

  paramsPaths <- c(
    "Path1|Param1",
    "Path2|DistincParam",
    "Applications|Glucose_iv_infusion|Active"
  )
  expectedVals <- c(5, 1, 0)
  expectedUnits <- c("mg", "µmol", "")
  idx <- match(paramsPaths, params$paths)
  expect_equal(expectedVals, params$values[idx])
  expect_equal(expectedUnits, params$units[idx])
})


test_that("It trows an error if wrong structure is provideed", {
  expect_error(
    extendParameterStructure(
      parameters = list(
        paths = "one",
        values = 2
      ),
      newParameters = list(
        paths = "one",
        values = 2,
        units = ""
      )
    ),
    "wrong structure"
  )
})

test_that("It accepts NULL for parameters and returns newParameters", {
  newParams <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = NULL,
    newParameters = newParams
  )

  expect_equal(extended$paths, newParams$paths)
  expect_equal(extended$values, newParams$values)
  expect_equal(extended$units, newParams$units)
})

test_that("It accepts NULL for newParameters and returns parameters", {
  params <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = NULL
  )

  expect_equal(extended$paths, params$paths)
  expect_equal(extended$values, params$values)
  expect_equal(extended$units, params$units)
})

test_that("It returns an empty valid structure when both parameters and newParameters are NULL", {
  extended <- extendParameterStructure(
    parameters = NULL,
    newParameters = NULL
  )

  expect_equal(extended, list(paths = NULL, values = NULL, units = NULL))
})

test_that("It extends an empty structure by new values", {
  params <- list(paths = NULL, values = NULL, units = NULL)
  newParams <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = newParams
  )

  expect_equal(extended$paths, newParams$paths)
  expect_equal(extended$values, newParams$values)
  expect_equal(extended$units, newParams$units)
})

test_that("It extends a structure by empty structure", {
  newParams <- list(paths = NULL, values = NULL, units = NULL)
  params <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = newParams
  )

  expect_equal(extended$paths, params$paths)
  expect_equal(extended$values, params$values)
  expect_equal(extended$units, params$units)
})

test_that("It extends a structure by a new structure", {
  params <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
    units = c("", "µmol")
  )
  newParams <- list(
    paths = c("Path2", "Path3"),
    values = c(1, 3),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = newParams
  )

  expect_equal(extended$paths, c("Path1", "Path2", "Path3"))
  expect_equal(extended$values, c(1, 1, 3))
  expect_equal(extended$units, c("", "", "µmol"))
})

# Unified parameterSets section ----

test_that("addParameterSet creates a set; removeParameterSet drops it", {
  project <- testProject()
  addParameterSet(project, "newset")
  expect_true("newset" %in% names(project$parameterSets))

  removeParameterSet(project, "newset")
  expect_false("newset" %in% names(project$parameterSets))
})

test_that("addParameterSet canonicalizes its id", {
  project <- testProject()
  expect_snapshot(addParameterSet(project, "New Set"))
  expect_true("new set" %in% names(project$parameterSets))
})

test_that("addParameterSet aborts on a duplicate id", {
  project <- testProject()
  expect_snapshot(error = TRUE, addParameterSet(project, "global"))
})

test_that("addParameterSet aborts on a duplicate id in the batch", {
  project <- testProject()
  expect_snapshot(error = TRUE, addParameterSet(project, c("a", "a")))
})

test_that("addParameterEntry creates the set on demand and appends entries", {
  project <- testProject()
  # Creating a set on demand is divergent from the other add* functions, so it
  # informs the user the first time (and only then).
  expect_snapshot(
    addParameterEntry(project, "tempset", "Organism|A", "K", 1.5, "1/h")
  )
  expect_true("tempset" %in% names(project$parameterSets))
  expect_length(project$parameterSets$tempset, 1L)

  # Appending to the existing set does not re-inform.
  expect_no_message(
    addParameterEntry(project, "tempset", "Organism|B", "L", 2.5, "1/h")
  )
  expect_length(project$parameterSets$tempset, 2L)
})

test_that("addParameterEntry accepts parallel vectors and writes once", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "parameter-sets")

  n <- 5L
  suppressMessages(
    addParameterEntry(
      project,
      "vecset",
      containerPath = paste0("Organism|A", seq_len(n)),
      parameterName = paste0("P", seq_len(n)),
      value = as.double(seq_len(n)),
      units = rep("1/h", n)
    )
  )

  # All N entries land in memory and on disk after one vectorized call.
  expect_length(project$parameterSets$vecset, n)
  reloaded <- loadProject(project$jsonPath)
  expect_length(reloaded$parameterSets$vecset, n)
  expect_identical(
    vapply(reloaded$parameterSets$vecset, \(e) e$parameterName, character(1)),
    paste0("P", seq_len(n))
  )
})

test_that("a vectorized addParameterEntry equals three scalar adds", {
  vectorized <- testProject()
  suppressMessages(
    addParameterEntry(
      vectorized,
      "set",
      containerPath = c("Organism|A", "Organism|B", "Organism|C"),
      parameterName = c("Ka", "Kb", "Kc"),
      value = c(1, 2, 3),
      units = c("1/h", "", "mg")
    )
  )

  scalar <- testProject()
  suppressMessages(
    addParameterEntry(scalar, "set", "Organism|A", "Ka", 1, "1/h")
  )
  addParameterEntry(scalar, "set", "Organism|B", "Kb", 2, "")
  addParameterEntry(scalar, "set", "Organism|C", "Kc", 3, "mg")

  expect_identical(
    vectorized$parameterSets$set,
    scalar$parameterSets$set
  )
})

test_that("addParameterEntry last-write-wins on an in-batch duplicate", {
  project <- testProject()
  suppressMessages(
    addParameterEntry(
      project,
      "dupset",
      containerPath = c("Organism|A", "Organism|A"),
      parameterName = c("K", "K"),
      value = c(1, 9),
      units = c("1/h", "1/min")
    )
  )

  # The duplicate (containerPath, parameterName) collapses to one entry, last
  # value winning, matching the scalar last-write-wins semantics.
  expect_length(project$parameterSets$dupset, 1L)
  expect_identical(project$parameterSets$dupset[[1]]$value, 9)
  expect_identical(project$parameterSets$dupset[[1]]$units, "1/min")
})

test_that("addParameterEntry aborts on mismatched vector lengths", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addParameterEntry(
      project,
      "set",
      containerPath = c("Organism|A", "Organism|B"),
      parameterName = "K",
      value = c(1, 2),
      units = "1/h"
    )
  )
})

test_that("a single scalar addParameterEntry still works (regression)", {
  project <- testProject()
  suppressMessages(
    addParameterEntry(project, "scalarset", "Organism|A", "K", 1.5, "1/h")
  )
  expect_length(project$parameterSets$scalarset, 1L)
  expect_identical(
    project$parameterSets$scalarset[[1]]$containerPath,
    "Organism|A"
  )
})

# A parameter set with many entries built in ONE vectorized call writes the set
# file exactly once. The per-call write-through re-encodes the whole growing
# set file, so an N-call buildup loop is O(N^2); the vectorized call collapses
# it to a single write. The prior linear-scaling regression test only exercised
# adding many DISTINCT sets (one entry each), so it never guarded this growing-
# set buildup case.
test_that("a vectorized bulk add of many entries is fast (one write)", {
  skip_on_cran()
  project <- testProject()

  n <- 1000L
  elapsed <- system.time(
    suppressMessages(
      addParameterEntry(
        project,
        "bulk",
        containerPath = paste0("Organism|A", seq_len(n)),
        parameterName = paste0("P", seq_len(n)),
        value = as.double(seq_len(n)),
        units = rep("1/h", n)
      )
    )
  )[["elapsed"]]

  expect_length(project$parameterSets$bulk, n)
  # One write of the whole set is well under a second on any machine; the old
  # per-call loop took ~26s for the same 1000 entries. A generous ceiling keeps
  # the test robust to machine noise while still failing on a regression back
  # to per-entry writes.
  expect_lt(elapsed, 5)
})

test_that("removeParameterEntry accepts parallel vectors", {
  project <- testProject()
  suppressMessages(
    addParameterEntry(
      project,
      "rset",
      containerPath = c("Organism|A", "Organism|B", "Organism|C"),
      parameterName = c("Ka", "Kb", "Kc"),
      value = c(1, 2, 3),
      units = c("1/h", "1/h", "1/h")
    )
  )

  removeParameterEntry(
    project,
    "rset",
    containerPath = c("Organism|A", "Organism|C"),
    parameterName = c("Ka", "Kc")
  )

  expect_length(project$parameterSets$rset, 1L)
  reloaded <- loadProject(project$jsonPath)
  expect_length(reloaded$parameterSets$rset, 1L)
  expect_identical(
    reloaded$parameterSets$rset[[1]]$parameterName,
    "Kb"
  )
})

test_that("removeParameterEntry auto-removes an emptied parameter set", {
  project <- testProject()
  suppressMessages(
    addParameterEntry(project, "tempset", "Organism|A", "K", 1.5, "1/h")
  )
  expect_true("tempset" %in% names(project$parameterSets))

  removeParameterEntry(project, "tempset", "Organism|A", "K")
  expect_false("tempset" %in% names(project$parameterSets))
})

# On-disk delete / nested-record-update write-through ----

test_that("removeParameterSet deletes the entity file and persists to disk", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "parameter-sets")
  suppressMessages(
    addParameterEntry(project, "tempset", "Organism|A", "K", 1.5, "1/h")
  )
  expect_true(file.exists(file.path(dir, "tempset.json")))

  removeParameterSet(project, "tempset")

  # In memory gone, entity file deleted, and absent from a fresh load.
  expect_false("tempset" %in% names(project$parameterSets))
  expect_false(file.exists(file.path(dir, "tempset.json")))
  reloaded <- loadProject(project$jsonPath)
  expect_false("tempset" %in% names(reloaded$parameterSets))
})

test_that("removeParameterEntry updates the on-disk set when entries remain", {
  project <- testProject()
  dir <- file.path(project$projectDirPath, "definitions", "parameter-sets")
  suppressMessages(
    addParameterEntry(project, "tempset", "Organism|A", "K", 1.5, "1/h")
  )
  addParameterEntry(project, "tempset", "Organism|B", "L", 2.5, "1/h")

  removeParameterEntry(project, "tempset", "Organism|A", "K")

  # The set survives with one entry; the on-disk file and a fresh load both
  # reflect the removal of the single entry.
  expect_length(project$parameterSets$tempset, 1L)
  reloaded <- loadProject(project$jsonPath)
  expect_length(reloaded$parameterSets$tempset, 1L)
  expect_identical(
    reloaded$parameterSets$tempset[[1]]$containerPath,
    "Organism|B"
  )
})

test_that("removeParameterEntry no-op on a missing entry does not mark modified", {
  project <- testProject()
  suppressMessages(
    addParameterEntry(project, "mset", "Organism|A", "K", 1.5, "1/h")
  )
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  expect_warning(
    removeParameterEntry(project, "mset", "Organism|A", "Ghost"),
    "not found"
  )

  expect_true(project$validatedSinceMutation)
})

test_that("the three former parameter-set kinds are merged into project$parameterSets", {
  project <- testProject()
  # The TestProject fixture has model sets (global, aciclovir), one individual
  # set (indiv1_default), and one application set (aciclovir_iv_250mg_default);
  # all live under the single parameterSets section now.
  expect_setequal(
    names(project$parameterSets),
    c("global", "aciclovir", "indiv1_default", "aciclovir_iv_250mg_default")
  )
})

# Vectorized addParameterSet / removeParameterSet ----

test_that("addParameterSet adds N sets in one write-through", {
  project <- testProject()
  addParameterSet(project, c("setA", "setB"))
  expect_true(all(c("seta", "setb") %in% names(project$parameterSets)))
  reloaded <- loadProject(project$jsonPath)
  expect_true(all(c("seta", "setb") %in% names(reloaded$parameterSets)))
})

test_that("addParameterSet aborts the whole batch on a clash and writes nothing", {
  project <- testProject()
  before <- names(project$parameterSets)
  expect_error(addParameterSet(project, c("newone", "global")))
  expect_identical(names(project$parameterSets), before)
})

test_that("removeParameterSet removes a vector of ids in one write-through", {
  project <- testProject()
  addParameterSet(project, c("a", "b"))
  removeParameterSet(project, c("a", "b"))
  expect_false(any(c("a", "b") %in% names(project$parameterSets)))
  reloaded <- loadProject(project$jsonPath)
  expect_false(any(c("a", "b") %in% names(reloaded$parameterSets)))
})

test_that("removeParameterSet warns when still referenced by a scenario, removes anyway", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  # `global` is a `modelParameterSets` entry of every scenario in the fixture.
  expect_snapshot(removeParameterSet(project, "global"))
  expect_false("global" %in% names(project$parameterSets))
})

test_that("removeParameterSet warns when still referenced by an individual, removes anyway", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  # `indiv1_default` is a `parameterSets` entry of individual `indiv1`, so this
  # exercises the individual-holder branch of the still-referenced scan.
  expect_snapshot(removeParameterSet(project, "indiv1_default"))
  expect_false("indiv1_default" %in% names(project$parameterSets))
})

# Print method ----

test_that("print.ParameterSet renders the entry count and a compact table", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$parameterSets[["global"]]))
})

test_that("print.ParameterSet renders an empty set", {
  project <- testProject()
  addParameterSet(project, "emptyset")
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$parameterSets[["emptyset"]]))
})

test_that("a classed ParameterSet still behaves as a list", {
  project <- testProject()
  set <- project$parameterSets[["global"]]
  expect_type(set, "list")
  expect_gt(length(set), 0L)
  expect_true(is.list(set[[1]]))
})

# initialConditions CRUD ----

test_that("addInitialConditions creates a set; removeInitialConditions drops it", {
  project <- testProject()
  addInitialConditions(project, "newset")
  expect_true("newset" %in% names(project$initialConditions))

  removeInitialConditions(project, "newset")
  expect_false("newset" %in% names(project$initialConditions))
})

test_that("addInitialConditions canonicalizes its id", {
  project <- testProject()
  expect_snapshot(addInitialConditions(project, "New Set"))
  expect_true("new set" %in% names(project$initialConditions))
})

test_that("addInitialConditions aborts on a duplicate id", {
  project <- testProject()
  addInitialConditions(project, "dupset")
  expect_snapshot(error = TRUE, addInitialConditions(project, "dupset"))
})

test_that("addInitialConditions aborts on a duplicate id in the batch", {
  project <- testProject()
  expect_snapshot(error = TRUE, addInitialConditions(project, c("a", "a")))
})

test_that("addInitialConditionEntry creates the set on demand and appends", {
  project <- testProject()
  expect_snapshot(
    addInitialConditionEntry(project, "tempset", "Organism|A", 1.5, "mg/l")
  )
  expect_true("tempset" %in% names(project$initialConditions))
  expect_length(project$initialConditions$tempset, 1L)

  expect_no_message(
    addInitialConditionEntry(project, "tempset", "Organism|B", 2.5, "mg/l")
  )
  expect_length(project$initialConditions$tempset, 2L)
})

test_that("a vectorized addInitialConditionEntry equals three scalar adds", {
  project <- testProject()
  suppressMessages(
    addInitialConditionEntry(
      project,
      "vecset",
      path = c("Organism|A", "Organism|B", "Organism|C"),
      value = c(1, 2, 3),
      unit = c("mg/l", "mg/l", "µmol/l")
    )
  )

  scalar <- testProject()
  suppressMessages(
    addInitialConditionEntry(scalar, "vecset", "Organism|A", 1, "mg/l")
  )
  addInitialConditionEntry(scalar, "vecset", "Organism|B", 2, "mg/l")
  addInitialConditionEntry(scalar, "vecset", "Organism|C", 3, "µmol/l")

  expect_identical(
    unclass(project$initialConditions$vecset),
    unclass(scalar$initialConditions$vecset)
  )
})

test_that("addInitialConditionEntry last-write-wins on an in-batch duplicate", {
  project <- testProject()
  suppressMessages(
    addInitialConditionEntry(
      project,
      "dset",
      path = c("Organism|A", "Organism|A"),
      value = c(1, 9),
      unit = c("mg/l", "mg/l")
    )
  )
  expect_length(project$initialConditions$dset, 1L)
  expect_identical(project$initialConditions$dset[[1]]$value, 9)
})

test_that("addInitialConditionEntry aborts on mismatched vector lengths", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addInitialConditionEntry(
      project,
      "set",
      path = c("Organism|A", "Organism|B"),
      value = 1,
      unit = "mg/l"
    )
  )
})

test_that("addInitialConditionEntry aborts on a blank unit (units are mandatory)", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addInitialConditionEntry(project, "set", "Organism|A", 1.5, "")
  )
})

test_that("addInitialConditionEntry writes the set to disk once", {
  project <- testProject()
  dir <- file.path(
    project$projectDirPath,
    "definitions",
    "initial-conditions"
  )
  suppressMessages(
    addInitialConditionEntry(project, "diskset", "Organism|A", 1.5, "mg/l")
  )
  expect_true(file.exists(file.path(dir, "diskset.json")))
  reloaded <- loadProject(project$jsonPath)
  expect_length(reloaded$initialConditions$diskset, 1L)
  expect_identical(reloaded$initialConditions$diskset[[1]]$path, "Organism|A")
})

test_that("removeInitialConditionEntry auto-removes an emptied set", {
  project <- testProject()
  suppressMessages(
    addInitialConditionEntry(project, "tempset", "Organism|A", 1.5, "mg/l")
  )
  removeInitialConditionEntry(project, "tempset", "Organism|A")
  expect_false("tempset" %in% names(project$initialConditions))
})

test_that("removeInitialConditionEntry updates the on-disk set when entries remain", {
  project <- testProject()
  suppressMessages(
    addInitialConditionEntry(project, "tempset", "Organism|A", 1.5, "mg/l")
  )
  addInitialConditionEntry(project, "tempset", "Organism|B", 2.5, "mg/l")

  removeInitialConditionEntry(project, "tempset", "Organism|A")

  expect_length(project$initialConditions$tempset, 1L)
  reloaded <- loadProject(project$jsonPath)
  expect_length(reloaded$initialConditions$tempset, 1L)
  expect_identical(
    reloaded$initialConditions$tempset[[1]]$path,
    "Organism|B"
  )
})

test_that("addInitialConditions adds N sets in one write-through", {
  project <- testProject()
  addInitialConditions(project, c("setA", "setB"))
  expect_true(all(c("seta", "setb") %in% names(project$initialConditions)))
  reloaded <- loadProject(project$jsonPath)
  expect_true(all(c("seta", "setb") %in% names(reloaded$initialConditions)))
})

test_that("removeInitialConditionEntry no-op on a missing entry warns", {
  project <- testProject()
  suppressMessages(
    addInitialConditionEntry(project, "mset", "Organism|A", 1.5, "mg/l")
  )
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  expect_warning(
    removeInitialConditionEntry(project, "mset", "Organism|Ghost"),
    "not found"
  )

  expect_length(project$initialConditions$mset, 1L)

  expect_true(project$validatedSinceMutation)
})

test_that("removeInitialConditions warns when still referenced by a scenario, removes anyway", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  addInitialConditions(project, "refset")
  setScenario(project, "testscenario", initialConditions = "refset")
  expect_snapshot(removeInitialConditions(project, "refset"))
  expect_false("refset" %in% names(project$initialConditions))
})

# Print method ----

test_that("print.InitialConditionSet renders the entry count and a compact table", {
  project <- testProject()
  suppressMessages(
    addInitialConditionEntry(
      project,
      "printset",
      path = c("Organism|A", "Organism|B"),
      value = c(1.5, 0.5),
      unit = c("mg/l", "µmol/l")
    )
  )
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$initialConditions[["printset"]]))
})

test_that("print.InitialConditionSet renders a unit-less entry", {
  # A hand-edited entity file can carry a record with no unit; the print method
  # must still render it (blank-unit branch). Build the set directly since the
  # authoring API requires a unit.
  set <- esqlabsR:::.asInitialConditionSet(list(
    list(path = "Organism|A", value = 1.5, unit = NULL)
  ))
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(set))
})

test_that("print.InitialConditionSet renders an empty set", {
  project <- testProject()
  addInitialConditions(project, "emptyset")
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$initialConditions[["emptyset"]]))
})

# readInitialConditionsFromXLS ----

test_that("`readInitialConditionsFromXLS()` reads a valid sheet and builds molecule paths", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  initialValues <- readInitialConditionsFromXLS(
    filePath = initialConditionsXLSpath,
    sheets = "ValidSheet"
  )

  expect_true(.validateParametersStructure(initialValues))
  expect_setequal(
    initialValues$paths,
    c("Organism|Liver|A", "Organism|Liver|B")
  )
})

test_that("`readInitialConditionsFromXLS()` returns the values and units of present molecules", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  initialValues <- readInitialConditionsFromXLS(
    filePath = initialConditionsXLSpath,
    sheets = "ValidSheet"
  )

  idxA <- which(initialValues$paths == "Organism|Liver|A")
  idxB <- which(initialValues$paths == "Organism|Liver|B")
  expect_equal(initialValues$values[[idxA]], 0.5)
  expect_equal(initialValues$values[[idxB]], 1.0)
  expect_equal(initialValues$units[[idxA]], "µmol")
  expect_equal(initialValues$units[[idxB]], "µmol")
})

test_that("`readInitialConditionsFromXLS()` ignores rows where 'Is Present' is FALSE but keeps NA", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  initialValues <- readInitialConditionsFromXLS(
    filePath = initialConditionsXLSpath,
    sheets = "ValidSheet"
  )

  expect_false("Organism|Kidney|A" %in% initialValues$paths)
  expect_true("Organism|Liver|B" %in% initialValues$paths)
})

test_that("`readInitialConditionsFromXLS()` uses the first sheet when no sheet is provided", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  initialValues <- readInitialConditionsFromXLS(
    filePath = initialConditionsXLSpath
  )

  expect_setequal(
    initialValues$paths,
    c("Organism|Liver|A", "Organism|Liver|B")
  )
})

test_that("`readInitialConditionsFromXLS()` warns and overwrites a path repeated across sheets", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  expect_snapshot(
    initialValues <- readInitialConditionsFromXLS(
      filePath = initialConditionsXLSpath,
      sheets = c("ValidSheet", "SecondSheet")
    )
  )

  idxA <- which(initialValues$paths == "Organism|Liver|A")
  expect_equal(initialValues$values[[idxA]], 2.5)
  expect_length(idxA, 1)
})

test_that("`readInitialConditionsFromXLS()` returns empty structure when all molecules are absent", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  initialValues <- readInitialConditionsFromXLS(
    filePath = initialConditionsXLSpath,
    sheets = "AllAbsent"
  )

  expect_true(.validateParametersStructure(initialValues))
  expect_length(initialValues$paths, 0)
  expect_length(initialValues$values, 0)
  expect_length(initialValues$units, 0)
})

test_that("`readInitialConditionsFromXLS()` errors when units are missing for a present molecule", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  expect_snapshot(
    error = TRUE,
    readInitialConditionsFromXLS(
      filePath = initialConditionsXLSpath,
      sheets = "MissingUnits"
    )
  )
})

test_that("`readInitialConditionsFromXLS()` errors when a value is missing for a present molecule", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  expect_snapshot(
    error = TRUE,
    readInitialConditionsFromXLS(
      filePath = initialConditionsXLSpath,
      sheets = "MissingValue"
    )
  )
})

test_that("`readInitialConditionsFromXLS()` errors on a non-logical 'Is Present' value", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  expect_snapshot(
    error = TRUE,
    readInitialConditionsFromXLS(
      filePath = initialConditionsXLSpath,
      sheets = "BadIsPresent"
    )
  )
})

test_that("`readInitialConditionsFromXLS()` accepts numeric 0/1 for 'Is Present'", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  initialValues <- readInitialConditionsFromXLS(
    filePath = initialConditionsXLSpath,
    sheets = "NumericIsPresent"
  )

  expect_setequal(initialValues$paths, "Organism|Liver|A")
  expect_equal(initialValues$values[[1]], 0.5)
})

test_that("`readInitialConditionsFromXLS()` warns and keeps the last value for a duplicate path", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  expect_snapshot(
    initialValues <- readInitialConditionsFromXLS(
      filePath = initialConditionsXLSpath,
      sheets = "DuplicatePath"
    )
  )

  expect_setequal(initialValues$paths, "Organism|Liver|A")
  expect_equal(initialValues$values[[1]], 2.5)
})

test_that("`readInitialConditionsFromXLS()` errors when a present row has a blank container path", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  expect_snapshot(
    error = TRUE,
    readInitialConditionsFromXLS(
      filePath = initialConditionsXLSpath,
      sheets = "BlankPath"
    )
  )
})

test_that("`readInitialConditionsFromXLS()` errors when a sheet has the wrong structure", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  expect_snapshot(
    error = TRUE,
    readInitialConditionsFromXLS(
      filePath = initialConditionsXLSpath,
      sheets = "InvalidSheet"
    )
  )
})

test_that("`readInitialConditionsFromXLS()` validates its arguments", {
  initialConditionsXLSpath <- getTestDataFilePath("InitialConditions.xlsx")
  expect_error(
    readInitialConditionsFromXLS(filePath = 123),
    regexp = 'argument "filePath" is of type <numeric>, but expected <character>!',
    fixed = TRUE
  )
  expect_error(
    readInitialConditionsFromXLS(
      filePath = initialConditionsXLSpath,
      sheets = 123
    ),
    regexp = 'argument "sheets" is of type <numeric>, but expected <character>!',
    fixed = TRUE
  )
})

# isTableFormulasEqual ----

# `isTableFormulasEqual()` only reads `$allPoints` and each point's `$x`/`$y`.
# A real `TableFormula` requires a live PK-Sim/.NET object (no exported
# constructor), so we exercise the comparison with lightweight stubs that mirror
# that interface, keeping the test self-contained.
test_that("isTableFormulasEqual compares every point, not just the first", {
  tableFormula <- function(...) {
    list(allPoints = lapply(list(...), \(p) list(x = p[[1]], y = p[[2]])))
  }

  f1 <- tableFormula(c(0, 1), c(10, 2), c(30, 3))
  # Differs from f1 only in the last point; the old loop returned after point 1
  # and wrongly reported these as equal.
  fLateDiff <- tableFormula(c(0, 1), c(10, 2), c(30, 99))

  expect_false(isTableFormulasEqual(f1, fLateDiff))
  expect_true(isTableFormulasEqual(f1, f1))

  # Two empty table formulas are equal (old code fell through and returned NULL).
  expect_true(isTableFormulasEqual(tableFormula(), tableFormula()))

  # Differing lengths are never equal.
  expect_false(isTableFormulasEqual(f1, tableFormula(c(0, 1))))
})

# setParameterValuesByPathWithCondition ----

# The length guard fires before any ospsuite/simulation call, so it can be
# exercised without a live Simulation (passing `simulation = NULL` is safe: the
# abort happens first).
test_that("setParameterValuesByPathWithCondition aborts on a values length mismatch", {
  expect_snapshot(
    error = TRUE,
    setParameterValuesByPathWithCondition(
      parameterPaths = c("Organism|Liver|Volume", "Organism|Volume"),
      values = c(1, 2, 3),
      simulation = NULL
    )
  )
})

test_that("setParameterValuesByPathWithCondition aborts on a units length mismatch", {
  expect_snapshot(
    error = TRUE,
    setParameterValuesByPathWithCondition(
      parameterPaths = c("Organism|Liver|Volume", "Organism|Volume"),
      values = c(1, 2),
      simulation = NULL,
      units = c("l", "l", "l")
    )
  )
})
