test_that("Simulation time can be initialized with arguments", {
  expect_no_error({
    SimulationTime$new(
      simulationTime = "0, 24, 60",
      simulationTimeUnit = "h"
    )
  })
})

test_that("Wrong simumationTimeUnit returns an error", {
  expect_error(
    {
      SimulationTime$new(
        simulationTime = "0, 24, 60",
        simulationTimeUnit = "µM"
      )
    },
    regexp = '`simulationTimeUnit` must be one of "s", "min".*'
  )
})

test_that("Wrong simulationTime format or logic returns an error", {
  # Negative values
  expect_error(
    {
      SimulationTime$new(
        simulationTime = "-1, 24, 60",
        simulationTimeUnit = "hour"
      )
    },
    regexp = "All values in the simulation time must be positive.*"
  )

  # More than 3 values
  expect_error(
    {
      SimulationTime$new(
        simulationTime = "0, 24, 60, 120",
        simulationTimeUnit = "hour"
      )
    },
    regexp = "The simulation time must be a string of three values separated by commas.*"
  )


  # Resolution is 0
  expect_error(
    {
      SimulationTime$new(
        simulationTime = "0, 24, 0",
        simulationTimeUnit = "hour"
      )
    },
    regexp = "The resolution must be greater than 0.*"
  )

  # first value must be lower than second value
  expect_error(
    {
      SimulationTime$new(
        simulationTime = "24, 0, 60",
        simulationTimeUnit = "hour"
      )
    },
    regexp = "The start time must be smaller than the end time.*"
  )
})

test_that("Simulation Time values are correctly extracted", {
  simTime <- SimulationTime$new(
    simulationTime = "0, 24, 60",
    simulationTimeUnit = "h"
  )

  expect_equal(simTime$startTime, 0)
  expect_equal(simTime$endTime, 24)
  expect_equal(simTime$resolution, 60)
})

test_that("Time Points are correctly computed", {
  simTime <- SimulationTime$new(
    simulationTime = "0, 24, 1",
    simulationTimeUnit = "h"
  )

  expect_equal(simTime$timePoints, 0:24)

  simTime <- SimulationTime$new(
    simulationTime = "0, 2, 0.5",
    simulationTimeUnit = "h"
  )

  expect_equal(simTime$timePoints, c(0, 2))

  # high number of points
  simTime <- SimulationTime$new(
    simulationTime = "0, 24, 100",
    simulationTimeUnit = "h"
  )
  expect_equal(length(simTime$timePoints), 2401)
})
