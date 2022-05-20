## context("DataMapping")

sim <- loadTestSimulation("Aciclovir")
outputPaths <- c("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)")
addOutputs(quantitiesOrPaths = outputPaths, simulation = sim)
simResults <- runSimulation(simulation = sim)

dataFolderPath <- getTestDataFilePath("")
dataConfiguration <- DataConfiguration$new(
  dataFolder = dataFolderPath,
  dataFile = "CompiledDataSet.xlsx",
  compoundPropertiesFile = "Compound_Properties.xlsx",
  dataSheets = c("TestSheet_1")
)
observedData <- readOSPSTimeValues(dataConfiguration = dataConfiguration)

test_that("It can create a DataMapping object", {
  dataMapping <- DataMapping$new()
  expect_equal(dataMapping$addLegend, TRUE)
  expect_equal(dataMapping$legendPosition, "topright")
  expect_error(capture.output(print(dataMapping)), NA)
})

test_that("It can add a single xy-values series without error", {
  dataMapping <- DataMapping$new()
  expect_equal(dataMapping$xySeriesCount, 0)
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)
  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series"
  )
  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeriesCount, 1)
  expect_equal(dataMapping$xySeries[["my series"]]$xValues, xVals)
  expect_equal(dataMapping$xySeries[["my series"]]$yValues, yVals)
  expect_equal(dataMapping$xySeries[["my series"]]$yError, NULL)
  expect_equal(length(dataMapping$groupings), 0)
  expect_equal(dataMapping$ungroupedSeries[[1]], "my series")
})

test_that("It can add a single xy-values series with error", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)
  yErr <- c(0.1, 0.1, 0.1, 0.2)
  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = "my series"
  )
  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeriesCount, 1)
  expect_equal(dataMapping$xySeries[["my series"]]$xValues, xVals)
  expect_equal(dataMapping$xySeries[["my series"]]$yValues, yVals)
  expect_equal(dataMapping$xySeries[["my series"]]$yError, yErr)
  expect_equal(length(dataMapping$groupings), 0)
  expect_equal(dataMapping$ungroupedSeries[[1]], "my series")
})

test_that("It can add a list of xy-values with error", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))
  yErr <- c(0.1, 0.1, 0.1, 0.2)

  # Error when the list of errors is not equal the list of values
  expect_error(dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  ))

  yErr <- list(c(0.1, 0.1, 0.1, 0.2), NULL, c(0.2, 0.3, 0.1, 0.2))
  expect_error(dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = list(
      "my series1",
      "my series2"
    )
  ))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeriesCount, 3)
  expect_equal(dataMapping$ungroupedSeries[[3]], "my series3")
})

test_that("It can add a list of xy-values with groupings", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), NULL, c(0.2, 0.3, 0.1, 0.2))
  groups <- list("Group1", "Group1")

  # Error when the number of specified groups does not equal the number of data sets
  expect_error(dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    ),
    groups = groups
  ))

  groups <- list("Group1", NULL, "Group1")
  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    ),
    groups = groups
  )

  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeriesCount, 3)
  expect_equal(dataMapping$ungroupedSeries[[1]], "my series2")
  expect_equal(dataMapping$groupings[["Group1"]], c("my series1", "my series3"))
})

test_that("It can change the grouping of a data set", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), NULL, c(0.2, 0.3, 0.1, 0.2))
  groups <- list("Group1", NULL, "Group1")

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    ),
    groups = groups
  )

  expect_equal(dataMapping$ungroupedSeries[[1]], "my series2")
  expect_equal(dataMapping$groupings[["Group1"]], c("my series1", "my series3"))

  # my series2 added to group2
  dataMapping$addXYSeries(
    xValsList = c(1, 2, 3),
    yValsList = c(2, 3, 4),
    labels = "my series2",
    groups = "Group2"
  )
  expect_equal(dataMapping$ungroupedSeries, list())
  expect_equal(dataMapping$groupings[["Group1"]], c("my series1", "my series3"))
  expect_equal(dataMapping$groupings[["Group2"]], c("my series2"))

  # my series1 without group
  dataMapping$addXYSeries(
    xValsList = c(1, 2, 3),
    yValsList = c(2, 3, 4),
    labels = "my series1"
  )
  expect_equal(dataMapping$ungroupedSeries[[1]], "my series1")
  expect_equal(dataMapping$groupings[["Group1"]], c("my series3"))
  expect_equal(dataMapping$groupings[["Group2"]], c("my series2"))
})

test_that("It can add one observed data without a group", {
  obsData <- observedData$TestSheet_1$Male$Ind1$iv$Dapagliflozin$PeripheralVenousBlood$Plasma
  dataMapping <- DataMapping$new()
  dataMapping$addXYData(XYData = obsData)
  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeries[[1]]$label, "TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$groupings, list())
  expect_equal(dataMapping$ungroupedSeries[[1]], "TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
})

test_that("It can add one observed data with a group", {
  obsData <- observedData$TestSheet_1$Male$Ind1$iv$Dapagliflozin$PeripheralVenousBlood$Plasma
  dataMapping <- DataMapping$new()
  dataMapping$addXYData(XYData = obsData, groups = "Group1")
  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeries[[1]]$label, "TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$groupings[["Group1"]], "TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$ungroupedSeries, list())
})

test_that("It can add multiple observed data without a group", {
  obsData <- list(
    observedData$TestSheet_1$Male$Ind1$iv$Dapagliflozin$PeripheralVenousBlood$Plasma,
    observedData$TestSheet_1$Female$Ind2$iv$Dapagliflozin$PeripheralVenousBlood$Plasma
  )
  dataMapping <- DataMapping$new()
  dataMapping$addXYData(XYData = obsData)
  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeries[[1]]$label, "TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$xySeries[[2]]$label, "TestSheet_1.Female.Ind2.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$groupings, list())
  expect_equal(dataMapping$ungroupedSeries[[1]], "TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
})

test_that("It can add multiple observed data with a group", {
  obsData <- list(
    observedData$TestSheet_1$Male$Ind1$iv$Dapagliflozin$PeripheralVenousBlood$Plasma,
    observedData$TestSheet_1$Female$Ind2$iv$Dapagliflozin$PeripheralVenousBlood$Plasma
  )
  dataMapping <- DataMapping$new()
  dataMapping$addXYData(XYData = obsData, groups = list(NULL, "Group1"))
  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeries[[1]]$label, "TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$xySeries[[2]]$label, "TestSheet_1.Female.Ind2.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$groupings[[1]], "TestSheet_1.Female.Ind2.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$ungroupedSeries[[1]], "TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
})

test_that("It can remove xySeries", {
  obsData <- list(
    observedData$TestSheet_1$Male$Ind1$iv$Dapagliflozin$PeripheralVenousBlood$Plasma,
    observedData$TestSheet_1$Female$Ind2$iv$Dapagliflozin$PeripheralVenousBlood$Plasma
  )
  dataMapping <- DataMapping$new()
  dataMapping$addXYData(XYData = obsData, groups = list(NULL, "Group1"))
  dataMapping$removeXYSeries("TestSheet_1.Male.Ind1.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$xySeries[[1]]$label, "TestSheet_1.Female.Ind2.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$groupings[[1]], "TestSheet_1.Female.Ind2.iv.Dapagliflozin.PeripheralVenousBlood.Plasma")
  expect_equal(dataMapping$ungroupedSeries, list())
})

test_that("It can add the same XYData to different groups", {
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  dataMapping <- DataMapping$new()
  # No group
  dataMapping$addModelOutputs(paths = outputPath, labels = "Sim output", simulationResults = simResults)
  dataMapping$addModelOutputs(paths = outputPath, labels = "Sim output", simulationResults = simResults)

  # Specified group
  dataMapping$addModelOutputs(
    paths = outputPath, labels = "Sim output", simulationResults = simResults,
    groups = "myGroup"
  )
  dataMapping$addModelOutputs(
    paths = outputPath, labels = "Sim output", simulationResults = simResults,
    groups = "myGroup"
  )

  capture.output(print(dataMapping))
  expect_equal(dataMapping$xySeriesCount, 1)
})

test_that("It throws an error when trying to add a non-existant simulation result", {
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood"
  dataMapping <- DataMapping$new()
  expect_error(dataMapping$addModelOutputs(paths = outputPath, labels = "Sim output", simulationResults = simResults),
    regexp = escapeForRegex(messages$errorOutputPathNotFound(outputPath))
  )
})

test_that("It prints a warning when trying remove XYData that has not been added", {
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  dataMapping <- DataMapping$new()
  dataMapping$addModelOutputs(paths = outputPath, labels = "Sim output", simulationResults = simResults)

  expect_warning(dataMapping$removeXYSeries("foo"),
    regexp = escapeForRegex(messages$warningLabelNotInDataMapping("foo"))
  )
})

test_that("It can remove XYData that is not in a group", {
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

  dataMapping <- DataMapping$new()
  expect_equal(dataMapping$xySeriesCount, 0)
  expect_equal(length(dataMapping$groupings), 0)

  dataMapping$addModelOutputs(paths = outputPath, labels = "Sim output", simulationResults = simResults)
  expect_equal(dataMapping$xySeriesCount, 1)
  expect_equal(length(dataMapping$groupings), 0)

  dataMapping$removeXYSeries(label = "Sim output")
  expect_equal(dataMapping$xySeriesCount, 0)
  expect_equal(length(dataMapping$groupings), 0)

  dataMapping$addModelOutputs(paths = outputPath, labels = "Sim output2", simulationResults = simResults)
  expect_equal(dataMapping$xySeriesCount, 1)
  expect_equal(length(dataMapping$groupings), 0)

  expect_warning(dataMapping$removeXYSeries(label = "Sim output"))
  expect_equal(dataMapping$xySeriesCount, 1)
  expect_equal(length(dataMapping$groupings), 0)

  dataMapping$addModelOutputs(paths = outputPath, labels = "Sim output", simulationResults = simResults)
  expect_equal(dataMapping$xySeriesCount, 2)
  expect_equal(length(dataMapping$groupings), 0)

  dataMapping$addModelOutputs(
    paths = outputPath, labels = "Sim output", simulationResults = simResults,
    groups = "myGroup"
  )
  expect_equal(dataMapping$xySeriesCount, 2)
  expect_equal(length(dataMapping$groupings), 1)

  dataMapping$removeXYSeries(label = "Sim output")
  expect_equal(dataMapping$xySeriesCount, 1)
  expect_equal(length(dataMapping$groupings), 0)

  dataMapping$addModelOutputs(paths = outputPath, labels = "Sim output", simulationResults = simResults)
  expect_equal(dataMapping$xySeriesCount, 2)
  expect_equal(length(dataMapping$groupings), 0)

  dataMapping$addModelOutputs(
    paths = outputPath, labels = "Sim output", simulationResults = simResults,
    groups = "myGroup"
  )
  expect_equal(dataMapping$xySeriesCount, 2)
  expect_equal(length(dataMapping$groupings), 1)
})

## context("setXFactors")

test_that("It can change the x-factor of one xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 1)
  dataMapping$setXFactors(labels = "my series1", xFactors = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 2)
})

test_that("If the label is not present in the mapping, nothing happens", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 1)
  dataMapping$setXFactors(labels = "my series2", xFactors = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 1)
})

test_that("It can change the x-factor of multiple xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 1)
  expect_equal(dataMapping$xySeries[["my series2"]]$xFactor, 1)
  expect_equal(dataMapping$xySeries[["my series3"]]$xFactor, 1)
  dataMapping$setXFactors(labels = list("my series1", "my series2", "my series4", "my series3"), xFactors = c(2, 3, 4, 5))
  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 2)
  expect_equal(dataMapping$xySeries[["my series2"]]$xFactor, 3)
  expect_equal(dataMapping$xySeries[["my series3"]]$xFactor, 5)
})

## context("setYFactors")

test_that("It can change the y-factor of one xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 1)
  dataMapping$setYFactors(labels = "my series1", yFactors = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 2)
})

test_that("If the label is not present in the mapping, nothing happens", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 1)
  dataMapping$setYFactors(labels = "my series2", yFactors = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 1)
})

test_that("It can change the y-factor of multiple xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 1)
  expect_equal(dataMapping$xySeries[["my series2"]]$yFactor, 1)
  expect_equal(dataMapping$xySeries[["my series3"]]$yFactor, 1)
  dataMapping$setYFactors(labels = list("my series1", "my series2", "my series4", "my series3"), yFactors = c(2, 3, 4, 5))
  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 2)
  expect_equal(dataMapping$xySeries[["my series2"]]$yFactor, 3)
  expect_equal(dataMapping$xySeries[["my series3"]]$yFactor, 5)
})

## context("setXOffsets")

test_that("It can change the x-offset of one xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 0)
  dataMapping$setXOffsets(labels = "my series1", xOffsets = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 2)
})

test_that("If the label is not present in the mapping, nothing happens", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 0)
  dataMapping$setXOffsets(labels = "my series2", xOffsets = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 0)
})

test_that("It can change the x-offset of multiple xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 0)
  expect_equal(dataMapping$xySeries[["my series2"]]$xOffset, 0)
  expect_equal(dataMapping$xySeries[["my series3"]]$xOffset, 0)
  dataMapping$setXOffsets(labels = list("my series1", "my series2", "my series4", "my series3"), xOffsets = c(2, 3, 4, 5))
  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 2)
  expect_equal(dataMapping$xySeries[["my series2"]]$xOffset, 3)
  expect_equal(dataMapping$xySeries[["my series3"]]$xOffset, 5)
})

## context("setYOffsets")

test_that("It can change the y-offset of one xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 0)
  dataMapping$setYOffsets(labels = "my series1", yOffsets = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 2)
})

test_that("If the label is not present in the mapping, nothing happens", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 0)
  dataMapping$setYOffsets(labels = "my series2", yOffsets = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 0)
})

test_that("It can change the y-offset of multiple xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 0)
  expect_equal(dataMapping$xySeries[["my series2"]]$yOffset, 0)
  expect_equal(dataMapping$xySeries[["my series3"]]$yOffset, 0)
  dataMapping$setYOffsets(labels = list("my series1", "my series2", "my series4", "my series3"), yOffsets = c(2, 3, 4, 5))
  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 2)
  expect_equal(dataMapping$xySeries[["my series2"]]$yOffset, 3)
  expect_equal(dataMapping$xySeries[["my series3"]]$yOffset, 5)
})

## context("setTypes")

test_that("It can change the type of one xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$type, "p")
  dataMapping$setTypes(labels = "my series1", types = "l")
  expect_equal(dataMapping$xySeries[["my series1"]]$type, "l")
})

test_that("If the label is not present in the mapping, nothing happens", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$type, "p")
  dataMapping$setTypes(labels = "my series2", types = "l")
  expect_equal(dataMapping$xySeries[["my series1"]]$type, "p")
})

test_that("It can change the type of multiple xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$type, "p")
  expect_equal(dataMapping$xySeries[["my series2"]]$type, "p")
  expect_equal(dataMapping$xySeries[["my series3"]]$type, "p")
  dataMapping$setTypes(labels = list("my series1", "my series2", "my series4", "my series3"), types = c("l", "m", "n", "o"))
  expect_equal(dataMapping$xySeries[["my series1"]]$type, "l")
  expect_equal(dataMapping$xySeries[["my series2"]]$type, "m")
  expect_equal(dataMapping$xySeries[["my series3"]]$type, "o")
})

#### setLinetypes ####
## context("setLinetypes")

test_that("It can change the type of one xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$lty, NULL)
  dataMapping$setLinetypes(labels = "my series1", linetypes = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$lty, 2)
})

test_that("If the label is not present in the mapping, nothing happens", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$lty, NULL)
  dataMapping$setLinetypes(labels = "my series2", linetypes = 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$lty, NULL)
})

test_that("It can change the type of multiple xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$lty, NULL)
  expect_equal(dataMapping$xySeries[["my series2"]]$lty, NULL)
  expect_equal(dataMapping$xySeries[["my series3"]]$lty, NULL)
  dataMapping$setLinetypes(labels = list("my series1", "my series2", "my series4", "my series3"), linetypes = c(1, 2, 4, 3))
  expect_equal(dataMapping$xySeries[["my series1"]]$lty, 1)
  expect_equal(dataMapping$xySeries[["my series2"]]$lty, 2)
  expect_equal(dataMapping$xySeries[["my series3"]]$lty, 3)
})

## context("setColors")

test_that("It can change the color of one xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$color, NULL)
  dataMapping$setColors(labels = "my series1", colors = "red")
  expect_equal(dataMapping$xySeries[["my series1"]]$color, "red")
})

test_that("It can set NULL as the color of one xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$color, NULL)
  dataMapping$setColors(labels = "my series1", colors = "red")
  dataMapping$setColors(labels = "my series1", colors = NULL)
  expect_equal(dataMapping$xySeries[["my series1"]]$color, NULL)
})

test_that("If the label is not present in the mapping, nothing happens", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = "my series1"
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$color, NULL)
  dataMapping$setColors(labels = "my series2", colors = "red")
  expect_equal(dataMapping$xySeries[["my series1"]]$color, NULL)
})

test_that("It can change the color of multiple xy-series", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  expect_equal(dataMapping$xySeries[["my series1"]]$color, NULL)
  expect_equal(dataMapping$xySeries[["my series2"]]$color, NULL)
  expect_equal(dataMapping$xySeries[["my series3"]]$color, NULL)
  dataMapping$setColors(labels = list("my series1", "my series2", "my series4", "my series3"), colors = list("red", NULL, "green", "blue"))
  expect_equal(dataMapping$xySeries[["my series1"]]$color, "red")
  expect_equal(dataMapping$xySeries[["my series2"]]$color, NULL)
  expect_equal(dataMapping$xySeries[["my series3"]]$color, "blue")
})

## context("setConfiguration")

test_that("It can set a configuration", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
  yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    labels = list(
      "my series1",
      "my series2",
      "my series3"
    )
  )

  configuration <- DataMappingConfiguration$new()
  configuration$setColors(labels = c("my series1", "my series5"), colors = c("red", "gold"))
  configuration$setLineTypes(labels = c("my series5", "my series2"), lineTypes = c("red", "b"))
  configuration$setXFactors(labels = c("my series3", "my series5"), xFactors = c(2, 0))
  configuration$setYFactors(labels = c("my series3", "my series1"), yFactors = c(3, 2))
  configuration$setXOffsets(labels = c("my series3", "my series5"), xOffsets = c(3, 2))
  configuration$setYOffsets(labels = c("my series3", "my series2", "my series1"), yOffsets = c(3, 2, 2))

  dataMapping$setConfiguration(dataMappingConfiguration = configuration)

  expect_equal(dataMapping$xySeries[["my series1"]]$color, "red")
  expect_equal(dataMapping$xySeries[["my series2"]]$color, NULL)
  expect_equal(dataMapping$xySeries[["my series3"]]$color, NULL)

  expect_equal(dataMapping$xySeries[["my series1"]]$type, "p")
  expect_equal(dataMapping$xySeries[["my series2"]]$type, "b")
  expect_equal(dataMapping$xySeries[["my series3"]]$type, "p")

  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 1)
  expect_equal(dataMapping$xySeries[["my series2"]]$xFactor, 1)
  expect_equal(dataMapping$xySeries[["my series3"]]$xFactor, 2)

  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 2)
  expect_equal(dataMapping$xySeries[["my series2"]]$yFactor, 1)
  expect_equal(dataMapping$xySeries[["my series3"]]$yFactor, 3)

  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 0)
  expect_equal(dataMapping$xySeries[["my series2"]]$xOffset, 0)
  expect_equal(dataMapping$xySeries[["my series3"]]$xOffset, 3)

  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 2)
  expect_equal(dataMapping$xySeries[["my series2"]]$yOffset, 2)
  expect_equal(dataMapping$xySeries[["my series3"]]$yOffset, 3)
})

test_that("Setting a does not affect initial xy-data objects configuration", {
  dataMapping <- DataMapping$new()
  xVals <- c(1, 2, 3, 4)
  yVals <- c(5, 6, 7, 8)

  xyData <- XYData$new(xVals = xVals, yVals = yVals, label = "my series1")

  dataMapping$addXYData(
    XYData = xyData
  )

  configuration <- DataMappingConfiguration$new()
  configuration$setColors(labels = c("my series1"), colors = c("red"))
  configuration$setLineTypes(labels = c("my series1"), lineTypes = c("b"))
  configuration$setXFactors(labels = c("my series1"), xFactors = c(2))
  configuration$setYFactors(labels = c("my series1"), yFactors = c(3))
  configuration$setXOffsets(labels = c("my series1"), xOffsets = c(3))
  configuration$setYOffsets(labels = c("my series1"), yOffsets = c(3))

  dataMapping$setConfiguration(dataMappingConfiguration = configuration)

  expect_equal(dataMapping$xySeries[["my series1"]]$color, "red")
  expect_equal(dataMapping$xySeries[["my series1"]]$type, "b")
  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 3)
  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 3)
  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 3)

  expect_equal(xyData$color, NULL)
  expect_equal(xyData$type, "p")
  expect_equal(xyData$xFactor, 1)
  expect_equal(xyData$yFactor, 1)
  expect_equal(xyData$xOffset, 0)
  expect_equal(xyData$yOffset, 0)

  xyData$color <- "green"
  xyData$type <- "l"
  xyData$xFactor <- 10
  xyData$yFactor <- 10
  xyData$xOffset <- 10
  xyData$yOffset <- 10

  expect_equal(dataMapping$xySeries[["my series1"]]$color, "red")
  expect_equal(dataMapping$xySeries[["my series1"]]$type, "b")
  expect_equal(dataMapping$xySeries[["my series1"]]$xFactor, 2)
  expect_equal(dataMapping$xySeries[["my series1"]]$yFactor, 3)
  expect_equal(dataMapping$xySeries[["my series1"]]$xOffset, 3)
  expect_equal(dataMapping$xySeries[["my series1"]]$yOffset, 3)
})

## context("xLim")

test_that("X limit of a empty mapping is c(0, 0)", {
  dataMapping <- DataMapping$new()
  expect_equal(dataMapping$xLim, c(0, 0))
})

test_that("It returns correct xLim when unit of DataMapping is the same as unit of xyData", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  expect_equal(dataMapping$xLim, c(-1, 4) * 1.1)
})

test_that("It returns correct xLim when unit of DataMapping is different from unit of xyData", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  dataMapping$xUnit <- "h"

  expect_equal(dataMapping$xLim, c(-1, 4) / 60 * 1.1)
})

test_that("It can set xLim", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  dataMapping$xLim <- c(2, 39)

  expect_equal(dataMapping$xLim, c(2, 39))
})

test_that("It throws an error if xLim is not of a correct length", {
  dataMapping <- DataMapping$new()
  expect_error(dataMapping$xLim <- 1, regexp = messages$errorWrongLength("value", 2))
})

test_that("xLim is not changed by adding new data if set manually", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$xLim <- c(2, 39)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  expect_equal(dataMapping$xLim, c(2, 39))
})

## context("yLim")

test_that("Y limit of a empty mapping is c(0, 0)", {
  dataMapping <- DataMapping$new()
  expect_equal(dataMapping$yLim, c(0, 0))
})

test_that("It returns correct yLim in lin scale when unit of DataMapping is the same as unit of xyData", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  expect_equal(dataMapping$yLim, c(-1.1, 10.1) * 1.1)
})

test_that("It returns correct yLim in lin scale when unit of DataMapping is different from unit of xyData", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  dataMapping$yDimension <- ospDimensions$Fraction
  dataMapping$yUnit <- "%"

  expect_equal(dataMapping$yLim, c(-1.1, 10.1) * 100 * 1.1)
})

test_that("It returns correct yLim in log scale when unit of DataMapping is the same as unit of xyData", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  dataMapping$log <- "y"
  expect_equal(dataMapping$yLim, c(1.71, 11.11))

  # If the lowest positive value becomes negative when error is subtracted
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 2.1, 0.1))

  dataMapping <- DataMapping$new()
  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  dataMapping$log <- "y"
  expect_equal(dataMapping$yLim, c(4.41, 11.11))

  # No positive values - do not know what should happen
  # xVals <- list(c(1, 2, 3, 4))
  # yVals <- list(c(-1, -2, -3, -4))
  #
  # dataMapping <- DataMapping$new()
  # dataMapping$addXYSeries(
  #   xValsList = xVals,
  #   yValsList = yVals,
  #   yErrorList = NULL,
  #   labels = c("my series1")
  # )
  #
  # dataMapping$log <- "y"
  # expect_equal(dataMapping$yLim, c(4.9, 10.1))
})

test_that("It can set yLim", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  dataMapping$yLim <- c(2, 39)

  expect_equal(dataMapping$yLim, c(2, 39))
})

test_that("It throws an error if yLim is not of a correct length", {
  dataMapping <- DataMapping$new()
  expect_error(dataMapping$yLim <- 1, regexp = messages$errorWrongLength("value", 2))
})

test_that("yLim is not changed by adding new data if set manually", {
  dataMapping <- DataMapping$new()
  xVals <- list(c(1, 2, 3, 4), c(0), c(0, 2.2, -1))
  yVals <- list(c(5, 6, 7, 8), c(10), c(-1, 2, 0))
  yErr <- list(c(0.1, 0.1, 0.1, 0.2), c(0.1), c(0.1, 0.1, 0.1))

  dataMapping$yLim <- c(2, 39)

  dataMapping$addXYSeries(
    xValsList = xVals,
    yValsList = yVals,
    yErrorList = yErr,
    labels = c("my series1", "my series2", "my series3")
  )

  expect_equal(dataMapping$yLim, c(2, 39))
})

## context("xDimension xUnit")

test_that("Dimension and unit of an empty DataMapping are NULL", {
  dataMapping <- DataMapping$new()
  expect_null(dataMapping$xDimension)
  expect_null(dataMapping$xUnit)
})

test_that("xDimension is set to the dimension of the first added XYData", {
  dataMapping <- DataMapping$new()

  xyData1 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData1")
  xyData2 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData2")

  dataMapping$addXYData(XYData = c(xyData1, xyData2))
  expect_equal(dataMapping$xDimension, ospDimensions$Time)
  expect_equal(dataMapping$xUnit, getBaseUnit(ospDimensions$Time))

  # Dimension of the first data set is different from the dimension of the second data set
  dataMapping <- DataMapping$new()
  xyData1$xDimension <- ospDimensions$Fraction
  dataMapping$addXYData(XYData = c(xyData1, xyData2))
  expect_equal(dataMapping$xDimension, ospDimensions$Fraction)
  expect_equal(dataMapping$xUnit, getBaseUnit(ospDimensions$Fraction))
})

test_that("It can change xDimension", {
  dataMapping <- DataMapping$new()

  xyData1 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData1")
  xyData2 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData2")

  dataMapping$addXYData(XYData = c(xyData1, xyData2))
  expect_equal(dataMapping$xDimension, ospDimensions$Time)
  expect_equal(dataMapping$xUnit, getBaseUnit(ospDimensions$Time))

  dataMapping$xDimension <- ospDimensions$`Abundance per mass protein`
  expect_equal(dataMapping$xDimension, ospDimensions$`Abundance per mass protein`)
  expect_equal(dataMapping$xUnit, getBaseUnit(ospDimensions$`Abundance per mass protein`))
})

test_that("It can change xUnit", {
  dataMapping <- DataMapping$new()

  xyData1 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData1")
  xyData2 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData2")

  dataMapping$addXYData(XYData = c(xyData1, xyData2))
  expect_equal(dataMapping$xDimension, ospDimensions$Time)
  expect_equal(dataMapping$xUnit, getBaseUnit(ospDimensions$Time))

  dataMapping$xUnit <- ospUnits$Time$`day(s)`
  expect_equal(dataMapping$xUnit, ospUnits$Time$`day(s)`)
})

## context("yDimension yUnit")

test_that("Dimension and unit of an empty DataMapping are NULL", {
  dataMapping <- DataMapping$new()
  expect_null(dataMapping$yDimension)
  expect_null(dataMapping$yUnit)
})

test_that("yDimension is set to the dimension of the first added XYData", {
  dataMapping <- DataMapping$new()

  xyData1 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData1")
  xyData2 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData2")

  dataMapping$addXYData(XYData = c(xyData1, xyData2))
  expect_equal(dataMapping$yDimension, ospDimensions$Dimensionless)
  expect_equal(dataMapping$yUnit, getBaseUnit(ospDimensions$Dimensionless))

  # Dimension of the first data set is different from the dimension of the second data set
  dataMapping <- DataMapping$new()
  xyData1$yDimension <- ospDimensions$`Concentration (mass)`
  dataMapping$addXYData(XYData = c(xyData1, xyData2))
  expect_equal(dataMapping$yDimension, ospDimensions$`Concentration (mass)`)
  expect_equal(dataMapping$yUnit, getBaseUnit(ospDimensions$`Concentration (mass)`))
})

test_that("It can change yDimension", {
  dataMapping <- DataMapping$new()

  xyData1 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData1")
  xyData2 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData2")

  dataMapping$addXYData(XYData = c(xyData1, xyData2))
  dataMapping$yDimension <- ospDimensions$`Abundance per mass protein`
  expect_equal(dataMapping$yDimension, ospDimensions$`Abundance per mass protein`)
  expect_equal(dataMapping$yUnit, getBaseUnit(ospDimensions$`Abundance per mass protein`))
})

test_that("It can change yUnit", {
  dataMapping <- DataMapping$new()

  xyData1 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData1")
  xyData2 <- XYData$new(xVals = c(1, 2, 3), yVals = c(1, 2, 3), label = "xyData2")

  dataMapping$addXYData(XYData = c(xyData1, xyData2))
  dataMapping$yDimension <- ospDimensions$`Abundance per mass protein`
  dataMapping$yUnit <- ospUnits$`Abundance per mass protein`$`nmol/mg mic. protein`
  expect_equal(dataMapping$yUnit, ospUnits$`Abundance per mass protein`$`nmol/mg mic. protein`)
})

## context("addDataSets")
test_that("It can add one DataSet with a group", {
  file <- getTestDataFilePath("ObsDataAciclovir_1.pkml")
  obsData <- ospsuite::loadDataSetFromPKML(filePath = file)
  dataMapping <- DataMapping$new()

  dataMapping$addDataSets(dataSets = obsData, groups = "Group1")

  xyData <- dataMapping$xySeries[[obsData$name]]
  expect_equal(xyData$label, obsData$name)
  expect_equal(xyData$xValues, obsData$xValues)
  expect_equal(xyData$yValues, obsData$yValues)
  expect_equal(xyData$yError, obsData$yErrorValues)

  expect_equal(xyData$dataType, XYDataTypes$Observed)
  expect_equal(xyData$MW, obsData$molWeight)
  expect_equal(xyData$xDimension, obsData$xDimension)
  expect_equal(xyData$xUnit, obsData$xUnit)
  expect_equal(xyData$yDimension, obsData$yDimension)
  expect_equal(xyData$yUnit, obsData$yUnit)
  # the loaded observed data has no error and therefore no unit.
  # Skip this use case for now
  # expect_equal(xyData$yErrorUnit, obsData$yErrorUnit)

  expect_equal(xyData$getAllMetaData(), obsData$metaData)
  capture.output(print(dataMapping))
  expect_equal(dataMapping$groupings[["Group1"]], xyData$label)
  expect_equal(dataMapping$ungroupedSeries, list())
})
