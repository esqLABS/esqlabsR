## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, results = "hide", message = F-----------------------------
library(esqlabsR)

## ----createXYData-------------------------------------------------------------
xVals <- c(0, 1, 2, 3)
yVals <- c(4, 5, 6, 7)
yError <- c(0.1, 0, 0.1, 1)

# Create new object of type XYData
xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
print(xyData)

## ----accessValues-------------------------------------------------------------
xyData$xValues
xyData$yValues
xyData$yError

## ----xyMinMax-----------------------------------------------------------------
xyData$xMax
xyData$xMin
xyData$yMax # Maximal value of 7 plus the error 1
xyData$yMin # Minimal value of 4 minus the error 0.1

## ----xyDimensions-------------------------------------------------------------
xyData$xDimension
xyData$xUnit
xyData$yDimension
xyData$yUnit
# List of all supported dimensions
length(ospDimensions)
ospDimensions[[1]]

## ----setDimension-------------------------------------------------------------
xyData$yDimension
xyData$yUnit
xyData$yErrorUnit
# Change the dimension of y values
xyData$yDimension <- ospDimensions$`Concentration (molar)`
xyData$yUnit
xyData$yErrorUnit

## ----xyOffset-----------------------------------------------------------------
xVals <- c(0, 1, 2, 3)
yVals <- c(4, 5, 6, 7)
yError <- c(0.1, 0, 0.1, 1)

xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
xyData$xValues
xyData$yValues

xyData$xMax
xyData$xMin
xyData$yMax # Maximal value of 7 plus the error 1
xyData$yMin # Minimal value of 4 minus the error 0.1

# Offset x values by -1 and y values by 2
xyData$xOffset <- -1
xyData$yOffset <- 2

# xValues and yValues are unaffected
xyData$xValues
xyData$yValues

xyData$xMax # Maximal value of 3 plus offset -1
xyData$xMin # Minimal value of 0 plus offset -1
xyData$yMax # Maximal value of 7 plus the error 1 plus offset 2
xyData$yMin # Minimal value of 4 minus the error 0.1 plus offset 2

# Set y factor to 0.5
xyData$yFactor <- 0.5

xyData$yMax # Maximal value of 7 plus the error 1 plus offset 2 multiplied by 0.5
xyData$yMin # Minimal value of 4 minus the error 0.1 plus offset 2 multiplied by 0.5


## ----DataConfiguration--------------------------------------------------------
dataConf <- DataConfiguration$new(
  dataFolder = file.path(getwd(), "..", "tests", "data"),
  dataFile = "CompiledDataSet.xlsx",
  compoundPropertiesFile = "Compound_Properties.xlsx",
  dataSheets = c("TestSheet_1")
)
print(dataConf)

dataConf$columnsToSplitBy

## ----readOSPSTimeValues-------------------------------------------------------
observedData <- readOSPSTimeValues(dataConfiguration = dataConf)
names(observedData)

observedData[[1]]
