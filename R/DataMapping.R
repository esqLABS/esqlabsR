#' @title DataMapping
#' @docType class
#' @description Mapping of model outputs to observed data
#' @export
#' @import ospsuite hash
#' @format NULL
DataMapping <- R6::R6Class(
  "DataMapping",
  inherit = Printable,
  cloneable = FALSE,
  active = list(
    #' @field xySeries Named list with the `XYData`
    #' that will be plotted. Names are the labels of the `xySeries` objects
    xySeries = function(value) {
      if (missing(value)) {
        as.list(private$.xySeries)
      } else {
        stop(messages$errorPropertyReadOnly("xySeries"))
      }
    },

    #' @field xySeriesCount number of `XYData` objects to be plotted
    xySeriesCount = function(value) {
      if (missing(value)) {
        length(private$.xySeries)
      } else {
        stop(messages$errorPropertyReadOnly("xySeriesCount"))
      }
    },

    #' @field xLim Limits of the x-axis. Numerical vector c(min, max)
    xLim = function(value) {
      if (missing(value)) {
        if (is.null(private$.xLim)) {
          if (length(self$xySeries) == 0) {
            return(c(0, 0))
          }
          xMax <- max(sapply(self$xySeries, function(x) {
            toUnit(
              quantityOrDimension = self$xDimension,
              values = x$xMax,
              targetUnit = self$xUnit,
              sourceUnit = x$xUnit
            )
          }))
          xMin <- min(sapply(self$xySeries, function(x) {
            toUnit(
              quantityOrDimension = self$xDimension,
              values = x$xMin,
              targetUnit = self$xUnit,
              sourceUnit = x$xUnit
            )
          }))
          # Extend limits by 10%
          return(c(xMin, xMax) + abs(c(xMin, xMax)) * c(-0.1, 0.1))
          # My cat wrote this, I leave it here out of respect
          # \code{runSimulationBatchesConcurrently}ß C.\JKFD. PO.#]}*#J#......................JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJöL
        } else {
          private$.xLim
        }
      } else {
        validateIsNumeric(value, nullAllowed = TRUE)
        if (!is.null(value)) {
          validateLength(value, 2)
        }
        private$.xLim <- value
      }
    },

    #' @field yLim Limits of the y-axis. Numerical vector c(min, max)
    yLim = function(value) {
      if (missing(value)) {
        if (is.null(private$.yLim)) {
          if (length(self$xySeries) == 0) {
            return(c(0, 0))
          }
          yMax <- max(sapply(self$xySeries, function(x) {
            toUnit(
              quantityOrDimension = self$yDimension,
              values = x$yMax,
              targetUnit = self$yUnit,
              sourceUnit = x$yUnit,
              molWeight = x$MW,
              molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
            )
          }))
          # If logarithmic scaling of the y axis is selected, the minimal value should be greater than zero
          yMin <- min(sapply(self$xySeries, function(x) {
            if (isCharInString("y", self$log)) {
              toUnit(
                quantityOrDimension = self$yDimension,
                values = x$yMinPositive(),
                targetUnit = self$yUnit,
                sourceUnit = x$yUnit,
                molWeight = x$MW,
                molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
              )
            } else {
              toUnit(
                quantityOrDimension = self$yDimension,
                values = x$yMin,
                targetUnit = self$yUnit,
                sourceUnit = x$yUnit,
                molWeight = x$MW,
                molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
              )
            }
          }))
          # Extend limits by 10%
          return(c(yMin, yMax) + abs(c(yMin, yMax)) * c(-0.1, 0.1))
        } else {
          private$.yLim
        }
      } else {
        validateIsNumeric(value, nullAllowed = TRUE)
        if (!is.null(value)) {
          validateLength(value, 2)
        }
        private$.yLim <- value
      }
    },

    #' @field xLab label of the x-axis.
    xLab = function(value) {
      if (missing(value)) {
        private$.xLab
      } else {
        validateIsString(value)
        private$.xLab <- value
      }
    },

    #' @field yLab label of the y-axis.
    yLab = function(value) {
      if (missing(value)) {
        private$.yLab
      } else {
        validateIsString(value)
        private$.yLab <- value
      }
    },

    #' @field xDimension Dimension of x values. See enum `ospDimensions` for the list of supported dimensions.
    #' If no dimension is specified, the dimension of the first added `XYSeries` is used.
    #' If no `XYSeries` are present, the dimension is `NULL`
    #' When changing the dimension, the unit is automatically set to the base unit of the dimension.
    xDimension = function(value) {
      if (missing(value)) {
        if (!is.null(private$.xDimension)) {
          return(private$.xDimension)
        }
        if (self$xySeriesCount == 0) {
          return(NULL)
        }
        return(self$xySeries[[1]]$xDimension)
      } else {
        validateDimension(value)
        private$.xDimension <- value
        self$xUnit <- getBaseUnit(value)
      }
    },

    #' @field yDimension Dimension of y values. See enum `ospDimensions` for the list of supported dimensions.
    #' If no dimension is specified, the dimension of the first added `XYSeries` is used.
    #' If no `XYSeries` are present, the dimension is `NULL`
    #'     #' When changing the dimension, the unit is automatically set to the base unit of the dimension.
    yDimension = function(value) {
      if (missing(value)) {
        if (!is.null(private$.yDimension)) {
          return(private$.yDimension)
        }
        if (self$xySeriesCount == 0) {
          return(NULL)
        }
        return(self$xySeries[[1]]$yDimension)
      } else {
        validateDimension(value)
        private$.yDimension <- value
        private$.yUnit <- getBaseUnit(value)
      }
    },

    #' @field xUnit Unit of x values.
    #' If no unit is specified, the default unit of the dimension is used.
    #' If no dimension is specified, the unit is `NULL`
    xUnit = function(value) {
      if (missing(value)) {
        if (!is.null(private$.xUnit)) {
          return(private$.xUnit)
        }
        if (is.null(self$xDimension)) {
          return(NULL)
        }
        return(getBaseUnit(self$xDimension))
      } else {
        private$.xUnit <- value
      }
    },

    #' @field yUnit Unit of y values.
    #' If no unit is specified, the default unit of the dimension is used.
    #' If no dimension is specified, the unit is `NULL`
    yUnit = function(value) {
      if (missing(value)) {
        if (!is.null(private$.yUnit)) {
          return(private$.yUnit)
        }
        if (is.null(self$yDimension)) {
          return(NULL)
        }
        return(getBaseUnit(self$yDimension))
      } else {
        private$.yUnit <- value
      }
    },

    #' @field groupings A named list listing which data sets are grouped together. Grouped data sets will be plotted with the same color
    #' and used together in the legend.
    groupings = function(value) {
      if (missing(value)) {
        as.list(private$.groupings)
      } else {
        stop(messages$errorPropertyReadOnly("groupings", optionalMessage = "Data sets are assigned to groupings when adding via `addModelOutputs'
                                            or 'addXYSeries'."))
      }
    },

    #' @field ungroupedSeries A list of `XYData` that do not belong to any group.
    #' `NULL` if empty.
    ungroupedSeries = function(value) {
      if (missing(value)) {
        private$.emptyGrouping
      } else {
        stop(messages$errorPropertyReadOnly("ungroupedSeries", optionalMessage = "Data sets are assigned to groupings when adding via `addModelOutputs'
                                            or 'addXYSeries'."))
      }
    },

    #' @field plotType A string defining what kind of plot is generated when the `plot()` method of the object is called.
    #' Supported plot types are listed in the enum `PlotTypes`. Default is "IndividualProfile"
    plotType = function(value) {
      if (missing(value)) {
        private$.plotType
      } else {
        validateEnumValue(enum = PlotTypes, value = value)
        private$.plotType <- value
      }
    },

    #' @field populationQuantiles A numerical vector with three quantile values used if `plotType = "PopulationQuantiles"`. Default is
    #' `c(0.05, 0.5, 0.95)`
    populationQuantiles = function(value) {
      if (missing(value)) {
        private$.populationQuantiles
      } else {
        validateIsNumeric(value)
        validateLength(value, 3)
        private$.populationQuantiles <- value
      }
    }
  ),
  private = list(
    .xySeries = NULL,
    # Map linking each xySeries to a group. Used for removal of xySeries
    .xySeriesGroupMap = NULL,
    .xLim = NULL,
    .yLim = NULL,
    .xLab = NULL,
    .yLab = NULL,
    .xDimension = NULL,
    .yDimension = NULL,
    .xUnit = NULL,
    .yUnit = NULL,
    .groupings = NULL,
    .emptyGrouping = NULL,
    .plotType = "IndividualProfile",
    .populationQuantiles = c(0.05, 0.5, 0.95),
    .removeLabelFromGroup = function(label, group) {
      # Empty grouping has to be treated separately. Empty group is identified by an NA
      # because NULL cannot be added to a list.
      # Have to unlist because removeFromList returns a list, but groupings are vectors of strings.
      # However, if the list is empty, create an empty list...
      if (is.na(group)) {
        private$.emptyGrouping <- unlist(removeFromList(label, private$.emptyGrouping)) %||% list()
      } else {
        private$.groupings[[group]] <- unlist(removeFromList(label, private$.groupings[[group]]))
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `DataMapping` object.
    initialize = function() {
      private$.xySeries <- hash::hash()
      private$.xySeriesGroupMap <- hash::hash()
      private$.groupings <- hash::hash()
      private$.emptyGrouping <- list()
    },
    #' @description
    #' Clean up upon object removal
    finalize = function() {
      hash::clear(private$.groupings)
      hash::clear(private$.xySeries)
      hash::clear(private$.xySeriesGroupMap)
    },

    #' @field log String defining which axis will be plotted in logarithmic scaling.
    #' Possible values are "" (empty string, both axis in linear scaling, default),
    #' "x", "y", "xy" (both axis are in logarithmic scaling).
    log = "",

    #' @field title Title of the plot. If the value is an empty string, no title is added. Otherwise, the title appears on top of the plot.
    title = "",

    #' @field addLegend Boolean defining if the legend should be added to the plot. If TRUE,
    #' a legend will be added for each group and each XYSeries that is not in any group
    addLegend = TRUE,

    #' @field legendPosition Position of the legend in the plot. Default value is "topright". See [legend()] for more information.
    legendPosition = "topright",

    #' @description Add simulated results to the data mapping
    #' @param paths A string or a list of strings representing the path(s) to the output(s) in the model.
    #' @param simulationResults Simulated results as returned by `runSimulation`
    #' @param labels A string or a list of strings that are used as a label (e.g. in the legend) for the output(s).
    #' If `NULL` (default), the path of the output is used as a label.
    #' @param groups A string or a list of strings assigning the outputs to a group. All outputs may be assigned to one group, or to
    #' different groups, while each output can be assigned to not more than one group. If an entry within the list is NULL, the corresponding
    #' output is not assigned to any group
    #' @param removeNA If TRUE (default), NA values will be removed from the simulated results. NA values can be the result of observer not being calculated at a certain time point.
    #' @description
    #' Add new `ModelOutput` to be plotted. Line type is set to "l" (line) by default.
    addModelOutputs = function(paths, labels, simulationResults, groups = NULL, removeNA = TRUE) {
      # Paths are checked for correct type in ospsuite
      validateIsString(labels)
      validateIsSameLength(paths, labels)
      outputValues <- getOutputValues(
        simulationResults = simulationResults,
        quantitiesOrPaths = paths,
        stopIfNotFound = FALSE
      )

      for (idx in seq_along(paths)) {
        yValues <- outputValues$data[[paths[[idx]]]]
        # If NULL is returned, the output with the given path could not be found
        if (all(is.na(yValues))) {
          stop(messages$errorOutputPathNotFound(paths[[idx]]))
        }

        xValues <- outputValues$data$Time
        # Remove any NA values, is specified.
        if (removeNA) {
          naVals <- is.na(yValues)
          yValues <- yValues[!naVals]
          xValues <- xValues[!naVals]
        }
        # If no label is specified, use output path as label
        label <- labels[[idx]] %||% paths[[idx]]
        timeValues <- XYData$new(xValues, yValues, label = label)

        self$addXYData(
          timeValues,
          groups = groups[[idx]]
        )
        xySeries <- self$xySeries[[label]]
        xySeries$type <- "l"
        xySeries$dataType <- XYDataTypes$Simulated
        xySeries$yDimension <- outputValues$metaData[[paths[[idx]]]]$dimension
        xySeries$yUnit <- outputValues$metaData[[paths[[idx]]]]$unit
        # get molecular weight
        entity <- getQuantity(path = paths[[idx]], container = simulationResults$simulation)
        mw <- NULL
        if (entity$quantityType == "Drug") {
          mw <- getParameter(path = paste(entity$name, "Molecular weight", sep = "|"), container = simulationResults$simulation, stopIfNotFound = F)
        } else if (entity$parentContainer$containerType == "Molecule") {
          mw <- getParameter(path = paste(entity$parentContainer$name, "Molecular weight", sep = "|"), container = simulationResults$simulation, stopIfNotFound = F)
        }
        if (!is.null(mw)) {
          xySeries$MW <- toDisplayUnit(quantity = mw, values = mw$value)
        }
      }

      invisible(self)
    },

    #' @description  Add `XYData` object(s).
    #'
    #' @param xValsList A single array or a list of arrays of x-values. For time series, the values must be in minutes.
    #' @param yValsList A single array or a list of arrays of y-values.
    #' @param yErrorList A single array or a list of arrays of y-error values. If `NULL` (default), no errors are
    #' assigned. If not `NULL`, the list must have the same number of entries (numerical arrays) as `yValsList`. If an entry of the list is `NULL`, the respective data set has no error.
    #' @param labels A string or a list of strings that are used as unique label for the output(s). Must be of same length as `xValsList`.
    #' @param groups A string or a list of strings assigning the data set to a group. If an entry within the list is `NULL`, the corresponding data set is not assigned to any group. If `NULL` (default), all data sets are not assigned to any group. If provided, `groups` must have the same length as `xValsList`
    #'
    #' @details
    #' Add new series of x-y values to be plotted. If an `XYData` with the same label already exists,
    #' it will be overwritten
    #' @export
    #'
    #' @examples
    #' dataMapping <- DataMapping$new()
    #' xVals <- list(c(1, 2, 3, 4), c(1, 2, 3, 4), c(2, 3, 4, 5))
    #' yVals <- list(c(5, 6, 7, 8), c(5, 6, 7, 8), c(6, 7, 8, 9))
    #' yErr <- list(c(0.1, 0.1, 0.1, 0.2), NULL, c(0.2, 0.3, 0.1, 0.2))
    #' groups <- list("Group1", NULL, "Group1")
    #' dataMapping$addXYSeries(xValsList = xVals, yValsList = yVals,
    #' yErrorList = yErr, labels = list("my series1", "my series2", "my series3"), groups = groups)
    addXYSeries = function(xValsList, yValsList, labels, yErrorList = NULL, groups = NULL) {
      # Label is validated for string in Plotable
      xValsList <- toList(xValsList)
      yValsList <- toList(yValsList)
      if (!is.null(yErrorList)) {
        yErrorList <- toList(yErrorList)
      }
      if (!is.null(groups)) {
        validateIsString(groups, nullAllowed = TRUE)
        validateIsSameLength(xValsList, groups)
      }
      validateIsSameLength(xValsList, yValsList, labels)

      for (idx in seq_along(labels)) {
        xyData <- XYData$new(xVals = xValsList[[idx]], yVals = yValsList[[idx]], yError = yErrorList[[idx]], label = labels[[idx]])
        group <- groups[[idx]]
        self$addXYData(xyData, group)
      }
      invisible(self)
    },


    #' @description Add `XYData` object(s). The objects are cloned at adding.
    #'
    #' @param XYData Object or a list of objects of the type `XYData`
    #' @param groups A string or a list of strings assigning the data set to a group. If an entry within the list is `NULL`, the corresponding data set is not assigned to any group. If `NULL` (default), all data sets are not assigned to any group. If provided, `groups` must have the same length as `XYData`
    #' output is not assigned to any group
    #' @export
    addXYData = function(XYData, groups = NULL) {
      validateIsOfType(XYData, "XYData")
      XYData <- toList(XYData)
      if (!is.null(groups)) {
        groups <- c(groups)
        validateIsSameLength(XYData, groups)
      }
      for (idx in seq_along(XYData)) {
        newGroupName <- groups[[idx]]
        # NULL is converted to NA as NULL cannot be put into a list.
        if (is.null(newGroupName)) {
          newGroupName <- NA
        }
        label <- XYData[[idx]]$label
        # clone the object and add it
        timeValuesClone <- XYData[[idx]]$clone()

        private$.xySeries[[label]] <- timeValuesClone
        # If an entry with the given label already exists in the DataMapping (i.e., it will be overwritten),
        # check if the group has changed. In no, do nothing. If yes, remove the label
        # from the old group and add to the new.
        if (hash::has.key(key = label, hash = private$.xySeriesGroupMap)) {
          if (compareWithNA(private$.xySeriesGroupMap[[label]], newGroupName)) {
            next
          }
          private$.removeLabelFromGroup(
            label = label,
            group = private$.xySeriesGroupMap[[label]]
          )
        }
        # Now assign the entry to the new group
        # If no group is specified, add the entry to the empty grouping
        if (is.na(newGroupName)) {
          private$.xySeriesGroupMap[[label]] <- newGroupName
          private$.emptyGrouping <- append(private$.emptyGrouping, label)
          next
        }
        # If a group with the given name already exists, put the entry into it
        if (hash::has.key(key = newGroupName, hash = private$.groupings)) {
          private$.groupings[[newGroupName]] <- append(private$.groupings[[newGroupName]], label)
          private$.xySeriesGroupMap[[label]] <- newGroupName
          next
        }
        # Create a new group and put the entry into
        private$.groupings[[newGroupName]] <- label
        private$.xySeriesGroupMap[[label]] <- newGroupName
      }
    },

    #' @description Add `DataSet` object(s).
    #' @details The objects are transformed to `XYData` before adding.
    #'
    #' @param dataSets Object or a list of objects of the type `DataSet`
    #' @param groups A string or a list of strings assigning the data set to a group. If an entry within the list is `NULL`, the corresponding data set is not assigned to any group. If `NULL` (default), all data sets are not assigned to any group. If provided, `groups` must have the same length as `dataSets`
    #' output is not assigned to any group
    #' @export
    addDataSets = function(dataSets, groups = NULL) {
      validateIsOfType(dataSets, "DataSet")
      dataSets <- toList(dataSets)
      xyDataList <- vector("list", length = length(dataSets))
      for (idx in seq_along(dataSets)) {
        dataSet <- dataSets[[idx]]
        label <- dataSet$name
        # Create a `XYData` object from `DataSet`
        xyData <- XYData$new(xVals = dataSet$xValues, yVals = dataSet$yValues, yError = dataSet$yErrorValues, label = label)
        xyData$dataType <- XYDataTypes$Observed
        xyData$MW <- dataSet$molWeight
        xyData$xDimension <- dataSet$xDimension
        xyData$xUnit <- dataSet$xUnit
        xyData$yDimension <- dataSet$yDimension
        xyData$yUnit <- dataSet$yUnit
        # In `DataSet`, yErrorUnit is `NULL` if no error is defined. In `XYData`,
        # there is always a unit of the error (which is by default set to the base
        # unit of the y dimension).
        if (!is.null(dataSet$yErrorUnit)) {
          xyData$yErrorUnit <- dataSet$yErrorUnit
        }
        # Set meta data
        for (metaDataIdx in seq_along(dataSet$metaData)) {
          xyData$setMetaData(names(dataSet$metaData)[[metaDataIdx]], dataSet$metaData[[metaDataIdx]])
        }
        xyDataList[[idx]] <- xyData
      }
      self$addXYData(xyDataList, groups)
    },

    #' @param label label of the x-y values series to be removed
    #' @description
    #' Remove the observed data with given label from the DataMapping.
    removeXYSeries = function(label) {
      # If no entry with the given label exists, show a warning and do nothing.
      if (!hash::has.key(key = label, hash = private$.xySeries)) {
        warning(messages$warningLabelNotInDataMapping(label))
        return(invisible(self))
      }
      hash::del(x = label, hash = private$.xySeries)

      private$.removeLabelFromGroup(
        label = label,
        group = private$.xySeriesGroupMap[[label]]
      )
      hash::del(x = label, hash = private$.xySeriesGroupMap)
      invisible(self)
    },

    #' @description Return group mapping of labels.
    #' @details  Returns a named list with keys being labels of xySeries and values group names. If value is `NA`, no group is defined for this label.
    #' @return  A named list with keys being labels of xySeries and values group names.
    #' @export
    getXYSeriesGroupMap = function() {
      return(as.list(private$.xySeriesGroupMap))
    },

    #' @description Set the X-factors of x-y values by labels.
    #' @details If the data set with a label is not present in the mapping, the label is ignored.
    #'
    #' @param labels A list of labels of `XYData`.
    #' @param xFactors Numeric values that will be multiplied by the x-values during plotting.
    setXFactors = function(labels, xFactors) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(xFactors, nullAllowed = TRUE)
      validateIsSameLength(labels, xFactors)

      for (idx in seq_along(labels)) {
        xySeries <- self$xySeries[[labels[[idx]]]]
        xySeries$xFactor <- xFactors[[idx]]
      }

      invisible(self)
    },

    #' @description Set the y-factors of x-y values by labels.
    #' @details If the data set with a label is not present in the mapping, the label is ignored
    #'
    #' @param labels A list of label of `XYData`
    #' @param yFactors Numeric values that will be multiplied by the y-values during plotting
    setYFactors = function(labels, yFactors) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(yFactors, nullAllowed = TRUE)
      validateIsSameLength(labels, yFactors)

      for (idx in seq_along(labels)) {
        xySeries <- self$xySeries[[labels[[idx]]]]
        xySeries$yFactor <- yFactors[[idx]]
      }

      invisible(self)
    },

    #' @description Set the X-offset of x-y values by labels.
    #' @details If the data set with a label is not present in the mapping, the label is ignored
    #'
    #' @param labels A list of label of `XYData`
    #' @param xOffsets Numeric values that will be added to the x-values during plotting
    setXOffsets = function(labels, xOffsets) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(xOffsets, nullAllowed = TRUE)
      validateIsSameLength(labels, xOffsets)

      for (idx in seq_along(labels)) {
        xySeries <- self$xySeries[[labels[[idx]]]]
        xySeries$xOffset <- xOffsets[[idx]]
      }

      invisible(self)
    },

    #' @description Set the Y-offset of x-y values by labels.
    #' @details If the data set with a label is not present in the mapping, the label is ignored
    #'
    #' @param labels A list of label of `XYData`
    #' @param yOffsets Numeric values that will be added to the y-values during plotting
    setYOffsets = function(labels, yOffsets) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(yOffsets, nullAllowed = TRUE)
      validateIsSameLength(labels, yOffsets)

      for (idx in seq_along(labels)) {
        xySeries <- self$xySeries[[labels[[idx]]]]
        xySeries$yOffset <- yOffsets[[idx]]
      }

      invisible(self)
    },

    #' @description Set the type(s) of the data to be plotted, e.g. line, points, etc.
    #' @details If no data set for the provided label is present in the mapping, the corresponding value is ignored.
    #' No check is performed whether a valid type is provided.
    #'
    #' @param labels A list of label of `XYData`
    #' @param types Plot types as accepted by the base `plot` method
    setTypes = function(labels, types) {
      validateIsString(c(labels, types), nullAllowed = TRUE)
      validateIsSameLength(labels, types)

      for (idx in seq_along(labels)) {
        xySeries <- self$xySeries[[labels[[idx]]]]
        xySeries$type <- types[[idx]]
      }

      invisible(self)
    },

    #' @description Set the line type(s) property of the data to be plotted. Line types as accepted by the base `plot` lty argument.
    #' @details If no data set for the provided label is present in the mapping, the corresponding value is ignored.
    #' No check is performed whether a valid type is provided.Line types as accepted by the base `plot` lty argument.
    #' Line types can be provided either as numeric or as character vectors (e.g. "dashed").
    #'
    #' @param labels A list of label of `XYData`
    #' @param linetypes Values that will be set as line type(s).
    setLinetypes = function(labels, linetypes) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsSameLength(labels, linetypes)

      for (idx in seq_along(labels)) {
        xySeries <- self$xySeries[[labels[[idx]]]]
        xySeries$lty <- linetypes[[idx]]
      }

      invisible(self)
    },

    #' @description Set the colors of the data to be plotted
    #' @details If the data set with a label is not present in the mapping, the label is ignored
    #'
    #' @param labels A list of label of `XYData`
    #' @param colors String names of colors of the data as accepted by the base `plot` method
    #' If the value is `NULL`, the color is automatically selected when plotting the data.
    setColors = function(labels, colors) {
      validateIsString(c(labels), nullAllowed = TRUE)
      # If the color is a single NULL, put it into a list
      if (is.null(colors)) {
        colors <- list(NULL)
      }
      validateIsSameLength(labels, colors)

      for (idx in seq_along(labels)) {
        xySeries <- self$xySeries[[labels[[idx]]]]
        xySeries$color <- colors[[idx]]
      }

      invisible(self)
    },

    #' @description Apply settings stored in a `DataMappingConfiguration`
    #' @details If the data set with a label is not present in the mapping, the label is ignored
    #' Equivalent to calling `setXFactors`, `setYFactors`, `setXOffsets`,
    #' `setYOffsets`, `setTypes`, and `setColors`.
    #'
    #' @param dataMappingConfiguration An object of type `DataMappingConfiguration`
    setConfiguration = function(dataMappingConfiguration) {
      self$setXFactors(names(dataMappingConfiguration$xFactors), dataMappingConfiguration$xFactors)
      self$setYFactors(names(dataMappingConfiguration$yFactors), dataMappingConfiguration$yFactors)
      self$setXOffsets(names(dataMappingConfiguration$xOffsets), dataMappingConfiguration$xOffsets)
      self$setYOffsets(names(dataMappingConfiguration$yOffsets), dataMappingConfiguration$yOffsets)
      self$setTypes(names(dataMappingConfiguration$lineTypes), dataMappingConfiguration$lineTypes)
      self$setColors(names(dataMappingConfiguration$colors), dataMappingConfiguration$colors)

      invisible(self)
    },

    #' @description Plot the data stored in this `DataMapping`.
    #' @param ... Any parameter that can be interpreted by the default [plot()] function
    plot = function(...) {
      # Get the function by its name
      fun <- get(x = paste0("plot", self$plotType))
      fun(self, ...)

      invisible(self)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Plot type", self$plotType)
      private$printLine("Population quantiles", self$populationQuantiles)
      private$printLine("labels", hash::keys(private$.xySeries))
      private$printLine("X limits", self$xLim)
      private$printLine("Y limits", self$yLim)
      private$printLine("X label", self$xLab)
      private$printLine("Y label", self$yLab)
      private$printLine("X unit", self$xUnit)
      private$printLine("Y unit", self$yUnit)
      private$printLine("Title", self$title)
      private$printLine("Log axes", self$log)
      invisible(self)
    }
  )
)
