#' @name sensitivityCalculation
#' @title Carry out and visualize sensitivity analysis (with OSPSuite)
#'
#' @param simulation An object of type `Simulation`.
#' @param outputPaths Path (or a vector of paths) to the output(s) for which the
#' sensitivity will be analyzed.
#' @param parameterPaths A single or a vector of the parameter path(s) to be
#' varied. Can also be a named vector, where the names are user-defined labels.
#' These names will be stored and used in downstream plotting functions (e.g.,
#' as legend labels) if provided.
#' @param variationRange Optional numeric vector or list defining the scaling of
#' the parameters. The same variation range is applied to all specified parameters
#' unless a list is provided, in which case the length of the list must match
#' the length of `parameterPaths`, allowing individual variation for each parameter.
#' If not specified, the following vector will be used: c(0.1, 0.2, 0.3, 0.4,
#' 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10).
#' @param variationType A string specifying whether the values in `variationRange`
#' are applied as `"absolute"` or `"relative"` scaling. When set to `"absolute"`,
#' the values are interpreted as absolute parameter values. When set to `"relative"`,
#' the values are interpreted as scaling factors relative to the initial parameter
#' values. Default is `"relative"`.
#' @param pkParameters A vector of names of PK parameters for which the
#' sensitivities will be calculated. For a full set of available standard PK
#' parameters, run `names(ospsuite::StandardPKParameter)`. By default, the
#' following parameters are considered: `"C_max"`, `"t_max"`, `"AUC_inf"`.
#' If `NULL`, all available PK-parameters (including the user-defined) will be calculated.
#' @param customOutputFunctions A named list with
#' custom function(s) for output calculations. User-defined functions should
#' have either 'x', 'y', or both 'x' and 'y' as parameters which correspond to
#' x-Dimension (time) or y-Dimension values from simulation results. The output
#' of the function is a single numerical value for each output and parameter path, which
#' is then included in the returned dataframe of PK parameters.
#' @param saOutputFilePath Path to excel file in which PK-parameter data should
#' be saved. If a file already exists, it will be overwritten. Default is `NULL`,
#' meaning the data will not be saved to a spreadsheet.
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions` used
#' during the simulation run
#'
#' @family sensitivity-calculation
#'
#' @returns
#'
#' A list containing following objects:
#' - `SimulationResults`
#' - specified output paths
#' - specified parameter paths
#' - A data frame of PK parameters
#'
#' @examples
#' \dontrun{
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' simulation <- loadSimulation(simPath)
#' outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
#' parameterPaths <- c(
#'   "Aciclovir|Lipophilicity",
#'   "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
#'   "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
#' )
#'
#' # extract the results into a list of dataframes
#' sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = parameterPaths
#' )
#'
#' # Calculate sensitivity for a user-defined function that computes the
#' # averate of the simulated y-values
#' customOutputFunctions <- list(
#'   "Average" = function(y) mean(y)
#' )
#' sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = parameterPaths,
#'   customOutputFunctions = customOutputFunctions
#' )
#' }
#'
#' @export
sensitivityCalculation <- function(
  simulation,
  outputPaths,
  parameterPaths,
  variationRange = c(seq(0.1, 1, by = 0.1), seq(2, 10, by = 1)),
  variationType = c("relative", "absolute"),
  pkParameters = c("C_max", "t_max", "AUC_inf"),
  customOutputFunctions = NULL,
  saOutputFilePath = NULL,
  simulationRunOptions = NULL
) {
  # Input validation ------------------------------------------------------

  # Validate vector arguments of character type
  .validateCharVector(outputPaths)
  .validateCharVector(parameterPaths)
  .validateCharVector(names(parameterPaths), nullAllowed = TRUE)
  .validateCharVector(variationType)
  .validateCharVector(pkParameters, nullAllowed = TRUE)

  variationType <- match.arg(variationType)

  # Check for non-standard PK parameters
  .validatePKParameters(pkParameters)

  # Validate customOutputFunctions
  .validateIsNamedList(customOutputFunctions, nullAllowed = TRUE)
  validateIsOfType(customOutputFunctions, "function", nullAllowed = TRUE)

  # Fail early to avoid costly failure after analysis is already carried out.
  if (!is.null(saOutputFilePath)) {
    validateIsFileExtension(saOutputFilePath, "xlsx")
  }

  # Creating SimulationResults batches ------------------------------------

  # Normalize variationRange
  variationRange <- .normalizeVariationRange(variationRange, parameterPaths)

  # Store old simulation outputs and set user defined
  oldOutputSelections <- simulation$outputSelections$allOutputs
  setOutputs(quantitiesOrPaths = outputPaths, simulation = simulation)

  # Store initial value for each parameter
  initialValues <- vector("double", length(parameterPaths))
  names(initialValues) <- parameterPaths

  # Classify parameters and retrieve initial values
  constantParamPaths <- list()
  formulaParamPaths <- list()

  for (parameterPath in parameterPaths) {
    # Check if the parameter is given by an explicit formula
    isExplicitFormulaByPath <- ospsuite::isExplicitFormulaByPath(
      path = parameterPath,
      simulation = simulation
    )

    # Classify as formula or constant parameter
    if (isExplicitFormulaByPath) {
      formulaParamPaths <- c(formulaParamPaths, parameterPath)
    } else {
      constantParamPaths <- c(constantParamPaths, parameterPath)
    }

    # Store the initial values for this parameter
    initialValues[[parameterPath]] <- ospsuite::getQuantityValuesByPath(
      quantityPaths = parameterPath,
      simulation = simulation
    )
  }

  # Transform and validate variationRange
  variationRange <- .transformVariationRange(
    variationRange,
    initialValues,
    variationType
  )
  variationRange <- lapply(variationRange, .validateVariationRange)

  # Initialize batchResultsIdMap
  batchResultsIdMap <- vector("list", length(parameterPaths))
  names(batchResultsIdMap) <- parameterPaths

  for (parameterPath in parameterPaths) {
    # Initialize batchResultsIdMap for the current parameter
    batchResultsIdMap[[parameterPath]] <- vector(
      "list",
      length(variationRange[[parameterPath]])
    )
    names(batchResultsIdMap[[parameterPath]]) <- variationRange[[parameterPath]]
  }

  constantParamPaths <- unlist(constantParamPaths, use.names = FALSE)
  formulaParamPaths <- unlist(formulaParamPaths, use.names = FALSE)

  simulationBatches <- list()

  # Create one batch for all constant parameters
  if (length(constantParamPaths) > 0) {
    constantBatch <- createSimulationBatch(
      simulation = simulation,
      parametersOrPaths = constantParamPaths
    )

    # Add run values. While varying one parameter, the values of remaining
    # constant parameters remain at their initial values.
    for (constantParamPath in constantParamPaths) {
      for (scaleFactorIdx in seq_along(variationRange[[constantParamPath]])) {
        # Change the value of the varied parameter
        runValues <- initialValues[constantParamPaths]
        runValues[[constantParamPath]] <-
          variationRange[[constantParamPath]][[scaleFactorIdx]] *
          runValues[[constantParamPath]]

        # Add run values and store the ID in the `batchResultsIdMap`
        batchResultsIdMap[[constantParamPath]][[scaleFactorIdx]] <-
          constantBatch$addRunValues(parameterValues = runValues)
      }
    }

    simulationBatches <- c(simulationBatches, constantBatch)
  }

  # Add batches for formula parameters
  for (formulaParamPath in formulaParamPaths) {
    formulaBatch <- createSimulationBatch(
      simulation = simulation,
      parametersOrPaths = formulaParamPath
    )

    # Add run values.
    for (scaleFactorIdx in seq_along(variationRange[[formulaParamPath]])) {
      batchResultsIdMap[[formulaParamPath]][[scaleFactorIdx]] <-
        formulaBatch$addRunValues(
          parameterValues = variationRange[[formulaParamPath]][[
            scaleFactorIdx
          ]] *
            initialValues[[formulaParamPath]]
        )
    }

    simulationBatches <- c(simulationBatches, formulaBatch)
  }

  # Simulate all batches in parallel
  simulationBatchesResults <- runSimulationBatches(
    simulationBatches = simulationBatches,
    simulationRunOptions = simulationRunOptions
  )

  # Call gc() on .NET
  ospsuite::clearMemory()

  # Remove top-level names to flatten the list in the next step
  names(simulationBatchesResults) <- NULL

  # Unlist to gather all results, using batchResultsIdMap to filter by parameter/scale factor
  simulationBatchesResults <- unlist(simulationBatchesResults)

  # Nest simulation results by parameter path and factor values
  simulationResultsBatch <- batchResultsIdMap

  for (parameterPath in names(batchResultsIdMap)) {
    for (parameterFactor in names(batchResultsIdMap[[parameterPath]])) {
      resultId <- batchResultsIdMap[[parameterPath]][[parameterFactor]]
      resultSim <- purrr::pluck(simulationBatchesResults, resultId)
      if (!is.null(resultSim)) {
        simulationResultsBatch[[parameterPath]][[parameterFactor]] <-
          purrr::pluck(simulationBatchesResults, resultId)
      } else {
        simulationResultsBatch[[parameterPath]][[parameterFactor]] <- NULL
        warning(
          messages$sensitivityAnalysisSimulationFailure(
            parameterPath,
            parameterFactor
          )
        )
      }
    }
  }

  # Extract and save data frames ------------------------------------------

  # Extract data frame for PK parameters
  pkData <- .simulationResultsBatchToPKDataFrame(
    simulationResultsBatch,
    parameterPaths,
    customOutputFunctions
  )

  # Filter out unneeded PK parameters
  if (!is.null(pkParameters)) {
    filterValues <- c(pkParameters, names(customOutputFunctions))
    pkData <- dplyr::filter(pkData, PKParameter %in% filterValues)
  }

  # Write each data frame in a list to a separate sheet in Excel
  if (!is.null(saOutputFilePath)) {
    # If there is no data to write to Excel sheet, inform the user and do nothing.
    if (nrow(pkData) == 0L) {
      warning(messages$noPKDataToWrite())
    } else {
      # Convert tidy data to wide format
      pkParameterNames <- c(
        ospsuite::allPKParameterNames(),
        names(customOutputFunctions)
      )
      pkDataWide <- .convertToWide(pkData, pkParameterNames)

      # Write to a spreadsheet with one sheet per output path.
      .writeExcel(data = pkDataWide, path = saOutputFilePath)
    }
  }

  # Return sensitivity calculation results --------------------------------

  # Final list with needed objects and data frames for plotting functions.
  results <- list(
    "simulationResults" = simulationResultsBatch,
    "outputPaths" = outputPaths,
    "parameterPaths" = parameterPaths,
    "pkData" = pkData
  )

  # Reset simulation outputs
  oldOutputSelections <- simulation$outputSelections$allOutputs
  clearOutputs(simulation = simulation)

  for (outputSelection in oldOutputSelections) {
    ospsuite::addOutputs(
      quantitiesOrPaths = outputSelection$path,
      simulation = simulation
    )
  }

  # Add additional `S3` class attribute.
  # Helpful for plotting methods to recognize this object.
  class(results) <- c("SensitivityCalculation", class(results))

  # Return the data in a list.
  return(results)
}
