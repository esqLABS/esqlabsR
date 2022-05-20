#' @name sensitivityCalculation
#' @title Carry out and visualize sensitivity analysis (with OSPSuite)
#'
#' @param simulation An object of type `Simulation`.
#' @param outputPaths Path (or a vector of paths) to the output(s) for which the
#'   sensitivity will be analyzed.
#' @param parameterPaths A single or a vector of the parameter path(s) to be
#'   varied.
#' @param variationRange Optional numeric vector defining the scaling of the
#'   parameters. The same variation range is applied to all specified
#'   parameters. If not specified, the following vector will be used: c(0.1,
#'   0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5,  6, 7, 8, 9, 10).
#' @param pkParameters A vector of names of PK parameters for which the
#'   sensitivities will be calculated. For a full set of available standard PK
#'   parameters, run `names(ospsuite::StandardPKParameter)`. By default, only
#'   the following parameters will be considered: `"C_max"`, `"t_max"`,
#'   `"AUC_inf"`. If `NULL`, all available PK-parameters will be calculated. You
#'   can also specify custom PK parameters.
#' @param pkDataFilePath Path to excel file in which
#'   PK-parameter data should be saved. If a file already exists, it will be
#'   overwritten. Default is `NULL`, meaning the data will not be saved to a
#'   spreadsheet.
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions` used during the simulation run
#'
#' @family sensitivity-calculation
#'
#' @return
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
#' }
#' @export
sensitivityCalculation <- function(simulation,
                                   outputPaths,
                                   parameterPaths,
                                   variationRange = c(seq(0.1, 1, by = 0.1), seq(2, 10, by = 1)),
                                   pkParameters = c("C_max", "t_max", "AUC_inf"),
                                   pkDataFilePath = NULL,
                                   simulationRunOptions = NULL) {
  # input validation ------------------------

  # Validate vector arguments of character type
  .validateCharVectors(outputPaths)
  .validateCharVectors(parameterPaths)
  .validateCharVectors(pkParameters)

  # Check for non-standard PK parameters
  .validatePKParameters(pkParameters)

  # Check provided variation range using custom function.
  # This also makes sure that there is always `1.0` present in this vector.
  variationRange <- .validateVariationRange(variationRange)

  # Fail early to avoid costly failure after analysis is already carried out.
  if (!is.null(pkDataFilePath)) {
    validateIsFileExtension(pkDataFilePath, "xlsx")
  }

  # creating `SimulationResults` batch ------------------------

  # Store old simulation outputs and set user defined
  oldOutputSelections <- simulation$outputSelections$allOutputs
  clearOutputs(simulation = simulation)
  addOutputs(quantitiesOrPaths = outputPaths, simulation = simulation)

  # Create as few simulation batches as possible.
  # All constant parameters can be simulated in one batch. Each formula
  # parameter must be a separate batch.
  constantParamPaths <- list()
  formulaParamPaths <- list()

  # Store initial values of the parameters, i.e., where scale factor is 1.
  initialValues <- vector("double", length(parameterPaths))
  names(initialValues) <- parameterPaths

  # Each simulation batch result has an ID.
  # Create a map of IDs to varied parameter paths and scale factors
  # (alternatively, values of the parameters could be used as keys).
  batchResultsIdMap <- vector("list", length(parameterPaths))
  names(batchResultsIdMap) <- parameterPaths

  for (parameterPath in parameterPaths) {
    # Initialize `batchResultsIdMap` for the current parameter
    batchResultsIdMap[[parameterPath]] <- vector("list", length(variationRange))
    names(batchResultsIdMap[[parameterPath]]) <- variationRange

    param <- getParameter(parameterPath, simulation)

    if (param$isConstant) {
      constantParamPaths <- c(constantParamPaths, parameterPath)
    } else {
      formulaParamPaths <- c(formulaParamPaths, parameterPath)
    }

    initialValues[[parameterPath]] <- param$value
  }

  constantParamPaths <- unlist(constantParamPaths)
  formulaParamPaths <- unlist(formulaParamPaths)

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
      for (scaleFactorIdx in seq_along(variationRange)) {
        # Change the value of the varied parameter
        runValues <- initialValues[constantParamPaths]
        runValues[[constantParamPath]] <- variationRange[[scaleFactorIdx]] * runValues[[constantParamPath]]

        # Add run values and store the ID in the `batchResultsIdMap`
        batchResultsIdMap[[constantParamPath]][[scaleFactorIdx]] <- constantBatch$addRunValues(parameterValues = runValues)
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
    for (scaleFactorIdx in seq_along(variationRange)) {
      batchResultsIdMap[[formulaParamPath]][[scaleFactorIdx]] <- formulaBatch$addRunValues(parameterValues = variationRange[[scaleFactorIdx]] * initialValues[[formulaParamPath]])
    }

    simulationBatches <- c(simulationBatches, formulaBatch)
  }

  # Simulate all batches in parallel
  simulationBatchesResults <- runSimulationBatches(
    simulationBatches = simulationBatches,
    simulationRunOptions = simulationRunOptions
  )

  # `runSimulationBatches()` returns a list with one entry per simulation batch.
  #
  # Unlist so all results are in one list. The names of the results are the IDs
  # of the runs. Using `batchResultsIdMap`, results for a certain
  # parameter/scale factor combination can be filtered out.
  simulationBatchesResults <- unlist(simulationBatchesResults)

  # Create a nested list of simulation batch results.
  # Outer list indexes each parameter path, while inner list corresponds to each
  # value of parameter factor.
  simulationResultsBatch <- batchResultsIdMap

  for (parameterPath in seq_along(simulationResultsBatch)) {
    for (parameterFactor in seq_along(simulationResultsBatch[[parameterPath]])) {
      simulationResultsBatch[[parameterPath]][[parameterFactor]] <- purrr::pluck(simulationBatchesResults, batchResultsIdMap[[parameterPath]][[parameterFactor]])
    }
  }

  # extract and save data frames ------------------------

  # Extract data frame for PK parameters
  pkData <- .simulationResultsBatchToPKDataFrame(simulationResultsBatch, parameterPaths)

  # Filter out unneeded PK parameters
  if (!is.null(pkParameters)) {
    pkData <- dplyr::filter(pkData, PKParameter %in% pkParameters)
  }

  # Write each wide data frame in a list to a separate sheet in Excel
  if (!is.null(pkDataFilePath)) {
    # Convert tidy data to wide format for each output path
    pkData_wide_list <- purrr::map(
      .x = pkData %>% split(.$OutputPath),
      .f = ~ .convertToWide(.x)
    )

    # The output paths can be quite long and don't make for good sheet names, so
    # use `OutputPathXXX` naming pattern for sheets instead.
    names(pkData_wide_list) <- paste0("OutputPath", seq(1:length(unique(pkData$OutputPath))))

    # Write to a spreadsheet with one sheet per output path.
    writexl::write_xlsx(pkData_wide_list, pkDataFilePath)
  }

  # return `SensitivityCalculation` ------------------------

  # Final list with needed objects and data frames for plotting functions.
  results <- list(
    "simulationResults" = simulationResultsBatch,
    "outputPaths"       = outputPaths,
    "parameterPaths"    = parameterPaths,
    "pkData"            = pkData
  )

  # Reset simulation outputs
  oldOutputSelections <- simulation$outputSelections$allOutputs
  clearOutputs(simulation = simulation)

  for (outputSelection in oldOutputSelections) {
    ospsuite::addOutputs(quantitiesOrPaths = outputSelection$path, simulation = simulation)
  }

  # Add additional `S3` class attribute.
  # Helpful for plotting methods to recognize this object.
  class(results) <- c("SensitivityCalculation", class(results))

  # Return the data in a list.
  return(results)
}
