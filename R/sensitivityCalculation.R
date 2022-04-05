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
                                   pkDataFilePath = NULL) {
  # input validation ------------------------

  # validate vector arguments of character type
  .validateCharVectors(outputPaths)
  .validateCharVectors(parameterPaths)
  .validateCharVectors(pkParameters)

  # check for non-standard PK parameters
  .validatePKParameters(pkParameters)

  # check provided variation range using custom function
  # this also makes sure that there is always `1.0` present in this vector
  variationRange <- .validateVariationRange(variationRange)

  # creating `SimulationResults` batch ------------------------

  # extract a list of `SimulationResults` objects
  simulationResultsBatch <- purrr::map(
    .x = parameterPaths,
    .f = ~ .extractSimulationResultsBatch(
      simulation     = simulation,
      parameterPath  = .x,
      variationRange = variationRange
    )
  )

  # name list with name of each parameter path
  names(simulationResultsBatch) <- parameterPaths

  # extract and save dataframes ------------------------

  # extract dataframe for PK parameters
  pkData <- .simulationResultsBatchToPKDataFrame(simulationResultsBatch, parameterPaths)

  # filter out unneeded PK parameters
  if (!is.null(pkParameters)) {
    pkData <- dplyr::filter(pkData, PKParameter %in% pkParameters)
  }

  # write each wide dataframe in a list to a separate sheet in Excel
  if (!is.null(pkDataFilePath)) {
    # only `xlsx` format allowed
    if (!isFileExtension(pkDataFilePath, "xlsx")) {
      stop("Only file path with `.xlsx` extension is allowed.")
    }

    # convert tidy data to wide format for each output path
    pkData_wide_list <- purrr::map(
      .x = pkData %>% split(.$OutputPath),
      .f = ~ .convertToWide(.x)
    )

    # the output paths can be quite long and don't make for good sheet names
    # instead use `OutputPathXXX` naming pattern for sheets
    names(pkData_wide_list) <- paste0("OutputPath", seq(1:length(unique(pkData$OutputPath))))

    # write to a spreadsheet with one sheet per output path
    writexl::write_xlsx(pkData_wide_list, pkDataFilePath)
  }

  # return `SensitivityCalculation` ------------------------

  # final list with needed objects and dataframes for plotting functions
  results <- list(
    "simulationResults" = simulationResultsBatch,
    "outputPaths"       = outputPaths,
    "parameterPaths"    = parameterPaths,
    "pkData"            = pkData
  )

  # add additional S3 class attribute
  # helpful for plotting methods to recognize this object
  class(results) <- c("SensitivityCalculation", class(results))

  # return the data in a list
  return(results)
}
