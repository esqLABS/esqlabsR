#' @name sensitivityCalculation
#' @title Carry out and visualize sensitivity analysis (with OSPSuite)
#'
#' @param simulation An object of type `Simulation`.
#' @param outputPaths Path (or a vector of paths) to the output(s) for which the
#'   sensitivity will be analyzed.
#' @param parameterPaths Path (a single or a vector) of the parameter(s) to be
#'   varied.
#' @param variationRange Optional numeric vector defining the scaling of the
#'   parameters. The same variation range should be applied to all specified
#'   parameters. If not specified, the following vector will be used: c(0.1,
#'   0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5,  6, 7, 8, 9, 10).
#' @param pkParameters A vector of names of PK parameters for which the
#'   sensitivities should be calculated. For a full set of available PK
#'   parameters, run `names(ospsuite::StandardpkParameter)`. By default, only
#'   the following parameters will be considered: `"C_max"`, `"t_max"`,
#'   `"AUC_inf"`.
#'
#' @note
#'
#' - PK parameter `"t_max"` in some contexts may not show any variation.
#'
#' @examples
#' library(ospsuite)
#'
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "esqlabsR")
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
#' @export

sensitivityCalculation <- function(simulation,
                                   outputPaths,
                                   parameterPaths,
                                   variationRange = c(seq(0.1, 1, by = 0.1), seq(2, 10, by = 1)),
                                   pkParameters = c("C_max", "t_max", "AUC_inf")) {
  # set outputs to the provided path
  clearOutputs(simulation)
  addOutputs(outputPaths, simulation)

  # create simulation batch for efficient calculations
  parameterPaths <- getAllParametersMatching(parameterPaths, simulation)

  # extract dataframes with results
  batchResults <- purrr::map(
    .x = parameterPaths,
    .f = ~ .extractSimBatchResults(simulation, .x, variationRange, pkParameters)
  )

  # extract individual dataframes for time series and PK analysis
  tsData <- purrr::map_dfr(batchResults, ~ purrr::pluck(.x, "tsData"))
  pkData <- purrr::map_dfr(batchResults, ~ purrr::pluck(.x, "pkData"))

  # save data in the respective sheets in a spreadsheet
  # one sheet per output path
  pkData <- pkData %>%
    tidyr::nest(data = -OutputPath) %>%
    dplyr::mutate(rowid = paste0("OutputPath", seq(1:nrow(.)))) %>%
    tidyr::unnest(cols = c(data))

  # convert tidy data to wide format and add them to a list
  pkData_wide_list <- purrr::map(
    .x = split(pkData, pkData$rowid),
    .f = ~ .convertToWide(.x)
  )

  # write each wide dataframe in a list to a separate sheet in Excel
  writexl::write_xlsx(pkData_wide_list, "PKdata.xlsx")

  # remove the unneeded column
  pkData <- dplyr::select(pkData, -rowid)

  # return the data in a list
  return(list("tsData" = tsData, "pkData" = pkData))
}
