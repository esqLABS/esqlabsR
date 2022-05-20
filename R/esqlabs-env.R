.getPackageVersion <- function() {
  version <- getNamespaceVersion("esqlabsR")
  return(version)
}

# Environment that holds various global variables and settings for the esqlabsR,
# It is not exported and should not be directly manipulated by other packages.
esqlabsEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
esqlabsEnv$packageName <- "esqlabsR"

# Version of the package
esqlabsEnv$packageVersion <- .getPackageVersion()

# Default width of a plot of a single `PlotMapping`
esqlabsEnv$widthPerPlotMapping <- 8

# Default height of a plot of a single `PlotMapping`
esqlabsEnv$heightPerPlotMapping <- 8

# Column names to split observed data by
esqlabsEnv$columnsToSplitDataBy <- c("Group Id", "Gender", "Patient Id", "Dose", "Route", "Molecule", "Organ", "Compartment")

# Column index for x values in observed data files
esqlabsEnv$XValuesColumn <- 10
# Column index for y values in observed data files
esqlabsEnv$YValuesColumn <- 11
# Column index for y error values in observed data files
esqlabsEnv$YErrorColumn <- 12

#' Get the value of a global esqlabsR setting.
#'
#' @param settingName String name of the setting
#'
#' @return Value of the setting stored in esqlabsEnv. If the setting does not
#'   exist, an error is thrown.
#' @export
#'
#' @examples
#' getEsqlabsRSetting("packageVersion")
#' getEsqlabsRSetting("widthPerPlotMapping")
getEsqlabsRSetting <- function(settingName) {
  if (!(any(names(esqlabsEnv) == settingName))) {
    stop(messages$errorEsqlabsRSettingNotFound(settingName))
  }

  obj <- esqlabsEnv[[settingName]]
  # Evaluate if the object is a function. This is required since some properties are defined as function reference
  if (is.function(obj)) {
    return(obj())
  }

  return(obj)
}
