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
#' getEsqlabsRSetting("packageName")
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
