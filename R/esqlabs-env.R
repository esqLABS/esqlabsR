# Environment that holds various global variables and settings for the esqlabsR,
# It is not exported and should not be directly manipulated by other packages.
esqlabsEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
esqlabsEnv$packageName <- "esqlabsR"

# Version of the package
esqlabsEnv$packageVersion <- getNamespaceVersion("esqlabsR")

#' Names of the settings stored in esqlabsEnv Can be used with `getEsqlabsRSetting()`
#' @export
esqlabsRSettingNames <- enum(names(esqlabsEnv))

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
    stop(messages$errorPackageSettingNotFound(settingName, esqlabsEnv))
  }

  obj <- esqlabsEnv[[settingName]]
  # Evaluate if the object is a function. This is required since some properties are defined as function reference
  if (is.function(obj)) {
    return(obj())
  }

  return(obj)
}

.getEsqlabsColors <- function() {
  # Set esqlabs color palette
  # Total number of colors
  nrOfColors <- 52
  # First three colors are fixed
  firstColors <- esqlabsColors(3)
  # Add secondary colors
  firstColors <- c(
    firstColors,
    "#e6a489",
    "#8ac8b9",
    "#9f8fcd",
    "#74a778",
    "#5e6598",
    "#c87699"
  )
  # Get colors in between the three fixed colors
  unsortedColors <- esqlabsColors(nrOfColors)
  unsortedColors <- setdiff(unsortedColors, firstColors)
  # nrOfColors - 3 because the first three colors are already sampled
  nrPerColor <- floor((nrOfColors - 3) / 3)
  firstColor <- seq(1, by = 3, length.out = floor(nrPerColor))
  thirdColor <- rev(seq(3, by = 3, length.out = floor(nrPerColor)))
  secondColor <- setdiff(1:(nrOfColors - 3), c(firstColor, thirdColor))
  idxs <- c(firstColor, secondColor, thirdColor)
  sortedColors <- vector("character", length = nrOfColors - 3)
  for (i in (1:(nrOfColors - 3))) {
    sortedColors[idxs[i]] <- unsortedColors[i]
  }

  return(c(firstColors, sortedColors))
}
