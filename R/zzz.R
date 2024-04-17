# nocov start
.onLoad <- function(...) {
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")

  .setEsqlabsColors()

  # Change maximal caption width in figures coming from TLF
  tlf::setDefaultMaxCharacterWidth(75)
}
# nocov end

.setEsqlabsColors <- function() {
  # Set esqlabs color palette
  # Total number of colors
  nrOfColors <- 52
  # First three colors are fixed
  firstColors <- esqLABS_colors(3)
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
  unsortedColors <- esqLABS_colors(nrOfColors)
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

  esqlabsEnv$colorPalette <- c(firstColors, sortedColors)
}
