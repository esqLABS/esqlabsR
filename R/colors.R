# Color palette ----

#' esqLABS color palette
#'
#' Returns the list of colors extrapolated between the esqLABS colors blue, red,
#' and green.
#'
#' For `nrOfColors` == 1, the esqLABS-blue is returned For `nrOfColors` == 2,
#' the esqLABS-blue and green are returned For `nrOfColors` == 3, the
#' esqLABS-blue, red, and green are returned For `nrOfColors` > 3, the three
#' esqLABS colors are fixed, and the remaining colors are extrapolated from blue
#' to red to green. If `nrOfColors` is uneven, the blue-to-red section becomes
#' one color more than the red-to-green section. In this implementation,
#' blue-to-green is not considered.
#'
#' @param nrOfColors Positive integer defining the number of colors to be
#'   generated.
#'
#' @import grDevices
#' @returns A list of colors as HEX values.
#' @import grDevices
#' @export
esqlabsColors <- function(nrOfColors) {
  # esqLABS colors in HSV model
  esqRedHSV <- grDevices::rgb2hsv(234, 94, 94, maxColorValue = 255)
  esqBlueHSV <- grDevices::rgb2hsv(74, 189, 203, maxColorValue = 255)
  esqGreenHSV <- grDevices::rgb2hsv(118, 187, 96, maxColorValue = 255)
  # default color palette.
  esq_palette <- c(
    hsv(esqBlueHSV[1], esqBlueHSV[2], esqBlueHSV[3]),
    hsv(esqRedHSV[1], esqRedHSV[2], esqRedHSV[3]),
    hsv(esqGreenHSV[1], esqGreenHSV[2], esqGreenHSV[3])
  )

  # pre-calculate distances between blue and red and red and green.
  deltaH_b_r <- (esqRedHSV[1] - esqBlueHSV[1])
  deltaS_b_r <- max(esqRedHSV[2], esqBlueHSV[2]) -
    min(esqRedHSV[2], esqBlueHSV[2])
  deltaV_b_r <- max(esqRedHSV[3], esqBlueHSV[3]) -
    min(esqRedHSV[3], esqBlueHSV[3])

  deltaH_r_g <- abs(esqRedHSV[1] - (esqGreenHSV[1] + 1))
  deltaS_r_g <- max(esqRedHSV[2], esqGreenHSV[2]) -
    min(esqRedHSV[2], esqGreenHSV[2])
  deltaV_r_g <- max(esqRedHSV[3], esqGreenHSV[3]) -
    min(esqRedHSV[3], esqGreenHSV[3])

  if (nrOfColors < 0) {
    stop(messages$nrOfColorsShouldBePositive(nrOfColors))
  }
  if (nrOfColors == 0) {
    return(c())
  }
  if (nrOfColors == 2) {
    palette <- c(esq_palette[1], esq_palette[3])
    return(palette)
  }
  if (nrOfColors <= 3) {
    palette <- esq_palette[1:nrOfColors]
    return(palette)
  }

  nrOfColorsToGenerate <- nrOfColors - 3

  palette <- esq_palette[1]
  nrOfColors_first <- nrOfColorsToGenerate %/% 2 + nrOfColorsToGenerate %% 2
  nrOfColors_second <- nrOfColorsToGenerate %/% 2
  # calculate the first half - blue to red.
  # Index starts with 1 as it defines the number of colors.
  for (i in 1:nrOfColors_first) {
    deltaH <- deltaH_b_r / (nrOfColors_first + 1)
    deltaS <- deltaS_b_r / (nrOfColors_first + 1)
    deltaV <- deltaV_b_r / (nrOfColors_first + 1)

    h <- esqBlueHSV[1] + deltaH * i
    if (h > 1) {
      h <- h - 1
    }
    s <- min(esqBlueHSV[2], esqRedHSV[2]) + deltaS * i
    v <- min(esqBlueHSV[3], esqRedHSV[3]) + deltaV * i

    palette <- c(palette, hsv(h, s, v))
  }

  palette <- c(palette, esq_palette[2])
  # calculate the second half - red to green.
  # Index starts with 1 as it defines the number of colors.
  if (nrOfColors_second > 0) {
    for (i in 1:nrOfColors_second) {
      deltaH <- deltaH_r_g / (nrOfColors_second + 1)
      deltaS <- deltaS_r_g / (nrOfColors_second + 1)
      deltaV <- deltaV_r_g / (nrOfColors_second + 1)

      h <- esqRedHSV[1] + deltaH * i
      if (h > 1) {
        h <- h - 1
      }
      s <- min(esqGreenHSV[2], esqRedHSV[2]) + deltaS * i
      v <- min(esqGreenHSV[3], esqRedHSV[3]) + deltaV * i

      palette <- c(palette, hsv(h, s, v))
    }
  }
  palette <- c(palette, esq_palette[3])

  return(palette) # nolint: return_linter.
}

#' Returns the HSV values for a given R color name
#'
#' @param color vector of any of the three kinds of R color specifications,
#'   i.e., either a color name (as listed by colors()), a hexadecimal string of
#'   the form "#rrggbb" or "#rrggbbaa" (see rgb), or a positive integer `i`
#'   meaning `palette()[i]`.
#'
#' @returns A matrix with a column for each color. The three rows of the matrix
#'   indicate hue, saturation and value and are named "h", "s", and "v"
#'   accordingly.
#' @export
#' @import ospsuite ospsuite.utils grDevices
#'
#' @examples
#' col2hsv("yellow")
#' @export
col2hsv <- function(color) {
  validateIsString(color)
  rgb <- col2rgb(color)
  return(grDevices::rgb2hsv(rgb))
}
