#' Hill equation, transforming a value of an effector (e.g. concentration) to a
#' saturable function value.
#'
#' @param x Value of the effector.
#' @param Vmax Maximal value of the function.
#' @param Km Value of the effector at which the value of the function is half-maximal.
#' @param alpha Hill coefficient defining the steepness of the function. Default is `1`.
#'
#' @return Vmax * x^alpha / (x^alpha + Km^alpha)
#' @export
hillFunction <- function(x, Vmax, Km, alpha = 1) {
  Vmax * x^alpha / (x^alpha + Km^alpha)
}

#' Function returning the fold difference between x and x_0
#'
#' @param x Value of the effector.
#' @param x_0 Basal value of the effector.
#' @param alpha Steepness of the function. Default is `1`.
#'
#' @return (x / x_0)^alpha
#' @export
foldChangeFunction <- function(x, x_0, alpha = 1) {
  (x / x_0)^alpha
}

#' Sine function
#'
#' @param x Value of the effector
#' @param amplitude Numeric, amplitude of the sine function
#' @param period Numeric, amplitude of the sine function
#' @param xOffset Offset of the function on the x-axis
#' @param yOffset Offset of the function on the y-axis
#'
#' @return Value of the sine function for x.
#' @export
sineFunction <- function(x, amplitude, period, xOffset, yOffset) {
  amplitude * sin((2 * pi / period) * (x + xOffset)) + yOffset
}
