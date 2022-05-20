#' Parallelize the execution of a function over a list of arguments values
#'
#' @param fun A function that will be called with different arguments values
#' @param firstArguments A list of the values of the first argument of the
#'   function. The function will be called `n` times where `n` is the number of
#'   entries in `firstArguments`
#' @param exports Names of the objects in the calling environment that the
#'   function relies on that are not passed as arguments. May be `NULL` (default).
#' @param ... Further arguments of the function.
#' @param nrOfCores Optional: the maximal number of parallel threads. By default
#'   the value defined in `ospsuite::getOSPSuiteSetting("numberOfCores")` is
#'   used, and equals the number of logical cores minus 1.
#' @param outputNames Optional: a list of names used for the output list. Result
#'   of each execution of `fun` will be named with the name having the same
#'   index in `outputNames` as as the argument value in `firstArguments`. If
#'   specified, `outputNames` must have the same length as `firstArguments`
#'
#' @return A list containing the outputs of the function `fun` iterated over the
#'   values in `firstArguments`.
#'
#' @import parallel
#' @seealso{[parLapply()]}
#' @export
executeInParallel <- function(fun,
                              firstArguments,
                              exports = NULL,
                              ...,
                              outputNames = NULL,
                              nrOfCores = ospsuite::getOSPSuiteSetting("numberOfCores")) {
  if (!is.null(outputNames)) {
    validateIsSameLength(firstArguments, outputNames)
  }

  cl <- makeCluster(nrOfCores)

  # Load necessary packages and export the environments
  tmp <- clusterEvalQ(cl, library(esqlabsR))
  tmp <- clusterExport(cl, exports)

  result <- tryCatch(
    {
      parLapply(cl = cl, X = firstArguments, fun = fun, ...)
    },
    error = function(e) {
      stop(e)
    },
    finally = {
      stopCluster(cl)
    }
  )

  names(result) <- outputNames
  return(result)
}
