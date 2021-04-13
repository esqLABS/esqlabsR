#' Parallelize the execution of a function over a list of arguments values
#'
#' @param fun A function that will be called with different arguments values
#' @param firstArguments A list of the values of the first argument of the function. The function
#' will be called \code{n} times where \code{n} is the number of entries in \code{firstArguments}
#' @param exports Names of the objects in the calling environment that the function relies on that are not passed as arguments. May be NULL (default)
#' @param ... Further arguments of the function.
#' @param nrOfCores Optinal: the maximal number of parallel threads. By default the value defined in \code{esqlabsEnv$maxNumberOfCores}
#' is used, and equals the number of logical cores minus 1.
#' @param outputNames Optional: a list of names used for the output list. Result of each execution of \code{fun} will be named with the name having the same index
#' in \code{outputNames} as as the argument value in \code{firstArguments}. If specified, \code{outputNames} must have the same length as \code{firstArguments}
#'
#' @return A list containing the outputs of the function \code{fun} iterated over the values in \code{firstArguments}.
#' @import parallel
#' @seealso{\code{\link{parLapply}}}
#' @export
executeInParallel <- function(fun, firstArguments, exports = NULL, ..., outputNames = NULL, nrOfCores = esqlabsEnv$maxNumberOfCores) {
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
