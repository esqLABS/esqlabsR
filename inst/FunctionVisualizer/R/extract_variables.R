#' Extract Variable Names from an Equation
#'
#' This function processes a given equation string to extract variable names.
#' It removes blanks, splits the equation based on arithmetic operators,
#' and excludes numeric values and predefined mathematical expressions.
#'
#' @param equation A character string representing the mathematical equation.
#' @param mathexpressions A character vector of mathematical functions to exclude.
#'' Defaults to c("exp", "log", "sin", "cos", "tan", "sqrt", "floor", "ceiling", "pi").
#'
#' @return A character vector of variable names extracted from the equation.
#'
#' @examples
#' extract_variable_names("a * (b + 5) / c - sqrt(d)")
#' # Returns: "a" "b" "c" "d"
#'
#' extract_variable_names("x^2 + y - log(z)", c("exp", "log", "sin", "cos", "tan", "sqrt", "floor", "ceiling", "pi"))
#' # Returns: "x" "y" "z"
extract_variable_names <- function(equation,
                                   mathexpressions = c("exp", "log", "sin", "cos", "tan", "sqrt", "floor", "ceiling", "pi")) {
  # Check input types
  if (!is.character(equation) || !is.character(mathexpressions)) {
    stop("Both 'equation' and 'mathexpressions' must be character vectors.")
  }

  # Remove all blanks from the equation
  cleaned_eq <- gsub("[[:blank:]]", "", equation)

  # Split the equation string at arithmetic operators and parentheses
  split_eq <- unlist(strsplit(
    cleaned_eq,
    "[-+*/^(){}]|(%%)|(%/%)"
  ))

  # Remove numeric values and predefined mathematical expressions to get variable names
  varnames <- setdiff(
    grep("[^[:digit:]*.?[:digit:]+]", split_eq, value = TRUE),
    mathexpressions
  )

  return(varnames)
}

##' Tests
# print(extract_variable_names("a * (b + 5) / c - sqrt(d)"))
# # Expected output: "a" "b" "c" "d"
#
# print(extract_variable_names("x^2 + y - log(z)", c("exp", "log", "sin", "cos", "tan", "sqrt", "floor", "ceiling", "pi")))
# # Expected output: "x" "y" "z"
