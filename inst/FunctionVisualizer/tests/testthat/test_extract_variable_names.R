test_that("extractVariableNames correctly identifies variables in a simple equation", {
  equation <- "a * (b + 5) / c - sqrt(d)"
  expected_output <- c("a", "b", "c", "d")
  result <- .extractVariableNames(equation)
  expect_equal(result, expected_output)
})

test_that("extractVariableNames handles mathematical expressions correctly", {
  equation <- "x^2 + y - log(z)"
  mathexpressions <- c("exp", "log", "sin", "cos", "tan", "sqrt", "floor", "ceiling", "pi")
  expected_output <- c("x", "y", "z")
  result <- .extractVariableNames(equation, mathexpressions)
  expect_equal(result, expected_output)
})

test_that("extractVariableNames excludes predefined mathematical expressions", {
  equation <- "exp(x) + sin(y) - cos(z)"
  mathexpressions <- c("exp", "log", "sin", "cos", "tan", "sqrt", "floor", "ceiling", "pi")
  expected_output <- c("x", "y", "z")
  result <- .extractVariableNames(equation, mathexpressions)
  expect_equal(result, expected_output)
})

test_that("extractVariableNames handles equations without variables", {
  equation <- "2 + 3 * 4 - sqrt(16)"
  expected_output <- character(0)
  result <- .extractVariableNames(equation)
  expect_equal(result, expected_output)
})

test_that("extractVariableNames handles custom operatorTokenRegex", {
  equation <- "a %% b + c %/% d"
  operatorTokenRegex <- "[-+*/^(){}]|(%%)|(%/%)"
  expected_output <- c("a", "b", "c", "d")
  result <- .extractVariableNames(equation, operatorTokenRegex = operatorTokenRegex)
  expect_equal(result, expected_output)
})

test_that("extractVariableNames throws an error with incorrect input types", {
  expect_error(.extractVariableNames(123), "Both 'equation', 'mathexpressions' and 'operatorTokenRegex' must be character vectors.")
  expect_error(.extractVariableNames("a + b", mathexpressions = 123), "Both 'equation', 'mathexpressions' and 'operatorTokenRegex' must be character vectors.")
})
