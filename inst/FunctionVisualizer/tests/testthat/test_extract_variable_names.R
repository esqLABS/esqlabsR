library(shinytest2)
library(testthat)

# Load the Shiny app
app <- AppDriver$new(
  variant = platform_variant(),
  name = "FunctionVisualizer",
  height = 619,
  width = 979
)

test_that("Variable names extraction works correctly", {
  app$set_inputs(equation = "Vmax * C / (Km + C)")
  expect_equal(
    app$get_value(export = "varnames"),
    c("Vmax", "C", "Km")
  )
})

# Stop the Shiny app after tests
app$stop()
