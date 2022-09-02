library(shinytest2)

test_that("`UnitConverter` Shiny app works", {
  skip_on_cran()

  appdir <- system.file(package = "esqlabsR", "UnitConverter")
  test_app(appdir)
})


test_that("`FunctionVisualizer` Shiny app works", {
  skip_on_cran()

  appdir <- system.file(package = "esqlabsR", "FunctionVisualizer")
  test_app(appdir)
})
