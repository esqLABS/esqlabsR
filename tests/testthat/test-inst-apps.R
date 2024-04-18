test_that("`UnitConverter` Shiny app works", {
  skip_on_ci()

  appdir <- system.file(package = "esqlabsR", "UnitConverter")
  shinytest2::test_app(appdir)
})


test_that("`FunctionVisualizer` Shiny app works", {
  skip_on_ci()

  appdir <- system.file(package = "esqlabsR", "FunctionVisualizer")
  shinytest2::test_app(appdir)
})
