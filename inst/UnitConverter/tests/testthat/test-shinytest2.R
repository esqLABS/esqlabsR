library(shinytest2)

test_that("{shinytest2} recording: UnitConverter: kg to g works as expected", {
  skip_on_ci()
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "UnitConverter",
    height = 619,
    width = 979
  )
  app$set_inputs(dimension = "Amount")
  app$set_inputs(unit1 = "kg")
  app$set_inputs(unit2 = "g")
  app$set_inputs(value1 = 1)
  app$expect_screenshot()
})
