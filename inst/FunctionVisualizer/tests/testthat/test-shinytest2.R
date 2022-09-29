# library(shinytest2)
#
# test_that("{shinytest2} recording: FunctionVisualizer", {
#   app <- AppDriver$new(
#     variant = platform_variant(),
#     name = "FunctionVisualizer",
#     height = 619,
#     width = 979
#   )
#   app$set_inputs(argument = "C")
#   app$set_inputs(Vmax = 51)
#   app$click("addSnapshot")
#   app$set_inputs(Km = 39)
#   app$expect_screenshot()
# })
