# TODO: uncomment once esqLABS/esqlabs.ParameterIdentification version can be used
#
# test_that("`exportSteadyStateToXLS` generates excel sheet path", {
#   withr::with_tempdir(
#     code = {
#       simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#       sim <- loadSimulation(simFilePath)
#       path <- exportSteadyStateToXLS(sim)
#       expect_true(endsWith(path, "Aciclovir_SS.xlsx"))
#     }
#   )
# })
