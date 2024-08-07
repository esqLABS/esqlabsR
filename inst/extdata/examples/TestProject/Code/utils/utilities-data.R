writeDataNamesIntoExcel <- function(obsDataSets, projectConfiguration) {
  ObservedDataNames <- sort(names(obsDataSets))
  wb <- openxlsx::loadWorkbook(file = projectConfiguration$plotsFile)
  openxlsx::writeData(wb = wb, sheet = "ObservedDataNames", x = (as.data.frame(ObservedDataNames)))
  openxlsx::saveWorkbook(wb, "../Parameters/Plots.xlsx", overwrite = TRUE)
}
