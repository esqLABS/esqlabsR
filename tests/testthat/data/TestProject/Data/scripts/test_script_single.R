ds <- ospsuite::DataSet$new(name = "ScriptGenerated")
ds$setValues(
  xValues = c(0, 1, 2, 4, 8),
  yValues = c(0, 10, 8, 5, 2)
)
ds$xUnit <- "h"
ds$yUnit <- "mg/l"
ds
