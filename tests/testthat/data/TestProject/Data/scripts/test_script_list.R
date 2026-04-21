ds1 <- ospsuite::DataSet$new(name = "ScriptDataSet1")
ds1$setValues(xValues = c(0, 1, 2), yValues = c(1, 2, 3))
ds1$xUnit <- "h"
ds1$yUnit <- "mg/l"

ds2 <- ospsuite::DataSet$new(name = "ScriptDataSet2")
ds2$setValues(xValues = c(0, 1, 2), yValues = c(4, 5, 6))
ds2$xUnit <- "h"
ds2$yUnit <- "mg/l"

list(ScriptDataSet1 = ds1, ScriptDataSet2 = ds2)
