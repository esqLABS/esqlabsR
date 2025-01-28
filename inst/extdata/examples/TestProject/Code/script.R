library(esqlabsR)

sourceAll(file.path(getwd(), "Code", "utils"))
sourceAll(file.path(getwd(), "Code", "InputCode"))
sourceAll(file.path(getwd(), "Code", "Scenarios"))
sourceAll(file.path(getwd(), "Code", "TransferFunctions"))

projectConfiguration <- esqlabsR::createProjectConfiguration()
###### Scenarios#########
scenarioResults <- defaultScenario(projectConfiguration = projectConfiguration)
