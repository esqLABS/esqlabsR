pkSimPath <- NULL
# For local testing, if required
# pkSimPath <- "c:\\Program Files\\Open Systems Pharmacology\\PK-Sim 11.1\\"

initPKSim(pkSimPath)
sourceAll(file.path(getwd(), "utils"))
sourceAll(file.path(getwd(), "InputCode"))
sourceAll(file.path(getwd(), "Scenarios"))
sourceAll(file.path(getwd(), "TransferFunctions"))
# Maybe have to provide a path to the file when using in tests, as current wd
# will be different
projectConfiguration <- esqlabsR::createDefaultProjectConfiguration()
###### Scenarios#########
scenarioResults <- defaultScenario(projectConfiguration = projectConfiguration)
