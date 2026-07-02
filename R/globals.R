# These variables are set to NULL to avoid R CMD Check warning
# 'no visible global function definition for ..."

# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    "dataCombinedName",
    "IndividualId",
    "OutputPath",
    "OutputPathId",
    "PKMeanPercentChange",
    "PKParameter",
    "PKParameterValue",
    "PKPercentChange",
    "Parameter",
    "ParameterFactor",
    "ParameterPath",
    "ParameterPathLabel",
    "ParameterPathUserName",
    "ParameterValue",
    "QuantityPath",
    "Scenario_name",
    "SensitivityPKParameter",
    "Study Id",
    "Unit",
    "Value",
    "dataType",
    "name",
    "paths",
    "scenario",
    "xOffsets",
    "xScaleFactors",
    "xValues",
    "yOffsets",
    "yScaleFactors",
    "yValues"
  ),
  package = "esqlabsR",
  add = FALSE
)
