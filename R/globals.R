# Column names that exist only inside a data mask (dplyr verbs and ggplot
# aesthetics that build or read them), which `R CMD check` cannot see are bound
# and so reports as undefined globals. Declaring them here is the alternative to
# rewriting every call site to use the `.data` pronoun.

utils::globalVariables(
  names = c(
    "IndividualId",
    "OutputPath",
    "PKMeanPercentChange",
    "PKParameter",
    "PKParameterBaseValue",
    "PKParameterValue",
    "PKPercentChange",
    "Parameter",
    "ParameterBaseValue",
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
    "xValues",
    "yValues"
  ),
  package = "esqlabsR",
  add = FALSE
)
