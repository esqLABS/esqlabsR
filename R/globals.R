# These variables are set to NULL to avoid R CMD Check warning
# 'no visible global function definition for ..."

# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c("ParameterPathLabel",
            "Scenario_name",
            "ParameterPathUserName"),
  package = "esqlabsR",
  add = FALSE
)
