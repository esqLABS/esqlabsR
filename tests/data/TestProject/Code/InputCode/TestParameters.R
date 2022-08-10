getTestParameters <- function(params) {
  paramVals <- enum(list(
    #    "Neighborhoods|Periportal_pls_Periportal_int|Insulin|ActiveTransport_Pl2Int|k_activeTransport" = 0.005
  ))

  paramUnits <- enum(list(
    # "Neighborhoods|Periportal_pls_Periportal_int|Insulin|ActiveTransport_Pl2Int|k_activeTransport" = "cm/min"
  ))

  # Construct the default parameters structure
  paths <- names(paramVals)
  values <- unname(paramVals[paths])
  units <- unname(paramUnits[paths])

  return(list(paths = paths, values = values, units = units))
}
