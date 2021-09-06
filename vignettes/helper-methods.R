## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, results = "hide", message = F-----------------------------
library(esqlabsR)

## ----getEsqlabsRSetting, error=TRUE-------------------------------------------
getEsqlabsRSetting("maxNumberOfCores")

getEsqlabsRSetting("Setting that does not exist")
