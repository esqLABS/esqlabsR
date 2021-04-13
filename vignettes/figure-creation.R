## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, results = "hide", message = F-----------------------------
library(esqlabsR)

## ----createDataMapping--------------------------------------------------------
dataMapping <- DataMapping$new()
print(dataMapping)

