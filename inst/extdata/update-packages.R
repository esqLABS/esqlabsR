require(rstudioapi)
require(remotes)

# To update esqlabsR package and all its dependencies
rstudioapi::restartSession(command = 'remotes::install_github("esqLABS/esqlabsR", force = TRUE, upgrade = TRUE)')
