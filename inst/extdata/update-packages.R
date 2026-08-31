require(rstudioapi)
require(remotes)

# The OSP dependencies are not on CRAN; they come from the OSP R-universe.
osp_repos <- 'options(repos = c(OSP = "https://open-systems-pharmacology.r-universe.dev", getOption("repos")))'

# To update esqlabsR package and all its dependencies
rstudioapi::restartSession(
  command = paste(
    osp_repos,
    'remotes::install_github("esqLABS/esqlabsR", force = TRUE, upgrade = TRUE)',
    sep = "; "
  )
)
