require(rstudioapi)
require(remotes)

# To update esqlabsR package and all its dependencies
rstudioapi::restartSession(command = 'remotes::install_github("esqLABS/esqlabsR", force = TRUE, upgrade = TRUE)')


install_option <- utils::menu(c("Yes", "No"),
                              title = "Do you want to update local (project) PKSIM installation"
)

if (install_option == 1) {
  # To update project PKSIM installation
  download.file("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/pksim_minimal.zip",
                destfile = "pksim_minimal.zip"
  )
  unzip("pksim_minimal.zip", exdir = "PKSim")
  file.remove("pksim_minimal.zip")
}

rstudioapi::restartSession(command = "library(esqlabsR)")

