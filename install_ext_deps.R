install.packages('https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip', repos = NULL, type = 'binary')

# Install from CRAN using Imports
# install.packages(c('FME', 'readr', 'tidyr', 'ggplot2', 'patchwork', 'GenSA'), repos = 'http://cran.us.r-project.org')"

# Installed from github with remotes
# download.file('https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/ospsuite.zip?pr=false', destfile = 'ospsuite.zip', mode='wb'); install.packages('ospsuite.zip', repos = NULL, type = 'binary')
# download.file('https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-rutils/artifacts/ospsuite.utils.zip?pr=false', destfile = 'ospsuite.utils.zip', mode='wb'); install.packages('ospsuite.utils.zip', repos = NULL, type = 'binary')
# download.file('https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/tlf-library/artifacts/tlf.zip?pr=false', destfile = 'tlf.zip', mode='wb');  install.packages('tlf.zip', repos = NULL, type = 'binary')

# No longer depending on
# download.file('https://ci.appveyor.com/api/projects/StephanSchaller/esqlabs-parameteridentification/artifacts/ospsuite.parameteridentification.zip?pr=false', destfile = 'ospsuite.parameteridentification.zip', mode='wb', headers = c('Authorization' = 'Bearer v2.4n7xybvbbboj2ye9fmk5')); install.packages('ospsuite.parameteridentification.zip', repos = NULL, type = 'binary')"


# Install suggested dependencies
install.packages(c("clipr",
                   "knitr",
                   "openxlsx",
                   "rmarkdown",
                   "shinytest2",
                   "testthat",
                   "vdiffr",
                   "withr"),
                 repos = 'http://cran.us.r-project.org')
