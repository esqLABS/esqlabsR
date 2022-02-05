# esqlabsR

Utilities functions for modeling and simulation workflows within *esqLABS*.

<!-- badges: start -->

  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/esqlabs/esqlabsr?branch=develop&svg=true)](https://ci.appveyor.com/project/StephanSchaller/esqlabsr/branch/develop)
  [![codecov](https://codecov.io/gh/esqlabs/esqlabsr/branch/develop/graph/badge.svg)](https://codecov.io/gh/esqlabs/esqlabsr)
  
<!-- badges: end -->

## Installation

The latest version of the package comes as a binary `*.zip` and can be downloaded from here (the `.zip` folder under `Artifacts` panel):
<https://ci.appveyor.com/project/StephanSchaller/esqlabsr/build/artifacts>

## Required packages

- rClr >0.9.1
  - https://github.com/Open-Systems-Pharmacology/rClr/releases/ or esqLABS internal
- ospsuite-r > 10.0
  - https://github.com/Open-Systems-Pharmacology/OSPSuite-R or esqLABS internal
- R6 (CRAN)
- readr (CRAN)
- readxl (CRAN)
- writexl (CRAN)
- hash (CRAN)
- shiny (CRAN)
- shinyjs (CRAN)
- vctrs (CRAN)

### For building from source and developing

- Rtools (https://cran.r-project.org/bin/windows/Rtools/)
  - After installation, add the folder to your $PATH: In start menu, type in "PATH", select "Change path environment for user", and add the path to Rtools folder.
- roxygen2 (CRAN)
- devtools (CRAN)
- rmarkdown (CRAN)
- testthat (CRAN)
- knitr (CRAN)
- styler (CRAN)

## Contributing

- Follow the OSPS-R [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/CODING_STANDARDS_R.md)
- Some additional useful information can be found [here](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/wiki/Developer-How-To's)

## Code of Conduct

  Please note that the esqlabsR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
