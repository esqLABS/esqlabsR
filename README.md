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

- rClr >0.9.2
  - https://github.com/Open-Systems-Pharmacology/rClr/releases/ or esqLABS internal
- ospsuite.utils
  - https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils or esqLABS internal
- tlf
  - https://github.com/Open-Systems-Pharmacology/TLF-Library or esqLABS internal
- ospsuite-r > 11.0
  - https://github.com/Open-Systems-Pharmacology/OSPSuite-R or esqLABS internal
  
### For projects created for version 3

To run code written for version 3 of `esqlabsR` package, additionally install the
[esqlabsRLegacy](https://github.com/esqLABS/esqlabsRLegacy) package.

### From CRAN:

- jsonlite
- patchwork
- purrr
- R6
- readr
- stringr
- tidyr
- colorspace
- dplyr
- ggplot2
- hash
- readxl
- shiny
- shinyjs
- tools
- vctrs
- writexl

## Contributing

- Follow the OSPS-R [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/CODING_STANDARDS_R.md).
- Some additional useful information can be found [here](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/wiki/Developer-How-To's).

## Code of Conduct

  Please note that the esqlabsR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
