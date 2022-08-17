# esqlabsR <img src="man/figures/logo.png" align="right" width="240" />

The **`{esqlabsR}`** R-package is designed to facilitate and standardize **modeling and simulation** (M&S) of PBPK and QSP models implemented in [Open Systems Pharmacology Software](https://www.open-systems-pharmacology.org/) (OSPS) and executed from R. The package provides functions to read and run scenarios, workflows, and simulations. Furthermore, it creates visualizations based on non-code input from Excel files. The package is based on R functions in the [`ospsuite` package](https://github.com/Open-Systems-Pharmacology/OSPSuite-R).

<!-- badges: start -->

  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/esqlabs/esqlabsr?branch=develop&svg=true)](https://ci.appveyor.com/project/StephanSchaller/esqlabsr/branch/develop)
  [![codecov](https://codecov.io/gh/esqlabs/esqlabsr/branch/develop/graph/badge.svg)](https://codecov.io/gh/esqlabs/esqlabsr)
  
<!-- badges: end -->

## Installation

Install as a binary file from [an AppVeyor link](https://ci.appveyor.com/project/StephanSchaller/esqlabsr/build/artifacts). The `{esqlabsR}` package requires the following packages: 

* Available from CRAN:
    * jsonlite
    * patchwork
    * purrr
    * R6
    * readr
    * stringr
    * tidyr
    * colorspace
    * dplyr
    * ggplot2
    * hash
    * readxl
    * shiny
    * shinyjs
    * tools
    * vctrs
    * writexl

* Available from github: 
    * [rClr](https://github.com/Open-Systems-Pharmacology/rClr/) > 0.9.2
    * [ospsuite.utils](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils)
    * [tlf](https://github.com/Open-Systems-Pharmacology/TLF-Library)
    * [ospsuite](https://github.com/Open-Systems-Pharmacology/OSPSuite-R) > 11.0
  
### For projects created for version 3

To run code written for version 3 of `esqlabsR` package, additionally install the
[esqlabsRLegacy](https://github.com/esqLABS/esqlabsRLegacy) package.

## Contributing

- Follow the OSPS-R [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/CODING_STANDARDS_R.md).
- Some additional useful information can be found [here](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/wiki/Developer-How-To's).

## Code of Conduct

  Please note that the esqlabsR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
