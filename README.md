# esqlabsR <a href="https://esqlabs.github.io/esqlabsR"><img src="man/figures/logo.png" align="right" height="138" /></a>

The **`{esqlabsR}`** R-package is designed to facilitate and standardize **modeling and simulation** (M&S) of PBPK and QSP models implemented in [Open Systems Pharmacology Software](https://www.open-systems-pharmacology.org/) (OSPS) and executed from R. The package provides functions to read and run scenarios, workflows, and simulations. Furthermore, it creates visualizations based on non-code input from Excel files. The package is based on R functions in the [`ospsuite` package](https://github.com/Open-Systems-Pharmacology/OSPSuite-R).

<!-- badges: start -->

  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/esqlabs/esqlabsr?branch=develop&svg=true)](https://ci.appveyor.com/project/StephanSchaller/esqlabsr/branch/develop)
  [![codecov](https://codecov.io/gh/esqlabs/esqlabsr/branch/develop/graph/badge.svg)](https://codecov.io/gh/esqlabs/esqlabsr)
  
<!-- badges: end -->

## Installation


```r
# {esqlabsR} and its Open Systems Pharmacology Suite's dependencies relies on 
# {rClr} (https://github.com/Open-Systems-Pharmacology/rClr) which is not 
# available on CRAN.
# Therefore, these must be installed from github using `{remotes}`.

install.packages("remotes")
install.packages('https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip', 
                  repos = NULL, 
                  type = 'binary')
remotes::install_github("esqLABS/esqlabsR")
``` 

  
### For projects created for version 3

To run code written for version 3 of `esqlabsR` package, additionally install the
[esqlabsRLegacy](https://github.com/esqLABS/esqlabsRLegacy) package.

## Contributing

- Follow the OSPS-R [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/CODING_STANDARDS_R.md).
- Some additional useful information can be found [here](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/wiki/Developer-How-To's).

## Code of Conduct

Please note that the esqlabsR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
