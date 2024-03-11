
<!-- README.md is generated from README.Rmd. Please edit that file -->

# esqlabsR <a href="https://esqlabs.github.io/esqlabsR/"><img src="man/figures/logo.png" align="right" height="139" alt="esqlabsR website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/esqLABS/esqlabsR/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/esqLABS/esqlabsR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/esqlabs/esqlabsR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/esqlabs/esqlabsR?branch=main)
[![pkgdown](https://github.com/esqLABS/esqlabsR/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/esqLABS/esqlabsR/actions/workflows/pkgdown.yml)
[![Download Latest
Build](https://custom-icon-badges.demolab.com/badge/-Download-blue?style=for-the-badge&logo=download&logoColor=white "Download Latest Build")](https://nightly.link/esqLABS/esqlabsR/workflows/R-CMD-check.yml/main/built_package.zip)
<!-- badges: end -->

The `{esqlabsR}` package facilitates and standardizes the modeling and
simulation of physiologically based kinetic (PBK) and quantitative
systems pharmacology/toxicology (QSP/T) models implemented in the [Open
Systems Pharmacology
Software](https://www.open-systems-pharmacology.org/) (OSPS).

The `{esqlabsR}` package is designed for PBK modelers who use the OSPS
suite. By using this package, you can streamline your modeling and
simulation (M&S) workflow and ensure standardized and reproducible
practices.

The package provides functions to:

- Design, import and run Simulations,
- Generate standardized plots and other reporting materials,
- Interact with the OSPS features using simple Excel files.

To get started with the esqlabsR package, please read the
`vignette("esqlabsR")`.

## Installation

Currently, esqlabsR is available only for Windows system. You can
install the package by running:

``` r
# {esqlabsR} and its Open Systems Pharmacology Suite's dependencies relies on
# {rClr} (https://github.com/Open-Systems-Pharmacology/rClr) which is not
# available on CRAN.
# Therefore, these must be installed from github using `{remotes}`.

install.packages("remotes")
install.packages("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip",
  repos = NULL,
  type = "binary"
)

remotes::install_github("esqLABS/esqlabsR@*release")
```

Get the latest development version with:

``` r
remotes::install_github("esqLABS/esqlabsR")
```

Note: For projects created for version 3 of `esqlabsR` package, install
[`esqlabsRLegacy`](https://github.com/esqLABS/esqlabsRLegacy).

## Usage

``` r
# load esqlabsR
library(esqlabsR)

# Load excel-based configuration
my_project_configuration <-
  createDefaultProjectConfiguration(example_ProjectConfiguration())


# Setup simulation scenarios
my_scenarios <-
  createScenarios(
    readScenarioConfigurationFromExcel( # Read scenarios from excel file
      scenarioNames = "TestScenario", # Import the scenario defined as "TestScenario"
      # in the excel file
      projectConfiguration = my_project_configuration
    )
  )

# Run simulations
my_simulation <- runScenarios(
  scenarios = my_scenarios
)

# Initialize a `DataCombined` object to store simulation results
my_datacombined <- DataCombined$new()

my_datacombined$addSimulationResults(my_simulation$TestScenario$results,
  names = "Simulated",
  groups = "Aciclovir"
)

# Plot simulation results
plotIndividualTimeProfile(my_datacombined)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="80%" style="display: block; margin: auto;" />

## Learn More

To get started, first read `vignette("esqlabsR")`. Then read more about
the specific topics:

- Start with `vignette("esqlabsR-workflow-overview")` to learn about the
  esqlabsR’s streamlined workflow.
- `vignette("esqlabsR-project-structure")` details the structure and
  purpose of each component file and directory of an esqlabsR project.
- `vignette("esqlabsR-design-scenarios")` explains how you can design
  your own simulations only using excel files.
- `vignette("esqlabsR-run-simulations")` describes all you need to know
  to run your customized simulations.
- `vignette("esqlabsR-plot-results")` explains how to generate
  visualizations from simulations.

## Related Work

`{esqlabsR}` relies on the following Open Systems Pharmacology R
packages:

- [rClr](https://github.com/Open-Systems-Pharmacology/rClr/)
- [ospsuite.utils](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils)
- [tlf](https://github.com/Open-Systems-Pharmacology/TLF-Library)
- [ospsuite](https://github.com/Open-Systems-Pharmacology/OSPSuite-R)
- [ospsuite.parameteridentification](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification)

## Contributing

- Follow the OSPS-R [coding
  standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/CODING_STANDARDS_R.md).
- Some additional useful information can be found
  [here](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/wiki/Developer-How-To's).

## Code of Conduct

Please note that the esqlabsR project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
