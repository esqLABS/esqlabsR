
<!-- README.md is generated from README.Rmd. Please edit that file -->

# esqlabsR <a href="https://esqlabs.github.io/esqlabsR"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/esqlabs/esqlabsr?branch=develop&svg=true)](https://ci.appveyor.com/project/StephanSchaller/esqlabsr/branch/develop)
[![codecov](https://codecov.io/gh/esqlabs/esqlabsr/branch/develop/graph/badge.svg)](https://codecov.io/gh/esqlabs/esqlabsr)

<!-- badges: end -->

## Overview

The goal of **{esqlabsR}** is to facilitate and standardize **modeling
and simulation** (M&S) of **PBPK** and **QSP** models implemented in
[Open Systems Pharmacology
Software](https://www.open-systems-pharmacology.org/) and executed from
R. The package provides functions to read and run scenarios, workflows,
and simulations. Additionally, it generates visualizations based on
non-code input from Excel files. By utilizing {esqlabsR} for your M&S
needs, you can streamline your workflow and ensure standardized data
practices. Learn more about the **{esqlabsR}**’s workflow in
`vignette("standard-workflow")`.

## Installation

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

remotes::install_github("esqLABS/esqlabsR")
```

### For projects created for version 3

To run code written for version 3 of `esqlabsR` package, additionally
install the [esqlabsRLegacy](https://github.com/esqLABS/esqlabsRLegacy)
package.

## Usage

``` r
library(esqlabsR)

my_project_configuration <- createDefaultProjectConfiguration(path = "tests/data/TestProject/Code/ProjectConfiguration.xlsx")

scenarioConfigurations <- readScenarioConfigurationFromExcel(
  scenarioNames = "TestScenario",
  projectConfiguration = my_project_configuration
)


scenarios <- createScenarios(scenarioConfigurations)

simulatedScenariosResults <- runScenarios(
  scenarios = scenarios
)
```

in esqlabsR, simulations are run by defining and executing multiple
scenarios. A scenario is defined by:

- The model,
- The parameterization of the model,
- The application protocol,
- \[optional\] The physiology of the individual or population.

All these inputs can be setup in a collection excel files with
predefined structures.

The package includes an example scenario that models the administration
of a single dose of 250 mg aciclovir intravenously to an individual with
a 90 ml/min estimated glomerular filtration rate. Here is a preview of
it content:

<div class="kable-table">

| Scenario_name             | IndividualId | PopulationId   | ApplicationProtocol | ModelFile      | ModelParameterSheets |
|:--------------------------|:-------------|:---------------|:--------------------|:---------------|:---------------------|
| TestScenario              | Indiv1       | NA             | Aciclovir_iv_250mg  | Aciclovir.pkml | Global               |
| TestScenario2             | Indiv        | NA             | Aciclovir_iv_250mg  | Aciclovir.pkml | Global               |
| PopulationScenario        | Indiv        | TestPopulation | Aciclovir_iv_250mg  | Aciclovir.pkml | Global               |
| PopulationScenarioFromCSV | Indiv        | TestPopulation | Aciclovir_iv_250mg  | Aciclovir.pkml | Global               |

</div>

All these variables are linking to other

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
