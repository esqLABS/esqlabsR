
<!-- README.md is generated from README.Rmd. Please edit that file -->

# esqlabsR <a href="https://esqlabs.github.io/esqlabsR/"><img src="man/figures/logo.png" align="right" height="139" alt="esqlabsR website" /></a>

<!-- badges: start -->

[![Build
status](https://img.shields.io/github/actions/workflow/status/esqlabs/esqlabsR/merge-to-main.yaml?branch=main&label=Build)](https://github.com/esqlabs/esqlabsR/actions/workflows/merge-to-main.yaml)
[![Codecov test
coverage](https://codecov.io/gh/esqlabs/esqlabsR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/esqlabs/esqlabsR?branch=main)
<!-- badges: end -->

The `{esqlabsR}` package facilitates and standardizes the modeling and
simulation of physiologically based kinetic (PBK) and quantitative
systems pharmacology/toxicology (QSP/T) models implemented in the [Open
Systems Pharmacology Suite](https://www.open-systems-pharmacology.org/)
(OSPS).

The `{esqlabsR}` package is designed for PBK modelers who use OSPS. By
using this package, you can streamline your modeling and simulation
(M&S) workflow and ensure standardized and reproducible practices.

The package provides functions to:

- Design, import, and run simulations,
- Generate standardized plots and other reporting materials,
- Validate and share reproducible project configurations.

To get started with the esqlabsR package, please follow the [Get Started
tutorial](https://esqlabs.github.io/esqlabsR/articles/esqlabsR.html).

## Installation

### Pre-requisites

<!-- As `{esqlabsR}` relies indirectly on `{rSharp}`, it requires its external dependencies (Visual C++ Redistributable and .NET 10). Install them by following these instructions: -->

- [For
  Windows](https://github.com/Open-Systems-Pharmacology/rSharp?tab=readme-ov-file#prerequisites)
- [For
  Linux](https://github.com/Open-Systems-Pharmacology/rSharp?tab=readme-ov-file#ubuntu)

### Install the package

`{esqlabsR}` depends on OSP packages that are not on CRAN. They are
published on the [OSP
R-universe](https://open-systems-pharmacology.r-universe.dev), so add it
to your repositories first:

``` r
options(
  repos = c(
    OSP = "https://open-systems-pharmacology.r-universe.dev",
    getOption("repos")
  )
)
```

You can then install the package by running:

``` r
install.packages("pak")
pak::pak("esqLABS/esqlabsR@*release")
```

The latest development version of the package can also be installed
with:

``` r
pak::pak("esqLABS/esqlabsR")
```

Note: For projects created for version 3 of the `esqlabsR` package,
refer to [`esqlabsRLegacy`](https://github.com/esqLABS/esqlabsRLegacy).

## Usage

An `{esqlabsR}` project consists of multiple files, all stored within
one folder. At its root sits a `Project.json` file alongside a
`definitions/` folder that holds the **definition** files. The
definitions are, for example, the scenarios, populations, individuals,
and plots. The package comes with an example project that you can load
and run directly.

The quickest way to see the workflow end-to-end is to load the example
project, run one of its scenarios, and plot the result:

``` r
library(esqlabsR)

# Load the example project shipped with the package.
project <- loadProject(exampleProjectPath())

# Run a single scenario.
results <- runScenarios(project, scenarios = "aciclovir_iv")

# Build the project's plots from the scenario results.
# `createPlots()` returns a named list of Plot Grids.
plots <- createPlots(project, scenarioResults = results)

plots$individual_diagnostics
```

`runScenarios()` returns a list with one entry per scenario, named after
the scenario. Each entry holds the prepared simulation, its results, the
extracted output values, and the population (or `NULL` for individual
scenarios).

To start your own project from scratch, create a new project with the
required folder structure and files in the current working directory (or
provide a path to the folder you want to create the project in) with:

``` r
esqlabsR::initProject()
```

Then load the project into your R session:

``` r
project <- esqlabsR::loadProject()
```

You then create and edit the project’s definitions (scenarios,
individuals, populations, parameter sets, output paths, observed data,
and plots) with the `add*()`, `set*()`, and `remove*()` functions. Each
of these requires a definition `id` (a unique name), for example
`addScenario(project, id = "aciclovir_iv", ...)`. These edits stay in
your R session; `saveProject()` then writes them to the definition
files, so nothing on disk changes until you ask for it. Once your
project is configured, you run simulations with `runScenarios()`, create
plots with `createPlots()`, and check the project for configuration
problems with `validateProject()`.

To share or archive a project, create a single self-contained
`.esqlabsR` snapshot with `snapshotProject()` and read it back with
`restoreProject()`.

A snapshot is not a copy of the project folder. It differs in three
ways:

- **One file instead of a folder.** The `Project.json` file and every
  definition file under `definitions/` are written into one `.esqlabsR`
  file, so a project configuration travels as a single attachment. The
  file name carries a timestamp by default, which makes a snapshot a
  dated record you can keep next to a report or a set of results.
- **It captures your R session, not the folder.** The snapshot holds the
  project as it is in your R session, unsaved edits included, and
  writing it does not change the project folder. That makes it a save
  point: try something out, and if it does not work out, go back with
  `restoreProject(snapshot, dir, overwrite = TRUE)`. A project you
  created in R that has no folder yet can be snapshotted as well.
- **It holds the configuration only.** Model files, observed data files,
  population CSV files, and scripts are not packed into the snapshot.
  They have to reach the recipient another way, for example alongside
  the snapshot or on a shared drive that both of you point at.

Copy or zip the whole project folder when you want to hand over
everything, including the model and data files. Use a snapshot when the
configuration is what you want to share, archive, or return to.
`restoreProject()` turns a snapshot back into a normal project folder
with `Project.json` and `definitions/`, and nothing is lost in the round
trip. The [How to set up a
project](https://esqlabs.github.io/esqlabsR/articles/how-to-set-up-a-project.html)
article shows the workflow step by step.

## Learn more

The articles on the [package
website](https://esqlabs.github.io/esqlabsR/) are grouped by what you
need from them. **Get started** is a single lesson through the whole
workflow. The **How-to** guides each take one task, such as designing
scenarios, running simulations, or plotting results. **Explanation**
covers why the project model and the plotting model work the way they
do. The **Reference articles** describe the project file and every
definition field, including which fields are required and what happens
when you omit one.

## Related Work

`{esqlabsR}` relies on the following Open Systems Pharmacology R
packages:

- [rSharp](https://github.com/Open-Systems-Pharmacology/rSharp/)
- [ospsuite.utils](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils)
- [tlf](https://github.com/Open-Systems-Pharmacology/TLF-Library)
- [ospsuite](https://github.com/Open-Systems-Pharmacology/OSPSuite-R)

## Contributing

- Follow the OSPS-R [coding
  standards](https://dev.open-systems-pharmacology.org/r-development-resources/coding_standards_r).
- Our contribution guide can be found
  [here](https://dev.open-systems-pharmacology.org/r-development-resources/collaboration_guide).

## Code of Conduct

Please note that the esqlabsR project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
