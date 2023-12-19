# Project Template


## How to use


1. Open windows terminal
2. Navigate to the folder where you want to create your project
3. Run the following command in windows terminal and follow instructions:

```bash
quarto use template esqLABS/esqlabsR/inst/extdata/examples/TestProject@add-project-template
```

## Project Setup

Once the project template is copied to your local machine, It is needed to setup the project and environment.

1. Open RStudio
2. Create a New Project > Existing Directory > Browse to the folder where you copied the project template
3. Open the file install_packages.R and run it to install all the typical packages needed for a esqlabsR project.

## Template

A report template file is provided in the project. This file is a `quarto` (or `.qmd`) notebook which contains the typical structure of a modeling and simulation workflow using esqlabsR and demonstrates basic functionality of the package and quarto syntax.

You can test if the project is correctly setup by clicking on "render" button in the top right corner of the `.qmd` file. This will generate a `.html` file that you can view in your browser.

## Create other notebooks

In order to create other notebooks, just copy/paste the original `.qmd` file and rename it.

**NB: notebooks should always be stored at the root of the project and never in subfolders.**
