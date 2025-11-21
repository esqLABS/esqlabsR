# ProjectConfiguration

An object storing configuration used project-wide

## Active bindings

- `projectConfigurationFilePath`:

  Path to the file that serve as base path for other parameters. If
  NULL, then, other paths should be absolute paths.

- `projectConfigurationDirPath`:

  Path to the folder that serve as base path for other paths. If NULL,
  then, other paths should be absolute paths.

- `modified`:

  Logical indicating whether any configuration properties have been
  modified since loading from file. Read-only.

- `modelFolder`:

  Path to the folder containing pkml simulation files.

- `configurationsFolder`:

  Path to the folder containing excel files with model parameterization.

- `modelParamsFile`:

  Name of the excel file with global model parameterization. Must be
  located in the "configurationsFolder".

- `individualsFile`:

  Name of the excel file with individual-specific model
  parameterization. Must be located in the "configurationsFolder".

- `populationsFile`:

  Name of the excel file with population information. Must be located in
  the "configurationsFolder".

- `populationsFolder`:

  Name of the folder containing population defined through csv files.
  Must be located in the "configurationsFolder".

- `scenariosFile`:

  Name of the excel file with scenario definitions. Must be located in
  the "configurationsFolder".

- `applicationsFile`:

  Name of the excel file scenario-specific parameters such as
  application protocol parameters. Must be located in the
  "configurationsFolder".

- `plotsFile`:

  Name of the excel file with plot definitions. Must be located in the
  "configurationsFolder".

- `dataFolder`:

  Path to the folder where experimental data files are located.

- `dataFile`:

  Name of the excel file with experimental data. Must be located in the
  "dataFolder"

- `dataImporterConfigurationFile`:

  Name of data importer configuration file in xml format used to load
  the data. Must be located in the "dataFolder"

- `outputFolder`:

  Path to the folder where the results should be saved relative to the
  "Code" folder Initialize

## Methods

### Public methods

- [`ProjectConfiguration$new()`](#method-ProjectConfiguration-new)

- [`ProjectConfiguration$print()`](#method-ProjectConfiguration-print)

- [`ProjectConfiguration$save()`](#method-ProjectConfiguration-save)

- [`ProjectConfiguration$clone()`](#method-ProjectConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    ProjectConfiguration$new(projectConfigurationFilePath = character())

#### Arguments

- `projectConfigurationFilePath`:

  A string representing the path to the project configuration file.
  Print

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print prints a summary of the Project Configuration.

#### Usage

    ProjectConfiguration$print(className = TRUE)

#### Arguments

- `className`:

  Whether to print the name of the class at the beginning. default to
  TRUE.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Export ProjectConfiguration object to ProjectConfiguration.xlsx

#### Usage

    ProjectConfiguration$save(path)

#### Arguments

- `path`:

  a string representing the path or file name where to save the file.
  Can be absolute or relative (to working directory).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProjectConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
