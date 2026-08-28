# Check if a directory contains an esqlabsR project

Checks if a directory already contains an esqlabsR project by looking
for the presence of ProjectConfiguration.xlsx file or Configurations
folder.

## Usage

``` r
isProjectInitialized(destination = ".")
```

## Arguments

- destination:

  A string defining the path to check for an existing project. Defaults
  to current working directory.

## Value

TRUE if an esqlabsR project exists in the directory, FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check if current directory has a project
hasProject <- isProjectInitialized()

# Check if specific directory has a project
hasProject <- isProjectInitialized("path/to/project")
} # }
```
