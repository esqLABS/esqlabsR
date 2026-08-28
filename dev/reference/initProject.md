# Initialize esqlabsR Project Folders and required Files

Creates the default project folder structure with Excel file templates
in the working directory.

## Usage

``` r
initProject(destination = ".", overwrite = FALSE)
```

## Arguments

- destination:

  A string defining the path where to initialize the project. default to
  current working directory.

- overwrite:

  If TRUE, overwrites existing project without asking for permission. If
  FALSE and a project already exists, asks user for permission to
  overwrite.
