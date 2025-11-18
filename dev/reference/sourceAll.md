# Source all .R files located in a specific folder

Source all .R files located in a specific folder

## Usage

``` r
sourceAll(folderPath, recursive = FALSE)
```

## Arguments

- folderPath:

  Path to the folder where .R files are located

- recursive:

  If `TRUE`, the contents of the sub-folders are also sourced, otherwise
  only the files located directly in the directory are considered.
  Default is `FALSE`.
