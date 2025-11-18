# Read XLSX files using `readxl::read_excel` with suppressed warnings

Read XLSX files using
[`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
with suppressed warnings

## Usage

``` r
readExcel(path, sheet = NULL, ...)
```

## Arguments

- path:

  Full path of an XLS/XLSX file

- sheet:

  Name or number of the sheet. If `NULL` (default), the first sheet of
  the file is used.

- ...:

  Any other parameters that can be passed to
  [`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)

## Value

A tibble with the contents of the excel sheet
