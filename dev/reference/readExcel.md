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

## Details

Rows that are blank in every column are dropped. Stray cell formatting
extends a sheet's used range past its last real row, so a workbook
edited over time routinely reports trailing rows that hold nothing;
`readxl` returns them as all-`NA` records. They carry no information a
project could use, and a parser that takes each row for a definition
would abort on the first of them for having no id. Dropping them at the
one place every sheet is read keeps every parser out of the business of
recognizing them.
