# Sanitize a string to be a valid Excel sheet name

Sanitize a string to be a valid Excel sheet name

## Usage

``` r
.sanitizeExcelSheetName(sheetName, warn = TRUE)
```

## Arguments

- sheetName:

  Character string. The proposed sheet name to sanitize.

- warn:

  Logical. Whether to warn if the sheet name was modified. Default is
  TRUE.

## Value

A valid Excel sheet name.

## Details

Sanitizes a string to comply with Excel sheet naming rules:

- Must be 31 characters or less

- Cannot contain any of these characters: / \\ \* \[ \] : ?

- Cannot be empty

- Leading/trailing spaces are trimmed If the name becomes empty after
  sanitization, "Sheet" is used as default. If the name is too long,
  it's truncated and a suffix may be added to ensure uniqueness. If
  warn=TRUE and the name was modified, a warning is issued.

\[ \]: R:%20
