# Convert Windows filepaths for R

Converts the Windows-like path (using `\`) from the clipboard to the
form readable by R (using` /`).

## Usage

``` r
pathFromClipboard(path = "clipboard")
```

## Arguments

- path:

  Path that will be converted. If `"clipboard"` (default), path is
  queried from clipboard.

## Value

String representation of a file path with `/` as separator.
