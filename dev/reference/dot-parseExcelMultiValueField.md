# Parse and validate comma-separated Excel field

Parses comma-separated values from Excel and validates using
ospsuite.utils. Provides Excel-specific error context (plotID, field
name) for common issues.

## Usage

``` r
.parseExcelMultiValueField(
  value,
  fieldName,
  plotID = NULL,
  expectedLength = NULL,
  expectedType = "numeric"
)
```

## Arguments

- value:

  Raw value from Excel cell

- fieldName:

  Name of the field for error messages

- plotID:

  Optional plot ID for error context

- expectedLength:

  Expected number of values (NULL for any length)

- expectedType:

  Expected type ("numeric" or "character")

## Value

Parsed and validated vector
