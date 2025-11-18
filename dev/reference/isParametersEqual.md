# Check if two parameters are equal with respect to certain properties.

Check if two parameters are equal with respect to certain properties.

## Usage

``` r
isParametersEqual(
  parameter1,
  parameter2,
  checkFormulaValues = FALSE,
  compareFormulasByValue = FALSE
)
```

## Arguments

- parameter1:

  First parameter to compare

- parameter2:

  Second parameter to compare

- checkFormulaValues:

  If TRUE, values of explicit formulas are always compared. Otherwise,
  the values are only compared if the formulas are overridden
  (isFixedValue == TRUE). FALSE by default.

- compareFormulasByValue:

  If `FALSE`(default), formulas are compared by their types and string.
  If `TRUE`, only values are compared.

## Value

`TRUE` if parameters are considered equal, `FALSE` otherwise

## Details

The parameters are not equal if: The paths of the parameters are not
equal; The types of the formulas differ (types checked: isConstant,
isDistributed, isExplicit, isTable); Constant formulas have different
values; Distributed formulas have different values (not checking for
distribution) Explicit formulas: If formula string are not equal, OR one
of the parameter values is fixed (formula is overridden), OR both
parameter values are fixed and differ, OR checkFormulaValues is TRUE and
the values differ (disregarding of overridden or not) Table formulas: If
the number of points differ, OR any of the points differ, OR one of the
parameter values is fixed (formula is overridden), OR both parameter
values are fixed and differ.
