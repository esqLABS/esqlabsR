# Add a new key-value pairs to an enum, where the value is a list.

Add a new key-value pairs to an enum, where the value is a list.

## Usage

``` r
enumPutList(key, values, enum, overwrite = FALSE)
```

## Arguments

- key:

  Key to be added

- values:

  Values to be added

- enum:

  enum the key-value pairs should be added to. **WARNING**: the original
  object is not modified!

- overwrite:

  if TRUE and a `key` exists, it will be overwritten with the new value.
  Otherwise, an error is thrown. Default is `FALSE`.

## Value

Enum with added key-value pair.

## Examples

``` r
library(ospsuite.utils)
myEnum <- enum(c(a = "b"))
myEnum <- enumPut("c", "d", myEnum)
myEnum <- enumPut(c("c", "d", "g"), list(12, 2, "a"), myEnum, overwrite = TRUE)
myEnum <- enumPutList("g", list(12, 2, "a"), myEnum, overwrite = TRUE)
```
