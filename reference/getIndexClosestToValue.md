# Find value in an array

Find the index of the value in an array that is closest to given one. By
default, no restriction is applied how big the absolute numerical
distance between `value` and a value in the `array` may be. A limit can
be set by the parameters `thresholdAbs` or `thresholdRel`. If no value
within the `array` has the distance to `value` that is equal to or less
than the threshold, the `value` is considered not present in the `array`
and `NULL` is returned.

## Usage

``` r
getIndexClosestToValue(value, array, thresholdAbs = NULL, thresholdRel = NULL)
```

## Arguments

- value:

  Numerical value

- array:

  Numerical array

- thresholdAbs:

  Absolute numerical distance by which the closest value in `array` may
  differ from `value` to be accepted. If both `thresholdAbs` and
  `thresholdRel` are `NULL` (default), no threshold is applied. If
  `thresholdAbs` is set, `thresholdRel` is ignored. If 0, only exact
  match between `value` and `array` is accepted.

- thresholdRel:

  A fraction by which the closest value may differ from `value` to be
  accepted. **WARNING**: setting a relative threshold will result in
  only exact matches if `value` is 0!

## Value

Index of a value within the array which is closest to `value` and the
difference is within the defined threshold. If multiple entries of
`array` have the same difference which is minimal, a vector of indices
is returned. If no value is within the defined threshold, `NULL` is
returned.
