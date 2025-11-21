# Calculate mean and standard deviation for the yValues of the given `DataSet` objects

Calculate mean and standard deviation for the yValues of the given
`DataSet` objects

## Usage

``` r
calculateMeanDataSet(
  dataSets,
  method = "arithmetic",
  lloqMode = LLOQMode$`LLOQ/2`,
  outputXunit = NULL,
  outputYunit = NULL,
  outputMolWeight = NULL
)
```

## Arguments

- dataSets:

  list of `DataSet` objects

- method:

  method for calculating the mean and standard deviation - either
  `arithmetic` (default) or `geometric`

- lloqMode:

  how to treat data points below LLOQ if LLOQ is given - `LLOQ/2`
  (default): use as given (since `DataSet` stores values below LLOQ as
  `LLOQ/2`), `LLOQ`: set value to LLOQ value, `ZERO`: set value to 0,
  `ignore`: do not use data points for mean calculation

- outputXunit:

  xUnit of output data set, if `NULL` (default) xUnit of the first data
  set will be used

- outputYunit:

  yUnit of output data set, if `NULL` (default) yUnit of the first data
  set will be used

- outputMolWeight:

  molWeight of output data set in `g/mol` - obligatory when initial data
  sets have differing molWeight values

## Value

A single `DataSet` object

## Details

Calculates mean and standard deviation of the yValues of the given
`DataSet` objects per xValue. The meta data of the returned `DataSet`
consists of all meta data that are equal in all initial data sets. Its
LLOQ is the mean LLOQ value of all data sets which have an LLOQ set,
e.g. if dataSet1 has LLOQ 1, dataSet2 has LLOQ 3 and dataSet3 has no
LLOQ, then 2 is used for the returned `DataSet`. The LLOQ of the
returned `DataSet` is the arithmetic mean of LLOQ values of all
`DataSet`s
