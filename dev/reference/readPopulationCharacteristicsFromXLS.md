# Read an excel file containing information about population and create a `PopulationCharacteristics` object

Read an excel file containing information about population and create a
`PopulationCharacteristics` object

## Usage

``` r
readPopulationCharacteristicsFromXLS(XLSpath, populationName, sheet = NULL)
```

## Arguments

- XLSpath:

  Path to the excel file

- populationName:

  Name of the population, as defined in the "PopulationName" column

- sheet:

  Name or the index of the sheet in the excel file. If `NULL`, the first
  sheet in the file is used.

## Value

A `PopulationCharacteristics` object based on the information in the
excel file.
