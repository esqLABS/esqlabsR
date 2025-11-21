# Read individual characteristics from file

Read individual characteristics from file

## Usage

``` r
readIndividualCharacteristicsFromXLS(
  XLSpath,
  individualId,
  sheet = "IndividualBiometrics",
  nullIfNotFound = TRUE
)
```

## Arguments

- XLSpath:

  Full path to the excel file

- individualId:

  (String) Id of the individual as stored in the `IndividualId` column.

- sheet:

  Name of the sheet. If `NULL` (default), the first sheet of the file is
  used.

- nullIfNotFound:

  Boolean. If `TRUE` (default), `NULL` is returned if no entry with the
  give `individualId` exists. Otherwise, an error is thrown.

## Value

An `IndividualCharacteristics` object

## Details

Read individual characteristics from an excel sheet and create an
`IndividualCharacteristics`-object. The excel sheet must have the
columns `IndividualId`, `Species`, `Population`, `Gender`,
`Weight [kg]`, `Height [cm]`, `Age [year(s)]`, and `Protein Ontogenies`.
