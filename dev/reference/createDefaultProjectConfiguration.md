# Create a default `ProjectConfiguration`

Create a `ProjectConfiguration` based on the
`"ProjectConfiguration.xlsx"`

## Usage

``` r
createDefaultProjectConfiguration(
  path = file.path("ProjectConfiguration.xlsx"),
  ignoreVersionCheck = FALSE
)
```

## Arguments

- path:

  path to the `ProjectConfiguration.xlsx` file. Defaults to the
  `ProjectConfiguration.xlsx` file in the working directory.

- ignoreVersionCheck:

  If `TRUE`, skip the version mismatch check when loading the
  configuration file. Use this in non-interactive contexts such as
  automated tests or scripts running from console where interactive user
  input cannot be assured. Defaults to `FALSE`.

## Value

Object of type `ProjectConfiguration`
