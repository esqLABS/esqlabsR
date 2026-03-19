# Validate all configuration files in a project

Validate all configuration files in a project

## Usage

``` r
validateAllConfigurations(projectConfiguration, ignoreVersionCheck = TRUE)
```

## Arguments

- projectConfiguration:

  ProjectConfiguration object or path to ProjectConfiguration.xlsx

- ignoreVersionCheck:

  Logical; if TRUE, skip project configuration version mismatch checks
  when creating a ProjectConfiguration from a path. Defaults to TRUE

## Value

Named list of validationResult objects
