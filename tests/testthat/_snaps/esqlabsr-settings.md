# Check that values for package environment bindings are correct

    Code
      getEsqlabsRSetting("xyz")
    Condition
      Error in `getEsqlabsRSetting()`:
      ! No global setting with the name "xyz" exists. Available global settings are: packageVersion, packageName, and colorPalette

