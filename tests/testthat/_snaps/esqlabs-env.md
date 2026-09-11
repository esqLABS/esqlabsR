# getEsqlabsRSetting returns correct settings

    Code
      getEsqlabsRSetting("nonExistentSetting")
    Condition
      Error in `getEsqlabsRSetting()`:
      ! No global setting with the name "nonExistentSetting" exists. Available global settings are: packageVersion, packageName, and colorPalette

