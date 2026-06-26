# jsonPath is read-only and aliases projectFilePath

    Code
      project$jsonPath <- "elsewhere.json"
    Condition
      Error:
      ! jsonPath is readonly

