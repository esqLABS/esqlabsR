# importProjectFromExcel aborts when two ids canonicalize to the same value

    Code
      importProjectFromExcel(file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(), silent = TRUE)
    Condition
      Error in `.canonicalizeId()`:
      ! Ids collide after canonicalization:
      x "Aciclovir_PVB" and "aciclovir_pvb" -> "aciclovir_pvb"
      i Two distinct ids that canonicalize to the same id are ambiguous; rename so they differ by more than case or forbidden characters.

