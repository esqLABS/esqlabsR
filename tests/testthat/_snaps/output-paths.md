# addOutputPath aborts on a duplicate id

    Code
      addOutputPath(project, existing, "Organism|other|Concentration in container")
    Condition
      Error in `addOutputPath()`:
      ! outputPath "aciclovir_fat_cell" already exists

# setOutputPath aborts on a non-existent id

    Code
      setOutputPath(project, "Ghost", "Organism|A|Concentration in container")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `setOutputPath()`:
      ! Cannot modify output path "ghost": it does not exist.
      i Use `addOutputPath()` to create it first.

# setOutputPath rejects an empty path

    Code
      setOutputPath(project, id, "")
    Condition
      Error in `setOutputPath()`:
      ! `path` must contain non-empty strings

# addOutputPath aborts on a path length that is neither 1 nor N

    Code
      addOutputPath(project, c("a", "b", "c"), c("X", "Y"))
    Condition
      Error in `addOutputPath()`:
      ! Cannot add outputPath:
      x path must be a character vector of length 1 or the same length as id

