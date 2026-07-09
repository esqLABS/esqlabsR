# addIndividual aborts when individualId already exists

    Code
      addIndividual(project, "indiv1", species = "Human", gender = "MALE")
    Condition
      Error in `addIndividual()`:
      ! individual "indiv1" already exists

# addIndividual aborts when gender is missing

    Code
      addIndividual(project, "newi", species = "Human")
    Condition
      Error in `addIndividual()`:
      ! Cannot add individual "newi":
      x gender must be a non-empty string

# addIndividual aborts when gender is not a valid GenderInt token

    Code
      addIndividual(project, "newi", species = "Human", gender = "banana")
    Condition
      Error in `addIndividual()`:
      ! Cannot add individual "newi":
      x gender must be one of MALE, FEMALE, UNKNOWN

# setIndividual aborts on a non-existent individual

    Code
      setIndividual(project, "Ghost", weight = 80)
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `setIndividual()`:
      ! Cannot modify individual "ghost": it does not exist.
      i Use `addIndividual()` to create it first.

# setIndividual rejects an empty gender like addIndividual

    Code
      setIndividual(project, "indiv1", gender = "")
    Condition
      Error in `setIndividual()`:
      ! `gender` must be a non-empty string

# setIndividual rejects a gender that is not a valid GenderInt token

    Code
      setIndividual(project, "indiv1", gender = "banana")
    Condition
      Error in `setIndividual()`:
      ! `gender` must be one of "MALE", "FEMALE", and "UNKNOWN"

# setIndividual rejects parameterSets that do not resolve

    Code
      setIndividual(project, "indiv1", parameterSets = "Ghost")
    Condition
      Warning:
      Canonicalized 1 referenced id to a safe form:
      * "Ghost" -> "ghost"
      Error in `setIndividual()`:
      ! `parameterSets` references undefined parameter sets:
      x "ghost"

# setIndividual aborts on an undefined parameter set

    Code
      setIndividual(project, "indiv1", parameterSets = "Ghost")
    Condition
      Warning:
      Canonicalized 1 referenced id to a safe form:
      * "Ghost" -> "ghost"
      Error in `setIndividual()`:
      ! `parameterSets` references undefined parameter sets:
      x "ghost"

# addIndividual aborts on a mismatched scalar field length

    Code
      addIndividual(project, c("a", "b", "c"), species = "Human", gender = c("MALE",
        "FEMALE"))
    Condition
      Error in `addIndividual()`:
      ! `gender` must be length 1 or length 3 (the number of ids).
      x It is length 2.

# addIndividual aborts on a duplicate id in the batch

    Code
      addIndividual(project, c("a", "a"), species = "Human", gender = "MALE")
    Condition
      Error in `addIndividual()`:
      ! duplicate individual id in the batch: "a"

# print.Individual renders the configured fields

    Code
      print(project$individuals[["indiv1"]])
    Output
      <Individual>
        * Species: Human
        * Population: European_ICRP_2002
        * Gender: MALE
        * Weight: 73
        * Height: 176
        * Age: 30
        * Parameter Sets: indiv1_default

# print.Individual renders a minimal individual

    Code
      print(project$individuals[["minimal"]])
    Output
      <Individual>
        * Species: Human
        * Population: <empty string>
        * Gender: MALE
        * Weight: <empty string>
        * Height: <empty string>
        * Age: <empty string>
        * Parameter Sets: <empty string>

