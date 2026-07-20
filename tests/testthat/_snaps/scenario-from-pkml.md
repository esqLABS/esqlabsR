# createScenariosFromPKML adds scenarios in place and returns the project invisibly

    Code
      result <- createScenariosFromPKML(pkmlFixture, project = project, scenarios = "seeded")
    Message
      i Added 1 scenario: "seeded"

# user alias ignored in favour of registered id emits an inform

    Code
      createScenariosFromPKML(pkmlFixture, project = project, scenarios = "seeded",
        outputPaths = stats::setNames(existingPath, "myAlias"))
    Message
      i Output path alias "myAlias" ignored: path "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" is already registered as "aciclovir_pvb".
      i Added 1 scenario: "seeded"

# named outputPaths colliding with an existing id mapped to a different path abort and leave the project unchanged

    Code
      createScenariosFromPKML(pkmlFixture, project = project, scenarios = "seeded",
        outputPaths = c(aciclovir_pvb = "Organism|Some|Other|Path"))
    Condition
      Error in `.resolveScenarioOutputPaths()`:
      ! x Output path id "aciclovir_pvb" already maps to a different path. i Existing: "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" i Requested: "Organism|Some|Other|Path"

# unknown parameterSets abort and leave the project unchanged

    Code
      createScenariosFromPKML(pkmlFixture, project = project, scenarios = "seeded",
        parameterSets = "DoesNotExist")
    Condition
      Warning:
      Canonicalized 1 referenced id to a safe form:
      * "DoesNotExist" -> "doesnotexist"
      Error in `project$createScenariosFromPKML()`:
      ! Cannot add scenario "seeded":
      x parameterSets not found in project$definitions$parameterSets: doesnotexist

# an unknown application aborts

    Code
      createScenariosFromPKML(pkmlFixture, project = project, scenarios = "seeded",
        application = "NoSuchProtocol")
    Condition
      Warning:
      Canonicalized 1 referenced id to a safe form:
      * "NoSuchProtocol" -> "nosuchprotocol"
      Error in `project$createScenariosFromPKML()`:
      ! Cannot add scenario "seeded":
      x application 'nosuchprotocol' not found in applications

# duplicate scenario names are expanded with numeric suffixes

    Code
      createScenariosFromPKML(c(pkmlFixture, pkmlFixture), project = project,
      scenarios = c("s", "s"))
    Condition
      Warning:
      Duplicate scenario names found and made unique by adding indices: i Duplicated names: "s", renamed to "s_2"
    Message
      i Added 2 scenarios: "s" and "s_2"

# NULL modelFolder falls back to the absolute pkml path with a warning

    Code
      suppressMessages(createScenariosFromPKML(pkmlFixture, project = project,
        scenarios = "seeded"))
    Condition
      Warning:
      ! The project has no modelFolder; storing an absolute model file path. i Set a modelFolder on the project so the scenario stores a portable relative path ('data/TestProject/Models/Simulations/Aciclovir.pkml').

# inconsistent vector argument lengths abort

    Code
      createScenariosFromPKML(rep(pkmlFixture, 2), project = project, scenarios = c(
        "A", "B", "C"))
    Condition
      Error in `.getScenarioCount()`:
      ! Inconsistent vector argument lengths: x All vector arguments with length > 1 must have the same length i Found lengths: "2, 3"

