# createScenariosFromPKML adds scenarios in place, marks the project modified, and returns it invisibly

    Code
      result <- createScenariosFromPKML(pkmlFixture, project = project,
        scenarioNames = "Seeded")
    Message
      i Added 1 scenario: "Seeded"

# user alias ignored in favour of registered id emits an inform

    Code
      createScenariosFromPKML(pkmlFixture, project = project, scenarioNames = "Seeded",
        outputPaths = stats::setNames(existingPath, "myAlias"))
    Message
      i Output path alias "myAlias" ignored: path "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" is already registered as "Aciclovir_PVB".
      i Added 1 scenario: "Seeded"

# named outputPaths colliding with an existing id mapped to a different path abort and leave the project unchanged

    Code
      createScenariosFromPKML(pkmlFixture, project = project, scenarioNames = "Seeded",
        outputPaths = c(Aciclovir_PVB = "Organism|Some|Other|Path"))
    Condition
      Error in `.resolveScenarioOutputPaths()`:
      ! x Output path id "Aciclovir_PVB" already maps to a different path. i Existing: "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" i Requested: "Organism|Some|Other|Path"

# unknown modelParameterSets abort and leave the project unchanged

    Code
      createScenariosFromPKML(pkmlFixture, project = project, scenarioNames = "Seeded",
        modelParameterSets = "DoesNotExist")
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "Seeded":
      x modelParameterSets not found in project$modelParameterSets: DoesNotExist

# unknown applicationProtocols abort

    Code
      createScenariosFromPKML(pkmlFixture, project = project, scenarioNames = "Seeded",
        applicationProtocols = "NoSuchProtocol")
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "Seeded":
      x applicationProtocol 'NoSuchProtocol' not found in applications

# duplicate scenario names are expanded with numeric suffixes

    Code
      createScenariosFromPKML(c(pkmlFixture, pkmlFixture), project = project,
      scenarioNames = c("S", "S"))
    Condition
      Warning:
      Duplicate scenario names found and made unique by adding indices: i Duplicated names: "S", renamed to "S_2"
    Message
      i Added 2 scenarios: "S" and "S_2"

# NULL modelFolder falls back to the absolute pkml path with a warning

    Code
      suppressMessages(createScenariosFromPKML(pkmlFixture, project = project,
        scenarioNames = "Seeded"))
    Condition
      Warning:
      ! The project has no modelFolder; storing an absolute model file path. i Set a modelFolder on the project so the scenario stores a portable relative path ('data/TestProject/Models/Simulations/Aciclovir.pkml').

# inconsistent vector argument lengths abort

    Code
      createScenariosFromPKML(rep(pkmlFixture, 2), project = project, scenarioNames = c(
        "A", "B", "C"))
    Condition
      Error in `.getScenarioCount()`:
      ! Inconsistent vector argument lengths: x All vector arguments with length > 1 must have the same length i Found lengths: "2, 3"

