# Generator for the legacy Excel test fixture
# =============================================================================
#
# Rebuilds `tests/testthat/data/TestProjectExcelLegacy/`, a plausible pre-5.6
# esqlabsR project used to characterize how the 6.0.0 importer handles the
# workbook shapes real legacy projects carry (issue #1213). Run it from the
# package root:
#
#   Rscript data-raw/TestProjectExcelLegacy.R
#
# The workbooks it writes are committed, matching how the other Excel fixture is
# kept; this script exists so they can be re-derived and reviewed as text rather
# than edited as opaque binaries.
#
# What makes this fixture *legacy*, and why each trait is here (the sibling
# `TestProjectExcel/` fixture has the modern spelling of every one of them, which
# is why it cannot reproduce any of these defects):
#
#   - Individuals and populations declare ontogenies through the two-column
#     `Protein` + `Ontogeny` pair, each cell a comma list zipped pairwise with
#     the other. That is the pre-5.6 layout, removed in esqlabsR #827, and the
#     shape every workbook older than 5.6 still has.
#   - The scenarios sheet has no `OverwriteFormulasInSS` column, and one scenario
#     leaves `SimulationTimeUnit` blank.
#   - Multi-value cells are quoted in the 5.x style, including one value that
#     itself contains a comma, and `DataCombinedName` is quoted on both the
#     defining and the referencing side.
#   - `nsd` is stored as text, as a hand-maintained workbook routinely is.
#   - The populations CSV folder sits under the configurations folder, and its
#     file keeps mixed case.
#   - The parameter-identification workbook uses the 5.x sheet layout, keeps its
#     `Group` and `ObservedDataSheet` columns, and carries a `Units` cell v5
#     ignored.
#
# `Models/Simulations/` holds only a placeholder: the 7 MB `Aciclovir.pkml` is
# not duplicated into this fixture. `localLegacyExcelProject()` injects it from
# `TestProjectExcel/` into each throwaway copy, so the fixture is a runnable
# project when a test uses it while costing the repository nothing.

library(writexl)

fixtureDir <- file.path("tests", "testthat", "data", "TestProjectExcelLegacy")
siblingDir <- file.path("tests", "testthat", "data", "TestProjectExcel")

configDir <- file.path(fixtureDir, "Configurations")
csvDir <- file.path(configDir, "PopulationsCSV")
dataDir <- file.path(fixtureDir, "Data")
modelDir <- file.path(fixtureDir, "Models", "Simulations")

for (d in c(
  configDir,
  csvDir,
  dataDir,
  modelDir,
  file.path(fixtureDir, "Results", "Figures"),
  file.path(fixtureDir, "Results", "SimulationResults")
)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# The OSPS paths and the observed data set name are the sibling fixture's, so
# both fixtures describe the same public Aciclovir model and the same bundled
# observed-data workbook.
pvbPath <-
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
fatPath <- "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
tsPath <- "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir"
dataSet <- paste0(
  "Laskin 1982.Group A_Aciclovir_1_Human_MALE_",
  "PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
)

# ProjectConfiguration.xlsx ----
# `modelFolder` (not the 6.0.0 `simulationsFolder`) and a `populationsFolder`
# under the configurations folder are both legacy spellings.
write_xlsx(
  list(
    Sheet1 = data.frame(
      Property = c(
        "modelFolder",
        "configurationsFolder",
        "modelParamsFile",
        "individualsFile",
        "populationsFile",
        "populationsFolder",
        "scenariosFile",
        "applicationsFile",
        "plotsFile",
        "parameterIdentificationFile",
        "dataFolder",
        "dataFile",
        "dataImporterConfigurationFile",
        "outputFolder",
        "esqlabsRVersion"
      ),
      Value = c(
        "Models/Simulations/",
        "Configurations/",
        "ModelParameters.xlsx",
        "Individuals.xlsx",
        "Populations.xlsx",
        "PopulationsCSV",
        "Scenarios.xlsx",
        "Applications.xlsx",
        "Plots.xlsx",
        "ParameterIdentification.xlsx",
        "Data/",
        "TestProject_TimeValuesData.xlsx",
        "esqlabs_dataImporter_configuration.xml",
        "Results/",
        "5.5.0"
      ),
      Description = "",
      stringsAsFactors = FALSE
    )
  ),
  file.path(fixtureDir, "ProjectConfiguration.xlsx")
)

# Individuals.xlsx ----
# `Adult` declares one ontogeny and `Child` two, through the legacy column pair.
# Two ontogenies matter on their own: a repaired importer would hand a length-2
# value to `.readOntogeniesFromList()`, which cannot take one.
#
# Both individuals also have a same-named parameter sheet, which is how the
# importer links an individual to its own parametrization. `Individual Parameter
# Sets` is the legacy spelling of that column, which the 6.0.0 importer does not
# read (it looks for `ParameterSets`), so the linkage here rests on the sheet
# names alone, exactly as it does on a real legacy workbook.
gfrSheet <- function(value) {
  data.frame(
    `Container Path` = "Organism|Kidney",
    `Parameter Name` = "GFR",
    Value = value,
    Units = "ml/min",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
write_xlsx(
  list(
    IndividualBiometrics = data.frame(
      IndividualId = c("Adult", "Child"),
      Species = "Human",
      Population = "European_ICRP_2002",
      Gender = c("MALE", "FEMALE"),
      `Weight [kg]` = c(73, 20),
      `Height [cm]` = c(176, 115),
      `Age [year(s)]` = c(30, 6),
      Protein = c("CYP3A4", "CYP3A4,CYP2D6"),
      Ontogeny = c("CYP3A4", "CYP3A4,CYP2C8"),
      `Individual Parameter Sets` = c("Adult", "Child"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    Adult = gfrSheet(90),
    Child = gfrSheet(60)
  ),
  file.path(configDir, "Individuals.xlsx")
)

# Populations.xlsx ----
# `AdultPop` declares two ontogenies through the legacy column pair. `CsvPop` is
# the population a scenario reads from CSV, so it declares none: its individuals
# come from the file, not from these demographics.
write_xlsx(
  list(
    Demographics = data.frame(
      PopulationName = c("AdultPop", "CsvPop"),
      species = "Human",
      population = "European_ICRP_2002",
      numberOfIndividuals = c(2, 2),
      proportionOfFemales = c(0, 0),
      weightMin = NA_real_,
      weightMax = NA_real_,
      weightUnit = "kg",
      heightMin = NA_real_,
      heightMax = NA_real_,
      heightUnit = "cm",
      ageMin = c(22, 22),
      ageMax = c(41, 41),
      BMIMin = NA_real_,
      BMIMax = NA_real_,
      BMIUnit = "kg/m²",
      Protein = c("CYP3A4,CYP2D6", NA),
      Ontogeny = c("CYP3A4,CYP2C8", NA),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    # A legacy workbook carries this sheet whether or not it is used.
    UserDefinedVariability = data.frame(
      `Container Path` = character(),
      `Parameter Name` = character(),
      Mean = numeric(),
      SD = numeric(),
      Distribution = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  ),
  file.path(configDir, "Populations.xlsx")
)

# ModelParameters.xlsx ----
# `Sheet, with comma` is what makes the quoted multi-value cell in Scenarios.xlsx
# necessary rather than decorative: without the quotes its comma would split it
# into two references.
write_xlsx(
  list(
    Global = data.frame(
      `Container Path` = "Organism|Liver",
      `Parameter Name` = "EHC continuous fraction",
      Value = 1,
      Units = NA_character_,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    Aciclovir = data.frame(
      `Container Path` = "Aciclovir",
      `Parameter Name` = "Lipophilicity",
      Value = -0.1,
      Units = "Log Units",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    `Sheet, with comma` = data.frame(
      `Container Path` = "Organism|Liver",
      `Parameter Name` = "EHC continuous fraction",
      Value = 1,
      Units = NA_character_,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  ),
  file.path(configDir, "ModelParameters.xlsx")
)

# Applications.xlsx ----
# The 5.x layout: no `ApplicationProtocols` sheet, one parameter-shaped sheet per
# protocol, so each sheet becomes both a parameter set and an application.
write_xlsx(
  list(
    IV_250mg = data.frame(
      `Container Path` = "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem",
      `Parameter Name` = "Dose",
      Value = 250,
      Units = "mg",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  ),
  file.path(configDir, "Applications.xlsx")
)

# Scenarios.xlsx ----
# No `OverwriteFormulasInSS` column at all, and `AdultScenario` leaves
# `SimulationTimeUnit` blank. `AdultScenario` also carries the quoted
# multi-value `ModelParameterSheets` cell.
write_xlsx(
  list(
    OutputPaths = data.frame(
      OutputPathId = c("Aciclovir_PVB", "Aciclovir_Fat"),
      OutputPath = c(pvbPath, fatPath),
      stringsAsFactors = FALSE
    ),
    Scenarios = data.frame(
      Scenario_name = c(
        "AdultScenario",
        "ChildScenario",
        "AdultPopScenario",
        "CsvPopScenario",
        "PIScenario",
        "PIScenario2"
      ),
      IndividualId = c("Adult", "Child", "Adult", "Adult", NA, NA),
      PopulationId = c(NA, NA, "AdultPop", "CsvPop", NA, NA),
      ReadPopulationFromCSV = c(NA, NA, FALSE, TRUE, NA, NA),
      ModelParameterSheets = c(
        "\"Global\", \"Aciclovir\", \"Sheet, with comma\"",
        "Global",
        "Global",
        "Global",
        "Aciclovir",
        "Aciclovir"
      ),
      ApplicationProtocol = "IV_250mg",
      SimulationTime = c(
        "0, 24, 60",
        "0, 24, 60",
        "0, 12, 20",
        "0, 12, 20",
        "0, 120, 1",
        "0, 120, 1"
      ),
      SimulationTimeUnit = c(NA, "h", "h", "h", "h", "h"),
      SteadyState = c(NA, NA, FALSE, FALSE, FALSE, FALSE),
      SteadyStateTime = NA_real_,
      SteadyStateTimeUnit = NA_character_,
      ModelFile = "Aciclovir.pkml",
      OutputPathsIds = c(
        "Aciclovir_PVB",
        "Aciclovir_PVB, Aciclovir_Fat",
        NA,
        NA,
        "Aciclovir_PVB",
        "Aciclovir_PVB"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  ),
  file.path(configDir, "Scenarios.xlsx")
)

# Plots.xlsx ----
# `DataCombinedName` is quoted on both the defining (`DataCombined`) and the
# referencing (`plotConfiguration`) side, which is the consistently quoted 5.x
# workbook that still validates. `nsd` is written as text.
write_xlsx(
  list(
    DataCombined = data.frame(
      DataCombinedName = c(
        "\"AciclovirPVB\"",
        "\"AciclovirPVB\"",
        "\"AciclovirPop\""
      ),
      dataType = c("simulated", "observed", "simulated"),
      label = c("Aciclovir simulated", "Aciclovir observed", "Population"),
      scenario = c("AdultScenario", NA, "AdultPopScenario"),
      path = c(pvbPath, NA, pvbPath),
      dataSet = c(NA, dataSet, NA),
      group = c("Aciclovir PVB", "Aciclovir PVB", "Aciclovir Pop"),
      stringsAsFactors = FALSE
    ),
    plotConfiguration = data.frame(
      plotID = c("P1", "P2", "P3"),
      DataCombinedName = c(
        "\"AciclovirPVB\"",
        "\"AciclovirPVB\"",
        "\"AciclovirPop\""
      ),
      plotType = c("individual", "observedVsSimulated", "population"),
      title = c("PlotTitle", NA, NA),
      xUnit = c("h", NA, "h"),
      xAxisScale = NA_character_,
      yAxisScale = NA_character_,
      xValuesLimits = c("0, 24", NA, "0, 24"),
      yValuesLimits = NA_character_,
      aggregation = c(NA, NA, "arithmetic"),
      quantiles = NA_character_,
      # Text, not a number: a hand-maintained workbook stores it this way, and
      # the importer copies whatever type it reads.
      nsd = c(NA, NA, "1.96"),
      foldDistance = c(NA, "2, 3", NA),
      subtitle = c("PlotSubtitle", NA, NA),
      stringsAsFactors = FALSE
    ),
    plotGrids = data.frame(
      name = c("Aciclovir", "AciclovirPop"),
      plotIDs = c("P1, P2", "P3"),
      title = c("GridTitle", NA),
      subtitle = c("GridSubtitle", NA),
      stringsAsFactors = FALSE
    ),
    # Legacy sheets a workbook carries whether or not they hold anything. The
    # importer reads none of them.
    exportConfiguration = data.frame(
      plotGridName = character(),
      outputName = character(),
      width = numeric(),
      stringsAsFactors = FALSE
    ),
    dataTypes = data.frame(
      dataType = c("observed", "simulated"),
      stringsAsFactors = FALSE
    ),
    plotTypes = data.frame(
      plotType = c(
        "individual",
        "population",
        "observedVsSimulated",
        "residualsVsSimulated",
        "residualsVsTime"
      ),
      stringsAsFactors = FALSE
    )
  ),
  file.path(configDir, "Plots.xlsx")
)

# ParameterIdentification.xlsx ----
# The 5.x layout (no `PITasks` sheet). Two `PIParameters` rows share `Group` 2
# and differ only in the scenario they name: v5 merged those by
# `(Group, Container Path, Parameter Name)` into one free parameter estimated
# across both scenarios. Their `Units` cell is the kind v5 ignored. The
# `ObservedDataSheet` column is the legacy one, and `DataSet` names an observed
# data set the project does not define.
write_xlsx(
  list(
    PIOutputMappings = data.frame(
      PITaskName = "AciclovirFit",
      Scenarios = c("PIScenario", "PIScenario2"),
      OutputPath = pvbPath,
      ObservedDataSheet = "Laskin 1982.Group A",
      DataSet = dataSet,
      Scaling = "lin",
      xOffset = NA_real_,
      yOffset = NA_real_,
      xFactor = NA_real_,
      yFactor = NA_real_,
      Weight = NA_real_,
      stringsAsFactors = FALSE
    ),
    PIParameters = data.frame(
      PITaskName = "AciclovirFit",
      Scenarios = c("PIScenario", "PIScenario", "PIScenario2"),
      `Container Path` = c("Aciclovir", tsPath, tsPath),
      `Parameter Name` = c("Lipophilicity", "TSspec", "TSspec"),
      Units = c("Log Units", "mg", "mg"),
      MinValue = c(-10, 0, 0),
      MaxValue = c(10, 10, 10),
      StartValue = c(1, 0.5, 0.5),
      Group = c(1, 2, 2),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    PIConfiguration = data.frame(
      PITaskName = "AciclovirFit",
      Algorithm = "BOBYQA",
      CIMethod = "hessian",
      PrintEvaluationFeedback = TRUE,
      AutoEstimateCI = FALSE,
      stringsAsFactors = FALSE
    ),
    AlgorithmOptions = data.frame(
      PITaskName = "AciclovirFit",
      OptionName = c("maxeval", "ftol_rel"),
      OptionValue = c(100, 0),
      stringsAsFactors = FALSE
    ),
    CIOptions = data.frame(
      PITaskName = "AciclovirFit",
      OptionName = "confLevel",
      OptionValue = 0.95,
      stringsAsFactors = FALSE
    )
  ),
  file.path(configDir, "ParameterIdentification.xlsx")
)

# Copied assets and placeholders ----
# The population CSV keeps mixed case, so it differs from the lowercase name the
# runner derives from the canonical population id.
file.copy(
  file.path(
    siblingDir,
    "Configurations",
    "PopulationsCSV",
    "TestPopulation.csv"
  ),
  file.path(csvDir, "CsvPop.csv"),
  overwrite = TRUE
)
for (f in c(
  "TestProject_TimeValuesData.xlsx",
  "esqlabs_dataImporter_configuration.xml"
)) {
  file.copy(
    file.path(siblingDir, "Data", f),
    file.path(dataDir, f),
    overwrite = TRUE
  )
}

# `Models/Simulations/` deliberately holds no .pkml; see the header.
writeLines(
  c(
    "The simulation this project references (`Aciclovir.pkml`) is not committed",
    "here. It is the same 7 MB public model the `TestProjectExcel/` fixture",
    "carries, and `localLegacyExcelProject()` copies it in when a test makes a",
    "throwaway copy of this project, so the fixture is runnable in a test",
    "without a second copy of the binary in the repository."
  ),
  file.path(modelDir, "README.md")
)
for (d in c("Figures", "SimulationResults")) {
  writeLines(
    "Output folder, kept so the empty directory survives version control.",
    file.path(fixtureDir, "Results", d, "README.md")
  )
}

message("Wrote ", fixtureDir)
