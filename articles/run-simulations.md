# 3. Run Simulations

Running scenarios can be divided into three steps:

1.  Creating the `ScenarioConfiguration` objects,
2.  Creating the `Scenario` objects from configurations,
3.  Running the scenarios.

### Creating `ScenarioConfiguration` objects

A `ScenarioConfiguration` object holds all information about a scenario
that is defined in the Excel files. Though it is possible to create an
empty `ScenarioConfiguration` and populate it by hand, we usually want
to create scenario configurations from the Excel files by calling the
[`readScenarioConfigurationFromExcel()`](https://esqlabs.github.io/esqlabsR/reference/readScenarioConfigurationFromExcel.md)
function. A `ScenarioConfiguration` is based on the
`ProjectConfiguration`, which has to be provided as an argument to the
function. To create the configuration for the *‘TestScenario’* scenario
defined in the `Scenarios.xlsx` file, we call:

``` r
# Create `ScenarioConfiguration` objects from excel files
scenarioConfigurations <- readScenarioConfigurationFromExcel(
  scenarioNames = "TestScenario",
  projectConfiguration = projectConfiguration
)
```

    #> <ScenarioConfiguration>
    #> 
    #> ── Scenario configuration ──────────────────────────────────────────────────────
    #>   • Scenario name: TestScenario
    #>   • Model file name: Aciclovir.pkml
    #>   • Application protocol: Aciclovir_iv_250mg
    #>   • Simulation type: Individual
    #>   • Individual Id: Indiv1
    #>   • Population Id: NULL
    #>   • Read population from csv file: FALSE
    #>   • Parameters sheets: Global
    #>   • Simulate steady-state: FALSE
    #>   • Steady-state time: 1000
    #> 
    #> ── Simulation time intervals ──
    #> 
    #> Interval 1:
    #>   • Start: 0
    #>   • End: 24
    #>   • Resolution: 60
    #>   • Simulation time intervals unit: h

Alternatively, we can create configurations for all scenarios defined in
the `Scenarios.xlsx` by calling
`readScenarioConfigurationFromExcel(projectConfiguration = projectConfiguration)`
without specifying the scenarios’ names.

### Creating `Scenario` objects from configurations

Once all scenario configurations are set up, `Scenario` objects can be
created from them. A `Scenario` object contains the fully parametrized
simulation, the `Population` object in case of a population simulation,
the underlying `ScenarioConfiguration` object, and the list of all
user-defined parameters.

During the model development/fitting phase, you might want to test
parameter values before storing them in the `Parameters` Excel files.
You can define the paths of the test parameters, their values, and the
units in the `TestParameters` file and pass them as `customParams`
argument to the
[`createScenarios()`](https://esqlabs.github.io/esqlabsR/reference/createScenarios.md)
function.

``` r
# Create `Scenario` objects from `ScenarioConfiguration` objects
scenarios <- createScenarios(scenarioConfigurations)
```

You can view the final parametrization that is applied to the simulation
by calling the `finalCustomParams` property:

``` r
scenarios$TestScenario$finalCustomParams
#> $paths
#> [1] "Organism|Liver|EHC continuous fraction"                           
#> [2] "Organism|Kidney|GFR"                                              
#> [3] "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
#> 
#> $values
#> [1]   1  90 250
#> 
#> $units
#> [1] ""       "ml/min" "mg"
```

### Running scenarios

Once the `Scenario` objects are created, they can be simulated by
calling the
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/reference/runScenarios.md)
function. The output of this function is a named list, where the names
are scenario names, and the values are the lists of simulations,
`SimulatioResults` produced by running the simulation, the output values
of the `SimulationResults`, and the population if the scenario is a
population simulation.

``` r
simulatedScenariosResults <- runScenarios(
  scenarios = scenarios
)

# Each simulation is stored separately inside the simulatedScenariosResults
names(simulatedScenariosResults)

# Each simulation can be accessed using its name
simulatedScenariosResults$TestScenario$simulation
#> <Simulation>
#>   • Name: TestScenario
#>   • Source file:
#>   /home/runner/work/_temp/Library/esqlabsR/extdata/examples/TestProject/Models/Simulations/Aciclovir.pkml

# Of course, it contains simulated results as dataframe
head(simulatedScenariosResults$TestScenario$outputValues$data)

# It also contains dataframe's metadata
head(simulatedScenariosResults$TestScenario$outputValues$metaData)
#> [1] "TestScenario"
#>   IndividualId Time
#> 1            0    0
#> 2            0    1
#> 3            0    2
#> 4            0    3
#> 5            0    4
#> 6            0    5
#>   Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
#> 1                                                                  0.000000
#> 2                                                                  2.712924
#> 3                                                                  7.830218
#> 4                                                                 13.107173
#> 5                                                                 18.253719
#> 6                                                                 23.234430
#> NULL
```

It is a good idea to store simulation results as `*.csv` along with
simulations as `.*pkml` and, optionally, the applied population as
`*.csv` file and load them for further processing to avoid re-simulating
every time, e.g., a change to a figure showing the simulation results is
required. The convenient function for saving the results of a scenario
run is `saveScenarioResults`. Then, you will be able to restore the
results using `loadScenarioResults`.

Now that the results have been generated, you can proceed to the next
step in the
[`vignette("plot-results")`](https://esqlabs.github.io/esqlabsR/articles/plot-results.md).
