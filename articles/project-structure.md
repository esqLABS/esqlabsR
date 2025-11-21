# 1. Initialize a Project

## Initialize the Project Structure

Project Structure can be initialized by calling the function
[`initProject()`](https://esqlabs.github.io/esqlabsR/reference/initProject.md).

``` r
library(esqlabsR)

initProject()
```

This will create a set of directories and files that match the expected
project’s structure.

    #> /home/runner/work/_temp/Library/esqlabsR/extdata/examples/TestProject
    #> ├── Configurations
    #> │   ├── Applications.xlsx
    #> │   ├── Individuals.xlsx
    #> │   ├── ModelParameters.xlsx
    #> │   ├── Plots.xlsx
    #> │   ├── Populations.xlsx
    #> │   ├── PopulationsCSV
    #> │   │   └── TestPopulation.csv
    #> │   └── Scenarios.xlsx
    #> ├── Data
    #> │   ├── TestProject_TimeValuesData.xlsx
    #> │   └── esqlabs_dataImporter_configuration.xml
    #> ├── Models
    #> │   └── Simulations
    #> │       └── Aciclovir.pkml
    #> ├── ProjectConfiguration.json
    #> ├── ProjectConfiguration.xlsx
    #> └── Results
    #>     ├── All plots.png
    #>     ├── Figures
    #>     └── SimulationResults

## Create a ProjectConfiguration

The `ProjectConfiguration` stores the paths to the model-, data-, and
other files and directories. It will be used at several steps in
`esqlabsR` workflows so it needs to be created before performing any
simulations.

By printing the `ProjectConfiguration`, we can see the locations of all
files used in the workflows:

``` r
my_project_configuration <- createProjectConfiguration(path = exampleProjectConfigurationPath())
```

Now that the project structure is initialized and the
`ProjectConfiguration` is created, read
[`vignette("design-scenarios")`](https://esqlabs.github.io/esqlabsR/articles/design-scenarios.md)
to continue the process. To learn more about `ProjectConfiguration`,
read the following sections.

## Details

### Change ProjectConfiguration from R

If required, you can change the location of one of the files or folders
using relative or absolute paths

``` r
# change the location of the output folder
projectConfiguration$outputFolder <- "../anotherOutputFolder"

# change the location of the model parameters file
projectConfiguration$modelParametersFile <- "absolute/path/to/anotherModelParameters.xlsx"
```

### About `ProjectConfiguration.xlsx`

The `ProjectConfiguration.xlsx` file will be used by `esqlabsR` to
generate a `ProjectConfiguration` object which is the central piece of a
project. It should be located in the root folder of the project.

This file defines *where* all the necessary files are stored in the
project folder. All the path specified in the `Value` column should be
*relative* to the `ProjectConfiguration.xlsx` location.

All these directories and files have a specific purpose and template and
are describe in next sections.

### Version Control and Project Sharing

While Excel provides an excellent interface for quickly editing and
creating new entries in your project configuration, it has limitations
when it comes to version control and team collaboration - Excel files
are binary and don’t work well with Git, making it cumbersome to share
changes between team members.

`esqlabsR` addresses these limitations by providing powerful version
control and project sharing capabilities through its snapshot and
restore functionality. This allows you to:

- **Track changes** to your project configuration in version control
  systems like Git
- **Share projects** with team members easily
- **Create backups** before making significant changes
- **Ensure reproducibility** across different environments

#### Creating Project Snapshots

The
[`snapshotProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/snapshotProjectConfiguration.md)
function exports all Excel configuration files in your project to a
single JSON file. This JSON format is ideal for version control as it’s:

- **Text-based**: Easy to track changes in Git
- **Human-readable**: Can be reviewed and compared
- **Complete**: Contains all project configuration data

``` r
# Create a snapshot from a ProjectConfiguration object
my_project_configuration <- createProjectConfiguration("ProjectConfiguration.xlsx")
snapshotProjectConfiguration(my_project_configuration)

# Or create a snapshot directly from the Excel file path
snapshotProjectConfiguration("ProjectConfiguration.xlsx")
```

By default, the JSON file will be created in the same directory as your
`ProjectConfiguration.xlsx` file with the name
`ProjectConfiguration.json`.

#### Restoring from Snapshots

The
[`restoreProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/restoreProjectConfiguration.md)
function recreates your entire project configuration from a JSON
snapshot:

``` r
# Restore a project from a JSON snapshot
restored_config <- restoreProjectConfiguration("ProjectConfiguration.json")
```

This function will: 1. Create the necessary directory structure 2.
Regenerate all Excel configuration files 3. Return a new
`ProjectConfiguration` object

#### Checking Project Status

Use
[`projectConfigurationStatus()`](https://esqlabs.github.io/esqlabsR/reference/projectConfigurationStatus.md)
to check if your Excel files are synchronized with your JSON snapshot:

``` r
# Check if files are in sync
status <- projectConfigurationStatus("ProjectConfiguration.xlsx", "ProjectConfiguration.json")
print(status$in_sync) # TRUE if synchronized, FALSE otherwise
```

#### Workflow Examples

**Team Collaboration**: When working with a team:

1.  **Initial Setup**: Create a snapshot of your project configuration

    ``` r
    snapshotProjectConfiguration("ProjectConfiguration.xlsx")
    ```

2.  **Version Control**: Commit the JSON file to your Git repository

    ``` bash
    git add ProjectConfiguration.json
    git commit -m "Add project configuration snapshot"
    ```

3.  **Team Sharing**: Team members can restore the project

    ``` r
    restored_config <- restoreProjectConfiguration("ProjectConfiguration.json")
    ```

**Backup Strategy**: Before making significant changes to your project:

``` r
# Create a backup snapshot
snapshotProjectConfiguration("ProjectConfiguration.xlsx")

# Make your changes...

# If something goes wrong, restore from the snapshot
restored_config <- restoreProjectConfiguration("ProjectConfiguration.json")
```

The version control features in `esqlabsR` provide a robust foundation
for team collaboration and project management. By using snapshots, you
can maintain project history in version control systems, share
configurations easily with team members, create reliable backups before
making changes, and ensure reproducibility across different
environments.
