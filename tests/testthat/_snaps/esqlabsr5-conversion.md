# convertProjectToV6() refuses a path that is not an esqlabsR 5 workbook

    Code
      convertProjectToV6(file.path(projectDir, "Missing.xlsx"))
    Condition
      Error in `convertProjectToV6()`:
      ! File not found: '<tmp-path>'

---

    Code
      convertProjectToV6(file.path(projectDir, "Data"))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is not an Excel file. `convertProjectToV6()` reads the 'ProjectConfiguration.xlsx' workbook of an esqlabsR 5 project.

---

    Code
      convertProjectToV6(file.path(projectDir, "Data",
        "esqlabs_dataImporter_configuration.xml"))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is not an Excel file. `convertProjectToV6()` reads the 'ProjectConfiguration.xlsx' workbook of an esqlabsR 5 project.

---

    Code
      convertProjectToV6(file.path(projectDir, "Data",
        "TestProject_TimeValuesData.xlsx"))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is not the project configuration of an esqlabsR 5 project: its first sheet has no Property and Value columns.

# convertProjectToV6() refuses a workbook that is already in the v6 layout

    Code
      convertProjectToV6(workbook)
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' is already a v6 workbook: its property sheet has a schemaVersion row.
      i Import it with `importProjectFromExcel()`.

# convertProjectToV6() refuses a folder that holds a v6 project or an earlier conversion

    Code
      convertProjectToV6(configPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' already holds a v6 project or an earlier conversion: 'Project.json' and 'definitions'.
      i Load the v6 project with `loadProject()`. To convert the esqlabsR 5 workbooks again, first move these out of the folder.

---

    Code
      convertProjectToV6(configPath(projectDir))
    Condition
      Error in `convertProjectToV6()`:
      x '<tmp-path>' already holds a v6 project or an earlier conversion: 'Project.xlsx' and 'esqlabsR5'.
      i Load the v6 project with `loadProject()`. To convert the esqlabsR 5 workbooks again, first move these out of the folder.

