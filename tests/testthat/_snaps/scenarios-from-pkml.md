# createScenariosFromPKML errors on non-Project input

    Code
      createScenariosFromPKML(pkmlFixture, project = "not a project")
    Condition
      Error in `validateIsOfType()`:
      ! `btw_mcp_server()`: argument "project" is of type <character>, but expected <Project>!

