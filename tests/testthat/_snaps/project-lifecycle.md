# saveProject errors when project has no jsonPath and path is NULL

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      ! No path specified and project has no jsonPath. Provide a path argument.

# saveProject errors on non-Project input

    Code
      saveProject("not a project")
    Condition
      Error in `validateIsOfType()`:
      ! `btw_mcp_server()`: argument "project" is of type <character>, but expected <Project>!

