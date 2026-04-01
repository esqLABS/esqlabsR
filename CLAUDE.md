# esqlabsR — Claude Instructions

## Project Overview

`{esqlabsR}` is an R package that facilitates and standardizes the modeling and simulation of physiologically based kinetic (PBK) and quantitative systems pharmacology/toxicology (QSP/T) models implemented in the [Open Systems Pharmacology Software](https://www.open-systems-pharmacology.org/) (OSPS). It extends `{ospsuite}`, `{ospsuite.utils}`, and `{tlf}`.

**Key dependencies:** `{ospsuite}`, `{ospsuite.utils}`, `{tlf}`, `{R6}`, `{ggplot2}`, `{cli}`, `{lifecycle}`

## Development Commands

```bash
# Load the package for interactive use
Rscript -e "devtools::load_all()"

# Run all tests
Rscript -e "devtools::test()"

# Run tests for a specific file (e.g., R/utilities.R)
Rscript -e "devtools::test(filter = '^utilities$')"

# Re-generate documentation
Rscript -e "devtools::document()"

# Check the package (R CMD check)
Rscript -e "devtools::check()"

# Format all R code
air format .

# Check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"
```

## Coding Standards

### Naming Conventions

- **Files:** kebab-case with `.R` extension (`scenario-configuration.R`, `test-scenario-configuration.R`)
- **Variables and functions:** `camelCase` (e.g., `performSimulation`, `parameterToDelete`)
- **Classes (R6):** PascalCase (e.g., `Scenario`, `ScenarioConfiguration`)
- **Constants:** `ALL_CAPS` (e.g., `DEFAULT_PERCENTILE`)
- **Internal functions:** prefix with `.` (e.g., `.runSingleSimulation`)
- Do **not** use Hungarian notation

### Code Style

- Use `<-` for assignment (never `=`)
- Indentation: 2 spaces (no tabs)
- Line length limit: 80 characters
- Use `TRUE`/`FALSE`, never `T`/`F`
- Prefer `return()` explicitly, even when optional
- No trailing semicolons
- Use `|>` (base pipe), not `%>%` (magrittr pipe)
- Use `\() ...` for single-line anonymous functions; `function() {...}` for multi-line
- Run `air format .` after writing any R code

### R6 Classes

- Classes use R6 with `cloneable = FALSE` by default
- Active bindings enforce read-only properties via `stop(messages$errorPropertyReadOnly(...))`
- Every class must implement a meaningful `print()` method
- Private methods are prefixed with `.` (e.g., `$.myPrivateMethod()`)
- Each class lives in its own file named after the R class in kebab-case

```r
# Example class structure
MyClass <- R6::R6Class(
  "MyClass",
  cloneable = FALSE,
  active = list(
    myProp = function(value) {
      if (missing(value)) {
        private$.myProp
      } else {
        stop(messages$errorPropertyReadOnly("myProp"))
      }
    }
  ),
  public = list(
    initialize = function(...) { ... },
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      invisible(self)
    }
  ),
  private = list(
    .myProp = NULL
  )
)
```

### Documentation

- All exported functions and classes use roxygen2 (`#'`) documentation
- Wrap roxygen comments at 80 characters
- Internal-only functions use `#' @keywords internal`
- Use markdown syntax in roxygen (`**bold**` not `\bold{}`)
- Reference functions/methods with `()` (e.g., `myFunction()`)
- Reference package names with `{}` (e.g., `{dplyr}`)
- Reference variables/objects as inline code (e.g., `myVar`)
- Add new exported topics to `_pkgdown.yml`
- Always run `devtools::document()` after changing roxygen comments

### Error Messages

- Error, warning, and informational messages are defined in `R/messages.R` as an `enum`
- Use `cli::cli_abort()`, `cli::cli_warn()`, and `cli::cli_inform()` for user-facing messages
- Reference messages via the `messages` enum (e.g., `stop(messages$myError())`)

### `NEWS.md`

- Add a bullet for every user-facing change; skip small documentation fixes and internal refactoring
- Format: brief description + issue reference in parentheses
- Keep bullets to a single line (no wrapping)
- Mention the function name early when relevant
- Order bullets alphabetically by function name

## Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`
- All new exported functionality must have tests
- Use `testthat` edition 3 (`Config/testthat/edition: 3`)
- Use snapshot testing with `expect_snapshot(error = TRUE)` for errors and warnings
- Avoid `expect_true()` and `expect_false()`; prefer specific expectations
- Tests involving `{ospsuite}` simulations may require a running OSPSuite environment; use `testthat::skip_if_not()` guards when necessary
- Use `withr` for side-effect cleanup (temp files, options, env vars)
- Snapshot files live in `tests/testthat/_snaps/`
- When visual output changes, review snapshots with `testthat::snapshot_review()` before accepting

## File Structure

```
R/
  {class-name}.R          # R6 class definitions
  utilities-{topic}.R     # Utility functions for a topic
  validation-{topic}.R    # Input validation functions
  messages.R              # All user-facing messages
  globals.R               # Global variable declarations
  zzz.R                   # Package load/unload hooks
tests/testthat/
  test-{name}.R           # Tests mirroring R/{name}.R
  setup.R                 # Shared test setup
```

## Collaboration Workflow

1. Each change must be related to an issue on the repository
2. Create a separate branch per change (`usethis::pr_init("branch-name")`)
3. Apply `air format .` before committing
4. Open a pull request with `usethis::pr_push()`
5. Every user-visible change needs an entry in `NEWS.md`
6. Every PR must be reviewed by at least one other contributor before merging
