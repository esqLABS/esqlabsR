---
name: ospsuite-r-coding-standards
description: >
  Coding standards, best practices, and collaboration guidelines for the
  esqlabsR package. Use when writing, reviewing, or refactoring R code in
  esqlabsR. Covers OSPSuite-specific R conventions, R6 class design, naming,
  documentation, and the pull-request workflow.
---

# OSPSuite R Coding Standards for esqlabsR

Based on the [OSPSuite R coding standards](https://github.com/Open-Systems-Pharmacology/developer-docs/blob/main/ospsuite-r-specifics/CODING_STANDARDS_R.md) and [collaboration guide](https://github.com/Open-Systems-Pharmacology/developer-docs/blob/main/ospsuite-r-specifics/collaboration_guide.md).

## Naming Conventions

### Files

Use **kebab-case** with `.R` extension for all source and test files:

```r
# Bad
DataCombined.R
test-DataCombined.R

# Good
data-combined.R
test-data-combined.R
```

### Objects

| Type | Convention | Example |
|------|-----------|---------|
| Variables & functions | `camelCase` | `performSimulation`, `parameterToDelete` |
| R6 classes | PascalCase | `Scenario`, `ScenarioConfiguration` |
| Constants | `ALL_CAPS` | `DEFAULT_PERCENTILE` |
| Internal functions | prefix with `.` | `.runSingleSimulation` |

**Do not** use Hungarian notation (no `b` for Boolean, `s` for string, etc.).

## R6 Class Design

Every R6 class in esqlabsR follows a consistent pattern:

```r
#' @title MyClass
#' @docType class
#' @description Brief description.
#' @format NULL
#' @export
MyClass <- R6::R6Class(
  "MyClass",
  cloneable = FALSE,
  active = list(
    #' @field myProp Description. Read-only.
    myProp = function(value) {
      if (missing(value)) {
        private$.myProp
      } else {
        stop(messages$errorPropertyReadOnly("myProp"))
      }
    }
  ),
  public = list(
    #' @description Create a new `MyClass` object.
    #' @param myArg Description of argument.
    #' @return A new `MyClass` object.
    initialize = function(myArg) {
      private$.myProp <- myArg
      invisible(self)
    },

    #' @description Print the object to the console.
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "My property" = self$myProp
      ))
      invisible(self)
    }
  ),
  private = list(
    .myProp = NULL
  )
)
```

**Key rules:**
- Set `cloneable = FALSE` by default
- Read-only properties use active bindings + `stop(messages$errorPropertyReadOnly(...))`
- Always implement a `print()` method
- Private methods start with `.` (e.g., `.computeInternal()`)
- Internal (non-exported) classes use `#' @keywords internal`
- Utility functions for a class go in `utilities-{class-name}.R`

## Code Style

### Assignments

```r
# Good
x <- 5

# Bad
x = 5
```

### Booleans

```r
# Good
if (condition == TRUE) { ... }

# Bad
if (condition == T) { ... }
```

### Explicit Returns

Prefer `return()` even when implicit return works:

```r
# Good
myFunction <- function(x) {
  result <- x * 2
  return(result)
}

# Acceptable (for simple one-liners)
myFunction <- function(x) x * 2
```

### Code Blocks

```r
# Good
if (condition) {
  doSomething()
} else {
  doOther()
}

# OK for simple side-effect-free statements
y <- if (x < 20) "Too low" else "Too high"
```

### Pipes

```r
# Good — base pipe
result <- data |> filter(x > 0) |> select(y)

# Bad — magrittr pipe
result <- data %>% filter(x > 0) %>% select(y)
```

## Documentation Standards

### Functions

```r
#' Brief one-line title
#'
#' @description Optional longer description using **markdown**.
#'
#' @param x Description of `x`. Wrap at 80 characters.
#' @param y Description of `y`.
#'
#' @returns Description of the return value.
#'
#' @examples
#' myFunction(x = 1, y = 2)
#'
#' @export
myFunction <- function(x, y) { ... }
```

### R6 Classes

Document the class with one roxygen block before the `R6Class()` call.
Use the `@field` tag for active bindings. Use the `@description` tag inside
`initialize` and other methods. Reference properties with `$` prefix:

> `$myProp` for properties, `$myMethod()` for methods.

### Conventions in Text

- Functions: `` `myFunction()` `` (with parentheses)
- Variables/objects: `` `myVar` ``
- Package names: `` `{dplyr}` ``
- Programming languages: `` `R` ``, `` `C++` ``

## Error and Warning Messages

All messages are defined as functions on the `messages` enum in `R/messages.R`.
Never inline message strings—always add them to `messages`:

```r
# Bad
stop("The property is read-only.")

# Good
stop(messages$errorPropertyReadOnly("myProp"))
```

Use `{cli}` functions for user-facing output:

```r
cli::cli_abort("Something went wrong: {.val {value}}")
cli::cli_warn("Deprecated: use {.fn newFunction} instead.")
cli::cli_inform("Loading {.pkg ospsuite}...")
```

## Global Variables and Constants

- Avoid global variables; if truly needed, discuss with the team first
- No magic numbers or hardcoded strings — declare constants instead:

```r
# Bad
if (percentile > 0.5) { ... }

# Good
DEFAULT_PERCENTILE <- 0.5
if (percentile > DEFAULT_PERCENTILE) { ... }
```

## Cyclomatic Complexity

Keep cyclomatic complexity ≤ 15 per function. Check with:

```r
cyclocomp::cyclocomp(myFunction)
cyclocomp::cyclocomp_package("esqlabsR")
```

If complexity exceeds 15, break the function into smaller pieces.

## Collaboration Workflow

### Branch and PR Rules

1. Every change relates to a GitHub issue
2. Every change is made on a separate branch
3. Every change is proposed via a pull request
4. Every user-visible change is reflected in `NEWS.md`
5. Every PR is reviewed by at least one other contributor
6. `{styler}` or `air format .` must be run before opening a PR

### Recommended Workflow

```r
# Start work
usethis::pr_init("my-feature-branch")

# ... make changes, commit ...

# Open PR
usethis::pr_push()

# Pause and switch back to main
usethis::pr_pause()

# Resume work on an existing PR
usethis::pr_fetch(123)   # PR number

# Finish after merge
usethis::pr_finish()
```

### PR Checklist

Before marking a PR as "ready for review":

- [ ] `air format .` run on all changed files
- [ ] `devtools::document()` run if roxygen comments changed
- [ ] `devtools::test()` passes
- [ ] `NEWS.md` updated for user-visible changes
- [ ] `_pkgdown.yml` updated if new exported functions added
- [ ] No commented-out code or debug artifacts
