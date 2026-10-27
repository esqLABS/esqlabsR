test_that("cliBullets() keeps the bullet names and interpolates its own arguments", {
  entry <- function(scenarioNames) {
    cliBullets(
      "!" = "{length(scenarioNames)} scenario{?s}: {.val {scenarioNames}}.",
      "i" = "A hint."
    )
  }
  bullets <- entry("b")
  expect_named(bullets, c("!", "i"))
  expect_identical(unname(bullets), c("1 scenario: \"b\".", "A hint."))

  # Raised from a frame that binds the same name to other values, the message
  # still names only the argument.
  scenarioNames <- c("a", "b", "c")
  warning <- rlang::catch_cnd(cli::cli_warn(entry("b")), "warning")
  expect_match(conditionMessage(warning), "1 scenario: \"b\"", fixed = TRUE)
})

test_that("cliBullets() lets a value with braces through a cli_abort() as written", {
  entry <- function(name) {
    cliBullets(
      "x" = "Scenario {.val {name}} failed.",
      "i" = "Value {name}, literal {.code {{x}}}."
    )
  }
  expect_snapshot(
    cli::cli_abort(entry("Fit {old}")),
    error = TRUE
  )
})

test_that("a bullet message puts each bullet on its own line", {
  expect_snapshot(
    cli::cli_abort(messages$invalidSimulationRunOptions(4)),
    error = TRUE
  )
  expect_snapshot(
    cli::cli_warn(messages$scenariosSkipped(c("first", "second")))
  )
})
