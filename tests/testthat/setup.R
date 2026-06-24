# Load application support files into testing environment

# Catch length>1 conditions in `if` / `while` at test time. Previously set
# from the package's `.onLoad`, which mutated global R behaviour for every
# other package in the user's session. Scope it to the test session here.
withr::local_envvar(
  .new = c("_R_CHECK_LENGTH_1_CONDITION_" = "true"),
  .local_envir = testthat::teardown_env()
)

# Default to quiet so incidental callsites of the surviving soft-deprecation
# wrappers (ProjectConfiguration(), createProjectConfiguration(), the
# snapshot/restore stubs) don't add noise to the suite. Tests that
# specifically assert a `lifecycle_warning_deprecated` use
# `withr::local_options(lifecycle_verbosity = "warning")` to opt back in.
options(lifecycle_verbosity = "quiet")
