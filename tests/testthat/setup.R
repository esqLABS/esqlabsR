# Load application support files into testing environment

# Catch length>1 conditions in `if` / `while` at test time. Set here rather than
# from the package's `.onLoad()`, which would mutate global R behaviour for
# every other package in the user's session; this scopes it to the test session.
withr::local_envvar(
  .new = c("_R_CHECK_LENGTH_1_CONDITION_" = "true"),
  .local_envir = testthat::teardown_env()
)

# Default to quiet so incidental callsites of the surviving deprecation
# wrapper (`ProjectConfiguration()`) don't add noise to the suite. Tests that
# specifically assert a `lifecycle_warning_deprecated` use
# `withr::local_options(lifecycle_verbosity = "warning")` to opt back in.
# Scoped to the test session (not a bare `options()`) so it does not leak.
withr::local_options(
  lifecycle_verbosity = "quiet",
  .local_envir = testthat::teardown_env()
)

