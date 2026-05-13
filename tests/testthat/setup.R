# Load application support files into testing environment

# Default to quiet so incidental callsites of the surviving soft-deprecation
# wrappers (ProjectConfiguration(), createProjectConfiguration(), the
# snapshot/restore stubs) don't add noise to the suite. Tests that
# specifically assert a `lifecycle_warning_deprecated` use
# `withr::local_options(lifecycle_verbosity = "warning")` to opt back in.
options(lifecycle_verbosity = "quiet")
