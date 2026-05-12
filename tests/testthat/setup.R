# Load application support files into testing environment

# Silence lifecycle::deprecate_soft() warnings emitted by tests that still
# exercise legacy entry points (createPlotsFromExcel, createDataCombinedFromExcel,
# loadObservedDataFromExcel, loadObservedDataFromPKML). Tests for the new
# JSON-driven entry points (createPlots, createDataCombined, loadObservedData)
# do not call the deprecated wrappers. Removed when the legacy paths retire.
options(lifecycle_verbosity = "quiet")
