# Load application support files into testing environment

# Catch length>1 conditions in `if` / `while` at test time. Previously set
# from the package's `.onLoad`, which mutated global R behaviour for every
# other package in the user's session. Scope it to the test session here.
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


# Disable showtext for snapshot tests.
#
# `tlf` (a transitive dep of `ospsuite`) calls `showtext::showtext_auto()` on
# load, which makes every graphics device rasterize glyphs to filled SVG paths
# using FreeType + the system "sans" font. That font resolves to different
# files on different OSes (Helvetica on macOS, DejaVu/Liberation on Linux),
# producing sub-pixel differences in glyph outlines and breaking byte-exact
# vdiffr snapshot comparisons across platforms. Disabling showtext makes
# svglite emit portable `<text>` elements with the actual string content
# instead of platform-dependent glyph paths.
if (requireNamespace("showtext", quietly = TRUE)) {
  showtext::showtext_auto(enable = FALSE)
}
