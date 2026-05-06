# Load application support files into testing environment

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
