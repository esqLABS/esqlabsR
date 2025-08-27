library(hexSticker)
library(magick)
library(showtext)
library(usethis)

# Loading Google fonts (http://www.google.com/fonts)
google_font_name <- "Comfortaa"
font_add_google(google_font_name)

# Automatically use showtext to render text for future devices
showtext_auto()

company <- image_read("hextools/circles.png")

sticker(
  company,
  package = "esqlabsR",
  p_color = "#545452",
  p_family = google_font_name,
  p_size = 40,
  p_x = 1,
  p_y = 1.3,
  s_x = 1,
  s_y = 0.85,
  s_width = 1.5,
  s_height = 1.10,
  h_color = "grey",
  filename = "hextools/esqlabsR-logo.png",
  h_fill = "white",
  url = "         https://esqlabs.com/",
  u_size = 10,
  u_color = "grey",
  dpi = 600
)

usethis::use_logo("hextools/esqlabsR-logo.png")

file.copy("hextools/circles.png", "man/figures/circles.png", overwrite = TRUE)
