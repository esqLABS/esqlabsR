# dependencies: ospsuite and shinyjs

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Unit Converter"),
  selectInput("dimension", "Dimension", choices = ospsuite::ospDimensions),
  uiOutput("units")
)


