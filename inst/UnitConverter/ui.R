ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Unit Converter"),
  selectInput("dimension", "Dimension", choices = ospsuite::ospDimensions),
  uiOutput("units")
)
