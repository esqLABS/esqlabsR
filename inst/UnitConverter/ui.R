ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Unit Converter"),
  selectInput("dimension", "Dimension",
    choices = ospsuite::ospDimensions[ospsuite::ospDimensions != "Dimensionless"]
  ),
  uiOutput("units")
)
