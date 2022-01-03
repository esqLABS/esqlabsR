ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$style(type = "text/css", ".modal-dialog {overflow: auto; width: auto; background:white;}"),
  splitLayout(
    wellPanel(
      wellPanel(
        textInput(inputId = "equation", "Equation:", "Vmax * C / (Km + C)"),
        actionButton("updatevars", "Update variables")
      ),
      uiOutput("variables")
    ),
    wellPanel(
      plotOutput("plot"),
      actionButton("addSnapshot", "Add Snapshot"),
      actionButton("removeSnapshots", "Remove Snapshots"),
      hr(),
      p(strong("Axis limits")),
      splitLayout(
        cellWidths = c("20%", "20%"),
        numericInput("xLower", value = 0, label = "x min", step = 0.1),
        numericInput("xUpper", value = 100, label = "x max", step = 0.1)
      ),
      splitLayout(
        cellWidths = c("20%", "20%"),
        numericInput("yLower", value = 0, label = "y min", step = 0.1),
        numericInput("yUpper", value = 100, label = "y max", step = 0.1)
      ),
      hr(),
      p(strong("Add single points to plot")),
      splitLayout(
        cellWidths = c("20%", "20%"),
        numericInput("xvalue", value = 0, label = "x", step = 0.01),
        numericInput("yvalue", value = 0, label = "y", step = 0.01)
      ),
      actionButton("addPoint", "Add Point"),
      actionButton("removePoints", "Remove All Points"),
      hr(),
      p(strong("Add data from .xlsx file")),
      fileInput("chooseFile", label = NULL, accept = ".xlsx"),
      actionButton("removeLastObsData", "Remove last added data"),
      actionButton("removeObsData", "Remove all uploaded data")
    )
  )
)
