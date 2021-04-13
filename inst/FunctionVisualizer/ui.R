ui <- fluidPage(
  shinyjs::useShinyjs(),
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
        p(strong("Add single points to plot")),
        splitLayout(numericInput("xvalue", value = 0, label = "x", step = 0.01),
                    numericInput("yvalue", value = 0, label = "y", step = 0.01)),
        actionButton("addPoint", "Add Point"),
        actionButton("removePoints", "Remove All Points")
      )
  )
)
