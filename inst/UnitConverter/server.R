server <- function(input, output, session) {
  dimension <- NULL
  currentMolWeight <- NULL
  currentUnit1 <- NULL
  currentUnit2 <- NULL

  output$units <- renderUI({
    dimension <<- input$dimension

    if (dimension %in% c("Concentration (mass)", "Concentration (molar)")) {
      units <- c(
        ospsuite::ospUnits[["Concentration [mass]"]],
        ospsuite::ospUnits[["Concentration [molar]"]]
      )
      currentMolWeight <<- 1
    } else if (dimension %in% c("Amount", "Mass")) {
      units <- c(
        ospsuite::ospUnits[["Amount"]],
        ospsuite::ospUnits[["Mass"]]
      )
      currentMolWeight <<- 1
    } else {
      units <- ospsuite::ospUnits[[gsub("(\\()(.*)(\\))", "[\\2]", dimension)]]
      currentMolWeight <<- NULL
    }

    currentUnit1 <<- units[[1]]
    currentUnit2 <<- units[min(2, length(units))]

    isolate({
      ui <- list(
        fluidRow(
          column(
            3, selectInput("unit1", "Unit 1", choices = units, selected = currentUnit1),
            numericInput("value1", NULL, value = 0)
          ),
          column(
            3,
            selectInput("unit2", "Unit 2", choices = units, selected = currentUnit2),
            numericInput("value2", NULL, value = 0)
          ),
          column(
            2,
            if (dimension %in% c("Concentration (mass)", "Concentration (molar)", "Amount", "Mass")) {
              numericInput("molweight", "Molecular weight in g/mol", value = currentMolWeight)
            } else {
              shinyjs::disabled(numericInput("molweight", "Molecular weight in g/mol", value = 1))
            }
          )
        )
      )

      do.call(tagList, ui)
    })
  })

  observer_left <- observe({
    if (is.null(input$value1) || is.na(input$value1) || !is.numeric(input$molweight)) {
      return()
    }

    currentMolWeight <<- input$molweight
    currentUnit1 <<- input$unit1
    currentUnit2 <<- input$unit2

    converted <- ospsuite::toUnit(dimension, input$value1,
      targetUnit = currentUnit2,
      sourceUnit = currentUnit1, molWeight = currentMolWeight,
      molWeightUnit = "g/mol"
    )
    isolate(updateNumericInput(session, "value2", value = converted))
  })

  observer_right <- observe({
    if (is.null(input$value2) || is.na(input$value2)) {
      return()
    }

    converted <- ospsuite::toUnit(dimension, input$value2,
      targetUnit = currentUnit1,
      sourceUnit = currentUnit2, molWeight = currentMolWeight,
      molWeightUnit = "g/mol"
    )
    isolate(updateNumericInput(session, "value1", value = converted))
  })
}
