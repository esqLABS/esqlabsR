server <- function(input, output, session) {
  colname <- ""

  # add more functions
  # maybe use base namespace
  mathexpressions <- c("exp", "log", "sin", "sqrt", "floor", "ceiling", "pi")
  currentArg <- NULL

  v <- reactiveValues(
    update = TRUE, varnames = NULL, equation = NULL,
    mins = NULL, maxs = NULL, current = NULL, x = NULL,
    y = NULL, snapshots = NULL, points_x = NULL, points_y = NULL,
    observedData = data.frame()
  )

  observeEvent(input$updatevars, {
    if (!is.null(input$argument)) {
      currentArg <<- input$argument
    }
    if (v$equation != input$equation) {
      colnames(v$snapshots) <- c("x_values", paste(v$equation, "with", colnames(v$snapshots)[-1]))
    }
    v$update <- input$updatevars
  })

  observeEvent(input$addPoint, {
    v$points_x <- c(v$points_x, input$xvalue)
    v$points_y <- c(v$points_y, input$yvalue)
  })

  observeEvent(input$removePoints, {
    v$points_x <- NULL
    v$points_y <- NULL
  })

  # dynamically add slider and numeric inputs (min/max/current) for variables in equation
  output$variables <- renderUI({
    if (!v$update) {
      return()
    }

    isolate({
      v$equation <- input$equation
      # variable names: remove all blanks from equation (gsub),
      # split equation string at all arithmetic operators (strsplit),
      # remove numeric values as they are not variables (grep)
      v$varnames <- setdiff(
        grep("[^[:digit:]*.?[:digit:]+]",
          unlist(strsplit(
            gsub("[[:blank:]]", "", input$equation),
            "[-+*/(){}]|(%%)|(%/%)"
          )),
          value = TRUE
        ),
        mathexpressions
      )

      # safe IDs of dynamically added UI elements
      v$mins <- paste0("min", v$varnames)
      v$maxs <- paste0("max", v$varnames)
      v$current <- paste0("current", v$varnames)

      # create min, max, current - numeric inputs for each variable in the equation
      # plus slider input
      variable_list <- lapply(1:length(v$varnames), function(i) {
        splitLayout(
          cellWidths = c("10%", "70%", "10%", "10%"),
          numericInput(v$mins[i], "min", 0),
          sliderInput(v$varnames[i], v$varnames[i], 0, 100, 1),
          numericInput(v$maxs[i], "max", 100),
          numericInput(v$current[i], "current", 1)
        )
      })

      if (!is.null(currentArg) && currentArg %in% v$varnames) {
        variable_list <- list(variable_list, radioButtons("argument", "Argument",
          choices = v$varnames,
          selected = currentArg
        ))
      } else {
        variable_list <- list(variable_list, radioButtons("argument", "Argument",
          choices = v$varnames
        ))
      }

      do.call(tagList, variable_list)
    })
  })

  # update slider min when corresponding value in numeric input field is changed
  observer_min <- observe({
    lapply(1:length(v$mins), function(i) {
      newval <- input[[v$mins[i]]]
      isolate(updateSliderInput(session, v$varnames[i], min = newval))
    })
  })

  # update slider max when corresponding value in numeric input field is changed
  observer_max <- observe({
    lapply(1:length(v$maxs), function(i) {
      newval <- input[[v$maxs[i]]]
      stepsize <- as.numeric(paste0("1e", floor(log10(newval - input[[v$mins[i]]])) - 2))
      isolate({
        updateSliderInput(session, v$varnames[i], max = newval, step = stepsize)
        updateNumericInput(session, paste0("current", v$varnames[i]), step = stepsize)
      })
    })
  })

  # update current slider position when corresponding value in numeric input field is changed
  observer_current <- observe({
    lapply(1:length(v$current), function(i) {
      newval <- input[[v$current[i]]]
      isolate(updateSliderInput(session, v$varnames[i], value = newval))
    })
  })

  # update numeric input field 'current' when corresponding slider is used
  observer_slider <- observe({
    lapply(1:length(v$current), function(i) {
      newval <- input[[v$varnames[i]]]
      isolate(updateNumericInput(session, v$current[i], value = newval))
    })
  })

  output$plot <- renderPlot({
    if (is.null(input$argument) || !(input$argument %in% v$varnames)) {
      return()
    }

    arg <- input$argument
    shinyjs::disable(arg)
    shinyjs::disable(paste0("current", arg))
    x_min <- input[[paste0("min", arg)]]
    x_max <- input[[paste0("max", arg)]]
    stepsize <- as.numeric(paste0("1e", floor(log10(x_max - x_min)) - 2))

    x_values <- seq(x_min, x_max, stepsize)
    # insert space before and after arithmetic operators in equation string
    eq <- paste0(gsub("([-+*/(){}]|(%%)|(%/%))", " \\1 ", v$equation), " ")

    colname <<- ""

    for (i in v$varnames) {
      if (i != arg) {
        # match variable+space so variable names are not matched in function names, e.g. x in exp
        eq <- gsub(paste0(i, " "), input[[i]], eq)
        colname <<- paste(colname, i, "=", input[[i]])
      } else {
        eq <- gsub(paste0(i, " "), "x_values", eq)
      }
    }

    # compute y values
    y_values <- eval(parse(text = eq))
    v$x <- x_values
    v$y <- y_values

    # no snapshots yet: plot only current x_values and y_values
    if (is.null(v$snapshots)) {
      if (sum(is.finite(y_values)) == 0) {
        # empty plot e.g. because there is a division by 0 in the equation
        plot.new()
      } else {
        plot(x_values, y_values,
          xlab = arg, ylab = v$equation, type = "l",
          xlim = c(input$xLower, input$xUpper),
          ylim = c(input$yLower, input$yUpper)
        )
      }

      # plot data uploaded from file
      if (nrow(v$observedData) != 0) {
        matpoints(v$observedData[, 1], v$observedData[, -1],
          pch = 1,
          col = rev(rainbow(ncol(v$observedData) - 1))
        )
        legend("topleft",
          legend = colnames(v$observedData)[-1],
          col = rev(rainbow(ncol(v$observedData) - 1)),
          pch = 1
        )
      }
    } else {
      # one or more snapshots have already been safed: add current values to plot
      plotdata <- v$snapshots
      if (!(colname %in% colnames(plotdata))) {
        newdata <- data.frame(v$y, v$x)
        colnames(newdata) <- c(colname, "x_values")
        plotdata <- merge(v$snapshots, newdata, by = "x_values", all = TRUE)
      }

      plotdata <- subset(plotdata, x_values <= x_max)
      plotdata <- subset(plotdata, x_values >= x_min)

      ys <- unlist(plotdata[, -1])
      ys <- ys[is.finite(ys)]
      y_min <- min(ys)
      y_max <- max(ys)
      lenObsData <- max(ncol(v$observedData) - 1, 0)
      color <- rainbow(max(ncol(plotdata) - 2, lenObsData))

      if (ncol(plotdata) == 2) {
        matplot(plotdata[, 1], plotdata[, -1],
          type = "l", xlab = arg, ylab = v$equation,
          col = 1, lty = 1,
          xlim = c(input$xLower, input$xUpper),
          ylim = c(input$yLower, input$yUpper)
        )
      } else {
        matplot(NA,
          xlim = c(input$xLower, input$xUpper),
          ylim = c(input$yLower, input$yUpper),
          xlab = arg, ylab = v$equation
        )

        mapply(function(y, color) {
          dat <- na.omit(cbind(plotdata[, 1], y))
          matlines(dat[, 1], dat[, 2], col = color)
        }, plotdata[, -1], c(color[0:(ncol(plotdata) - 2)], 1))
      }

      if (nrow(v$observedData) != 0) {
        matpoints(v$observedData[, 1], v$observedData[, -1], pch = 1, col = color)
      }

      legend("topleft",
        legend = c(colnames(v$observedData)[-1], colnames(plotdata)[-1]),
        col = c(color[0:lenObsData], color[0:(ncol(plotdata) - 2)], 1),
        lty = c(rep(NA, max(ncol(v$observedData) - 1, 0)), rep(1, ncol(plotdata))),
        pch = c(rep(1, max(ncol(v$observedData) - 1, 0)), rep(NA, ncol(plotdata)))
      )
    }

    # add single points to plot
    if (!is.null(v$points_x)) matpoints(v$points_x, v$points_y, pch = 3)
  })

  # observer for "Add Snapshot" button - current values are added as column to
  # reactive data frame v$snapshots
  observeEvent(input$addSnapshot, {
    isolate({
      newdata <- data.frame(v$x, v$y)
      colnames(newdata) <- c("x_values", colname)
      if (is.null(v$snapshots)) {
        v$snapshots <- newdata
      } else if (!(colname %in% names(v$snapshots))) {
        v$snapshots <- merge(v$snapshots, newdata, by = "x_values", all = TRUE)
      }
    })
  })

  # set snapshots data frame to NULL when "Remove Snapshots" button is clicked
  observeEvent(input$removeSnapshots, {
    v$snapshots <- NULL
  })

  # set snapshots data frame to NULL when argument changes
  observeEvent(input$argument, {
    v$snapshots <- NULL
  })

  # disable slider for argument, enable all others
  observer_argument <- observe({
    if (is.null(input$argument)) {
      return()
    }
    arg <- input$argument

    lapply(1:length(v$varnames), function(i) {
      variable <- v$varnames[i]
      if (variable != arg) {
        shinyjs::enable(variable)
        shinyjs::enable(paste0("min", variable))
        shinyjs::enable(paste0("max", variable))
        shinyjs::enable(paste0("current", variable))
      }
    })

    shinyjs::disable(arg)
    shinyjs::enable(paste0("min", arg))
    shinyjs::enable(paste0("max", arg))
    shinyjs::disable(paste0("current", arg))
  })

  # file input popup
  observeEvent(input$chooseFile, {
    file <- input$chooseFile
    if (grepl("(.xlsx$)|(.xls$)", file$name)) {
      content <- readxl::read_excel(file$datapath)
      showModal(modalDialog(
        tags$h4("Please choose columns for the x and y axis"),
        selectInput("fileX", "x axis", choices = names(content)),
        selectInput("fileY", "y axis", choices = names(content)),
        textInput("fileLabel", "Legend label", placeholder = "optional - legend label in plot"),
        output$table <- renderTable(head(content)),
        footer = tagList(
          actionButton("submit", "Submit"),
          modalButton("cancel")
        )
      ))
    } else {
      showModal(modalDialog(
        tags$h4("Please choose a xls/xlsx file"),
        footer = modalButton("OK")
      ))
    }
  })

  # handle input from popup, add to plot
  observeEvent(input$submit, {
    removeModal()
    file <- input$chooseFile
    content <- readxl::read_excel(file$datapath)
    content <- content[, c(input$fileX, input$fileY)]
    if (input$fileLabel == "") {
      names(content) <- c("x_values", paste(input$fileY, file$name))
    } else {
      names(content) <- c("x_values", input$fileLabel)
    }

    if (nrow(v$observedData) == 0) {
      v$observedData <- content
    } else {
      v$observedData <- merge(v$observedData, content, by = "x_values", all = TRUE)
    }
  })

  observeEvent(input$removeLatestObsData, {
    if (ncol(v$observedData) > 2) {
      v$observedData <- v$observedData[1:(ncol(v$observedData) - 1)]
    } else {
      v$observedData <- data.frame()
    }
  })

  observeEvent(input$removeObsData, {
    v$observedData <- data.frame()
  })
}
