transferFunctions <- function(projectConfiguration, outputToPNG = FALSE) {
  M_SGLT1_GLP1_syn(12, 30000, 2)
  M_duo_GLP1Sec_0(20, 70000, 1)
}

M_SGLT1_GLP1_syn <- function(Vmax, Km, alpha) {
  xVals <- 1:4500
  vol <- 0.0469073209202113
  plot(xVals, hillFunction((xVals / vol), Vmax, Km, alpha),
    main = "M_SGLT1_GLP1_syn",
    type = "l"
  )
  abline(v = 1500) # id high
  abline(v = 105) # id low
  abline(v = 1500) # po 50
  abline(v = 4500) # po 100
}

M_duo_GLP1Sec_0 <- function(Vmax, Km, alpha) {
  xVals <- (1:2200) * 100

  plot(xVals, hillFunction(xVals, Vmax, Km, alpha),
    main = "M_duo_GLP1Sec_0",
    type = "l",
    col = "red",
    ylim = c(0, 20)
  )
  points(xVals, hillFunction(xVals, 20, 75000, 3),
    type = "l"
  )

  points(xVals, xVals / 8500,
    type = "l",
    col = "blue"
  )
  abline(v = 4000) # id low
  abline(v = 43000) # id high
  abline(v = 64000) # po 50
  abline(v = 170000) # po 100
}
