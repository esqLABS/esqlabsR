longText <- function(label) {
  paste(label, "This is a", paste(rep("very", 40), collapse = " "), "long text")
}

boxPlots <- function() {
  list(
    ospsuite.plots::plotBoxWhisker(
      data = mtcars,
      mapping = ggplot2::aes(x = am, y = wt),
      outliers = FALSE
    ),
    ospsuite.plots::plotBoxWhisker(
      data = ToothGrowth,
      mapping = ggplot2::aes(x = supp, y = len)
    )
  )
}

test_that("plotGrid() errors when the argument is not a PlotGridConfiguration", {
  expect_error(
    plotGrid(DataSet$new(name = "DS")),
    "PlotGridConfiguration"
  )
})

test_that("PlotGridConfiguration prints its annotations, arrangement, and tags", {
  plotGridConfiguration <- PlotGridConfiguration$new(boxPlots())
  plotGridConfiguration$title <- "Aciclovir"
  plotGridConfiguration$nColumns <- 2L
  plotGridConfiguration$tagLevels <- "A"

  expect_snapshot(print(plotGridConfiguration))
})

test_that("addPlots() appends a single plot or a list of plots", {
  titled <- function(title) ggplot2::ggplot() + ggplot2::labs(title = title)
  plotGridConfiguration <- PlotGridConfiguration$new()

  plotGridConfiguration$addPlots(titled("IV"))
  plotGridConfiguration$addPlots(list(p1 = titled("Oral"), p2 = titled("IM")))
  plotGridConfiguration$addPlots(list(p3 = titled("SC")))

  expect_equal(
    plotGridConfiguration$plotList,
    list(
      titled("IV"),
      p1 = titled("Oral"),
      p2 = titled("IM"),
      p3 = titled("SC")
    )
  )
})

test_that("plotGrid() applies the tag margin to every panel", {
  plotGridConfiguration <- PlotGridConfiguration$new(boxPlots())
  plotGridConfiguration$tagLevels <- "a"
  plotGridConfiguration$tagMargin <- c(1, 2, 3, 4)

  pGrid <- plotGrid(plotGridConfiguration)

  expect_equal(
    lapply(pGrid$patches$plots, function(p) p$theme$plot.tag$margin),
    list(ggplot2::margin(1, 2, 3, 4, unit = "pt"))
  )
  expect_equal(
    pGrid$theme$plot.tag$margin,
    ggplot2::margin(1, 2, 3, 4, unit = "pt")
  )
})

test_that(".convertAngleToOrientation maps counter-clockwise angles", {
  expect_equal(
    vapply(c(0, 90, 180, 270, -90, 45), .convertAngleToOrientation, ""),
    c(
      "upright",
      "left-rotated",
      "inverted",
      "right-rotated",
      "right-rotated",
      "upright"
    )
  )
})

test_that("plotGrid() renders the grid with its annotations and tags", {
  plotGridConfiguration <- PlotGridConfiguration$new(boxPlots())
  plotGridConfiguration$title <- "my combined plot"
  plotGridConfiguration$subtitle <- "something clever"
  plotGridConfiguration$caption <- "my sources"
  plotGridConfiguration$nColumns <- 2L
  plotGridConfiguration$tagLevels <- "A"
  plotGridConfiguration$tagPrefix <- "Plot ("
  plotGridConfiguration$tagSuffix <- ")"
  plotGridConfiguration$tagColor <- "blue"
  plotGridConfiguration$tagSize <- 15
  plotGridConfiguration$tagAngle <- 45
  plotGridConfiguration$tagPosition <- "top"
  plotGridConfiguration$titleHorizontalJustification <- 0.5
  plotGridConfiguration$subtitleHorizontalJustification <- 0.5

  pGrid <- plotGrid(plotGridConfiguration)

  expect_s3_class(pGrid, "patchwork")
  vdiffr::expect_doppelganger("plotGrid works as expected", pGrid)
})

test_that("plotGrid() wraps long grid annotations and long plot titles", {
  plots <- lapply(boxPlots(), function(p) {
    p +
      .plotTheme(createEsqlabsPlotConfiguration(), p) +
      ggplot2::labs(title = longText("Title:"))
  })
  plotGridConfiguration <- PlotGridConfiguration$new(plots)
  plotGridConfiguration$tagLevels <- "A"
  plotGridConfiguration$title <- longText("Title:")
  plotGridConfiguration$subtitle <- longText("Subtitle:")
  plotGridConfiguration$caption <- longText("Caption:")
  plotGridConfiguration$titleHorizontalJustification <- 0.5

  vdiffr::expect_doppelganger(
    "long labels in plotgrid",
    plotGrid(plotGridConfiguration)
  )
})
