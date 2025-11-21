# Create an instance of `DefaultPlotConfiguration` R6 class

An instance of `DefaultPlotConfiguration` R6 class from `{tlf}` package
is needed for creating visualizations with the `{ospsuite}` package.

The default attributes of the class are chosen to reflect the corporate
standards adopted by esqLABS GmbH.

## Usage

``` r
createEsqlabsPlotConfiguration()
```

## Value

An instance of `DefaultPlotConfiguration` R6 class.

## See also

Other create-plotting-configurations:
[`createEsqlabsExportConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/createEsqlabsExportConfiguration.md),
[`createEsqlabsPlotGridConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/createEsqlabsPlotGridConfiguration.md)

## Examples

``` r
createEsqlabsPlotConfiguration()
#> <DefaultPlotConfiguration>
#>   Public:
#>     caption: NULL
#>     captionAlign: right
#>     captionAngle: 0
#>     captionColor: black
#>     captionFontFace: plain
#>     captionFontFamily: 
#>     captionMargin: 2 2 5 2
#>     captionSize: 8
#>     clone: function (deep = FALSE) 
#>     displayLLOQ: TRUE
#>     errorbarsAlpha: 0.75
#>     errorbarsCapSize: 2.75
#>     errorbarsLinetype: solid
#>     errorbarsSize: 0.65
#>     foldLinesLegend: FALSE
#>     foldLinesLegendDiagonal: FALSE
#>     legendBackgroundAlpha: 0
#>     legendBackgroundColor: white
#>     legendBorderColor: NULL
#>     legendBorderSize: NULL
#>     legendBorderType: 1
#>     legendKeysAlign: left
#>     legendKeysAngle: 0
#>     legendKeysColor: black
#>     legendKeysFontFace: plain
#>     legendKeysFontFamily: 
#>     legendKeysMargin: 2 0 2 0
#>     legendKeysSize: 6
#>     legendPosition: outsideTopLeft
#>     legendTitle: NULL
#>     legendTitleAlign: left
#>     legendTitleAngle: 0
#>     legendTitleColor: black
#>     legendTitleFontFace: plain
#>     legendTitleFontFamily: 
#>     legendTitleMargin: 2 2 2 2
#>     legendTitleSize: 10
#>     linesAlpha: 0.75
#>     linesColor: #4ABDCB #EA5E5E #76BB60 #e6a489 #8ac8b9 #9f8fcd #74a778  ...
#>     linesLinetype: NULL
#>     linesSize: 0.5
#>     lloqDirection: NULL
#>     plotBackgroundColor: black
#>     plotBackgroundFill: white
#>     plotBackgroundLinetype: blank
#>     plotBackgroundSize: 0.5
#>     plotPanelBackgroundColor: black
#>     plotPanelBackgroundFill: white
#>     plotPanelBackgroundLinetype: solid
#>     plotPanelBackgroundSize: 0.5
#>     pointsAlpha: 0.75
#>     pointsColor: #4ABDCB #EA5E5E #76BB60 #e6a489 #8ac8b9 #9f8fcd #74a778  ...
#>     pointsShape: circle diamond triangle square invertedTriangle cross th ...
#>     pointsSize: 1.75
#>     ribbonsAlpha: 0.5
#>     ribbonsFill: #4ABDCB #EA5E5E #76BB60 #e6a489 #8ac8b9 #9f8fcd #74a778  ...
#>     ribbonsLinetype: solid longdash dotted dashed twodash dotdash blank
#>     ribbonsSize: 1
#>     subtitle: NULL
#>     subtitleAlign: left
#>     subtitleAngle: 0
#>     subtitleColor: black
#>     subtitleFontFace: plain
#>     subtitleFontFamily: 
#>     subtitleMargin: 0 2 10 2
#>     subtitleSize: 10
#>     title: NULL
#>     titleAlign: left
#>     titleAngle: 0
#>     titleColor: black
#>     titleFontFace: plain
#>     titleFontFamily: 
#>     titleMargin: 20 2 10 2
#>     titleSize: 10
#>     watermark: NULL
#>     watermarkAlign: center
#>     watermarkAngle: 30
#>     watermarkColor: grey40
#>     watermarkFontFace: plain
#>     watermarkFontFamily: 
#>     watermarkMargin: 1 1 1 1
#>     watermarkSize: 20
#>     xAxisColor: black
#>     xAxisExpand: FALSE
#>     xAxisLabelTicksAlign: center
#>     xAxisLabelTicksAngle: 0
#>     xAxisLabelTicksColor: black
#>     xAxisLabelTicksFontFace: plain
#>     xAxisLabelTicksFontFamily: 
#>     xAxisLabelTicksMargin: 2 2 2 2
#>     xAxisLabelTicksSize: 8
#>     xAxisLimits: NULL
#>     xAxisLinetype: solid
#>     xAxisScale: NULL
#>     xAxisSize: 0.5
#>     xAxisTicks: NULL
#>     xAxisTicksLabels: identity
#>     xGridColor: grey
#>     xGridLinetype: blank
#>     xGridSize: 0.25
#>     xLabel: NULL
#>     xLabelAlign: center
#>     xLabelAngle: 0
#>     xLabelColor: black
#>     xLabelFontFace: plain
#>     xLabelFontFamily: 
#>     xLabelMargin: 10 0 0 0
#>     xLabelSize: 9
#>     xUnit: NULL
#>     xValuesLimits: NULL
#>     yAxisColor: black
#>     yAxisExpand: FALSE
#>     yAxisLabelTicksAlign: center
#>     yAxisLabelTicksAngle: 0
#>     yAxisLabelTicksColor: black
#>     yAxisLabelTicksFontFace: plain
#>     yAxisLabelTicksFontFamily: 
#>     yAxisLabelTicksMargin: 2 2 2 2
#>     yAxisLabelTicksSize: 8
#>     yAxisLimits: NULL
#>     yAxisLinetype: solid
#>     yAxisScale: NULL
#>     yAxisSize: 0.5
#>     yAxisTicks: NULL
#>     yAxisTicksLabels: identity
#>     yGridColor: grey
#>     yGridLinetype: blank
#>     yGridSize: 0.25
#>     yLabel: NULL
#>     yLabelAlign: center
#>     yLabelAngle: 90
#>     yLabelColor: black
#>     yLabelFontFace: plain
#>     yLabelFontFamily: 
#>     yLabelMargin: 0 0 10 0
#>     yLabelSize: 9
#>     yUnit: NULL
#>     yValuesLimits: NULL
```
