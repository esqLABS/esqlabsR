# ExportConfiguration

R6 class extending
[tlf::ExportConfiguration](https://rdrr.io/pkg/tlf/man/ExportConfiguration.html)
with row-aware height.

## Value

The file name of the exported plot

## Details

Inherits fields and behavior from
[tlf::ExportConfiguration](https://rdrr.io/pkg/tlf/man/ExportConfiguration.html).

## Inherited fields

See
[tlf::ExportConfiguration](https://rdrr.io/pkg/tlf/man/ExportConfiguration.html)
for `name`, `path`, `format`, `width`, `height`, `units`, and `dpi`.

## Super class

[`tlf::ExportConfiguration`](https://rdrr.io/pkg/tlf/man/ExportConfiguration.html)
-\> `ExportConfiguration`

## Active bindings

- `heightPerRow`:

  The height of the plot dimensions for a row in a multi panel plot. The
  final height of the figure will be 'heightPerRow' times the number of
  rows. If `NULL` (default), value used in `height` is used. If not
  `NULL`, this value always overrides the `height` property.

## Methods

### Public methods

- [`ExportConfiguration$savePlot()`](#method-ExportConfiguration-savePlot)

- [`ExportConfiguration$clone()`](#method-ExportConfiguration-clone)

Inherited methods

- [`tlf::ExportConfiguration$convertPixels()`](https://esqlabs.github.io/esqlabsR/tlf/html/ExportConfiguration.html#method-ExportConfiguration-convertPixels)
- [`tlf::ExportConfiguration$getFileName()`](https://esqlabs.github.io/esqlabsR/tlf/html/ExportConfiguration.html#method-ExportConfiguration-getFileName)
- [`tlf::ExportConfiguration$initialize()`](https://esqlabs.github.io/esqlabsR/tlf/html/ExportConfiguration.html#method-ExportConfiguration-initialize)
- [`tlf::ExportConfiguration$print()`](https://esqlabs.github.io/esqlabsR/tlf/html/ExportConfiguration.html#method-ExportConfiguration-print)

------------------------------------------------------------------------

### Method [`savePlot()`](https://rdrr.io/r/grDevices/savePlot.html)

Save/Export a plot

#### Usage

    ExportConfiguration$savePlot(plotObject, fileName = NULL)

#### Arguments

- `plotObject`:

  A `ggplot` object

- `fileName`:

  character file name of the exported plot

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ExportConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
