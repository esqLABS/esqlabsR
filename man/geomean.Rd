% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{geomean}
\alias{geomean}
\title{Calculate geometric mean of a numeric vector}
\usage{
geomean(x, na.rm = FALSE, trim = 0)
}
\arguments{
\item{x}{Numeric array to calculate geometric mean for}

\item{na.rm}{A logical value indicating whether \code{NA} values should be
stripped before the computation proceeds}

\item{trim}{Fraction (0 to 0.5) of observations to be trimmed from each
end of \code{x} before the mean is computed. Values of trim outside that range
are taken as the nearest endpoint}
}
\value{
Geometric mean of \code{x}
}
\description{
Calculate geometric mean of a numeric vector
}
