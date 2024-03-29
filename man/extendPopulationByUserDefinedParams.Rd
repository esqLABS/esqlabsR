% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-population.R
\name{extendPopulationByUserDefinedParams}
\alias{extendPopulationByUserDefinedParams}
\title{Add user defined variability on parameters to a population.}
\usage{
extendPopulationByUserDefinedParams(
  population,
  parameterPaths,
  meanValues,
  sdValues,
  distributions = Distributions$Normal
)
}
\arguments{
\item{population}{Object of type \code{Population}}

\item{parameterPaths}{Vector of parameter path for which the variability is to be added.}

\item{meanValues}{Vector of mean values of the parameters. Must have the same
length as \code{parameterPaths}. The type of mean (arithmetic, geometric)
depends on the selected \code{distribution}. The values must be in the base
units of the parameters.}

\item{sdValues}{Vector of standard deviation values of the parameters. Must
have the same length as \code{parameterPaths}. The type of standard deviation
depends on the selected \code{distribution}.}

\item{distributions}{Type of distribution from which the random values will
be sampled. Must have the same length as \code{parameterPaths}.
A list of supported distributions is defined in \code{Distributions}. Default is \code{"Normal"}.}
}
\description{
Add user defined variability on parameters to a population.
}
