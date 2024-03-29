% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-transfer-functions.R
\name{hillFunction}
\alias{hillFunction}
\title{Hill equation, transforming a value of an effector (e.g. concentration) to a
saturable function value.}
\usage{
hillFunction(x, Vmax, Km, alpha = 1)
}
\arguments{
\item{x}{Value of the effector.}

\item{Vmax}{Maximal value of the function.}

\item{Km}{Value of the effector at which the value of the function is half-maximal.}

\item{alpha}{Hill coefficient defining the steepness of the function. Default is \code{1}.}
}
\value{
Vmax * x^alpha / (x^alpha + Km^alpha)
}
\description{
Hill equation, transforming a value of an effector (e.g. concentration) to a
saturable function value.
}
