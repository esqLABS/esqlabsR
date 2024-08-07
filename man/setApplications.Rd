% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-scenario-configuration.R
\name{setApplications}
\alias{setApplications}
\title{Set an application protocol in a \code{Simulation} from the excel file.}
\usage{
setApplications(simulation, scenarioConfiguration)
}
\arguments{
\item{simulation}{A \code{Simulation} object that will be modified.}

\item{scenarioConfiguration}{A \code{ScenarioConfiguration} object holding the
name of the application protocol.

This function is deprecated. Use \code{setParametersFromXLS} instead.}
}
\description{
Set an application protocol in a \code{Simulation} from the excel file.
}
\details{
Set the parameter values describing the application protocol
defined in the scenario configuration.
}
