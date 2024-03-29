% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-individual.R
\name{applyIndividualParameters}
\alias{applyIndividualParameters}
\title{Apply an individual to the simulation.
For human species, only parameters that do not override formulas are applied.
For other species, all parameters returned by \code{createIndividual} are applied.}
\usage{
applyIndividualParameters(individualCharacteristics, simulation)
}
\arguments{
\item{individualCharacteristics}{\code{IndividualCharacteristics} describing an individual. Optional}

\item{simulation}{\code{Simulation} loaded from the PKML file}
}
\description{
Apply an individual to the simulation.
For human species, only parameters that do not override formulas are applied.
For other species, all parameters returned by \code{createIndividual} are applied.
}
\examples{
\dontrun{
simulation <- loadSimulation(filePath = modelPath)
humanIndividualCharacteristics <- createIndividualCharacteristics(
  species = Species$Human, population = HumanPopulation$European_ICRP_2002,
  gender = Gender$Male, weight = 70
)
applyIndividualParameters(humanIndividualCharacteristics, simulation)
}
}
