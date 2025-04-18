% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-data.R
\name{loadObservedData}
\alias{loadObservedData}
\title{Load data from excel}
\usage{
loadObservedData(
  projectConfiguration,
  sheets = NULL,
  importerConfiguration = NULL
)
}
\arguments{
\item{projectConfiguration}{Object of class \code{ProjectConfiguration} containing
the necessary information.}

\item{sheets}{String or a list of strings defining which sheets to load.
If \code{NULL} (default), all sheets within the file are loaded.}

\item{importerConfiguration}{\code{DataImporterConfiguration} object used to load
the data. If \code{NULL} (default), default esqlabs importer configuration as
defined in \code{projectConfiguration$dataImporterConfigurationFile} will be used.}
}
\value{
A named list of \code{DataSet} objects, with names being the names of the data sets.
}
\description{
Loads data sets from excel. The excel file containing the data
must be located in the folder \code{projectConfiguration$dataFolder}
and be named \code{projectConfiguration$dataFile}.
Importer configuration file must be located in the same folder and named
\code{projectConfiguration$dataImporterConfigurationFile}.
}
\examples{
\dontrun{
# Create default project configuration
projectConfiguration <- createProjectConfiguration()
dataSets <- loadObservedData(projectConfiguration)
}
}
