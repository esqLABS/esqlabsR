% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project-configuration.R
\docType{class}
\name{ProjectConfiguration}
\alias{ProjectConfiguration}
\title{ProjectConfiguration}
\description{
An object storing configuration used project-wide
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{projectConfigurationFilePath}}{Path to the file that serve as base
path for other parameters. If NULL, then, other paths should be absolute
paths.}

\item{\code{projectConfigurationDirPath}}{Path to the folder that serve as base
path for other paths. If NULL, then, other paths should be absolute
paths.}

\item{\code{modified}}{Logical indicating whether any configuration properties
have been modified since loading from file. Read-only.}

\item{\code{modelFolder}}{Path to the folder containing pkml simulation files.}

\item{\code{configurationsFolder}}{Path to the folder containing excel files with model
parameterization;}

\item{\code{modelParamsFile}}{Name of the excel file with global model
parameterization.
Must be located in the "configurationsFolder".}

\item{\code{individualsFile}}{Name of the excel file with
individual-specific model parameterization.
Must be located in the "configurationsFolder"}

\item{\code{populationsFile}}{Name of the excel file with population
information.
Must be located in the "configurationsFolder".}

\item{\code{populationsFolder}}{Name of the folder containing population defined through csv files
Must be located in the "configurationsFolder".}

\item{\code{scenariosFile}}{Name of the excel file with scenario
definitions.
Must be located in the "configurationsFolder".}

\item{\code{applicationsFile}}{Name of the excel file scenario-specific
parameters such as application protocol parameters.
Must be located in the "configurationsFolder".}

\item{\code{plotsFile}}{Name of the excel file with plot definitions.
Must be located in the "configurationsFolder".}

\item{\code{dataFolder}}{Path to the folder where experimental data files are
located.}

\item{\code{dataFile}}{Name of the excel file with experimental data.
Must be located in the "dataFolder"}

\item{\code{dataImporterConfigurationFile}}{Name of data importer configuration
file in xml format used to load the data.
Must be located in the "dataFolder"}

\item{\code{outputFolder}}{Path to the folder where the results should be
saved to; relative to the "Code" folder
Initialize}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ProjectConfiguration-new}{\code{ProjectConfiguration$new()}}
\item \href{#method-ProjectConfiguration-print}{\code{ProjectConfiguration$print()}}
\item \href{#method-ProjectConfiguration-save}{\code{ProjectConfiguration$save()}}
\item \href{#method-ProjectConfiguration-clone}{\code{ProjectConfiguration$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectConfiguration-new"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectConfiguration-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectConfiguration$new(projectConfigurationFilePath = character())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{projectConfigurationFilePath}}{A string representing the path to the
project configuration file.
Print}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectConfiguration-print"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectConfiguration-print}{}}}
\subsection{Method \code{print()}}{
print prints a summary of the Project Configuration.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectConfiguration$print(className = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{className}}{Whether to print the name of the class at the beginning. default to TRUE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectConfiguration-save"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectConfiguration-save}{}}}
\subsection{Method \code{save()}}{
Export ProjectConfiguration object to ProjectConfiguration.xlsx
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectConfiguration$save(path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{a string representing the path or file name where to save the file. Can be absolute or relative (to working directory).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectConfiguration-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectConfiguration-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectConfiguration$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
