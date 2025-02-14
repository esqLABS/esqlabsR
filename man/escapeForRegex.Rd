% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{escapeForRegex}
\alias{escapeForRegex}
\title{Escape a string for possible regular expression match}
\usage{
escapeForRegex(string)
}
\arguments{
\item{string}{String to be escaped}
}
\value{
string with prefix "\\Q" and suffix "\\E" appended.
The resulting string will not be recognized as a regular expression pattern.
}
\description{
Escape a string for possible regular expression match
}
