% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colours_pmcharacterr.R
\name{colours_pmcharacterr}
\alias{colours_pmcharacterr}
\title{Function to generate a custom colour palette for particulate matter plots.}
\usage{
colours_pmcharacterr(
  format = c("character", "palette", "tibble", "data.frame", "df"),
  names = TRUE
)
}
\arguments{
\item{format}{What type of object should be returned?}

\item{names}{When \code{format} is \code{character}, should the character 
vector be named?}
}
\value{
Named vector, vector, named colour palette, or a tibble.
}
\description{
Function to generate a custom colour palette for particulate matter plots.
}
\examples{

colours_pmcharacterr()
colours_pmcharacterr(format = "palette")
colours_pmcharacterr(format = "tibble")

}
\author{
Stuart K. Grange
}
