% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flow_addons.R
\name{FF_LogNormal}
\alias{FF_LogNormal}
\title{Flood frequency calculation with lognormal}
\usage{
FF_LogNormal(m, s, p)
}
\arguments{
\item{m}{mean of lognormal estimate}

\item{s}{standard deviation of lognormal}

\item{p}{return interval value}
}
\value{
A probability
}
\description{
calculates flood frequency from lognormal districution params
}
\note{
The user must enter p as a single value
}
