% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/riverwave_plot.R
\name{rw_3d}
\alias{rw_3d}
\title{3D Riverwave Plot}
\usage{
rw_3d(data, value_name, q1, q2, wy_month = 10, ...)
}
\arguments{
\item{data}{A data.frame with date and flow columns.}

\item{value_name}{One unquoted expression for flow column name, e.g. vettenforing_m3_s.}

\item{q1}{Numeric. Peakflow for 1 year recurrence interval.}

\item{q2}{Numeric. Peakflow for 2 year recurrence interval.}

\item{wy_month}{A numeric for what month to use as start of water year, 10 (default).}

\item{...}{Arguments to pass to \link[rgl]{view3d}.}
}
\value{
A 3D riverwave plot.
}
\description{
3D Riverwave Plot
}
