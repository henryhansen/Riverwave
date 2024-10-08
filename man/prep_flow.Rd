% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flow_addons.R
\name{prep_flow}
\alias{prep_flow}
\title{Prepping Flow Data}
\usage{
prep_flow(data, value_name = vattenforing_m3_s, wy_month = 10)
}
\arguments{
\item{data}{A data.frame with date and flow columns.}

\item{value_name}{One unquoted expression for flow column name, e.g. vettenforing_m3_s.}

\item{wy_month}{A numeric for what month to use as start of water year, 10 (default).}
}
\value{
A \code{tibble} with original data and added date columns.
}
\description{
Prepping Flow Data
}
\note{
In data.frame or \code{tibble} `date` column must be named `date`.
}
\examples{
stream_flow <- data.frame(flow = c(seq(30, 60), seq(60, 30, length.out = 60)),
                          date = seq(as.Date('2012-01-01'), by = "day", length.out = 91))

stream_flow_prepped <- prep_flow(stream_flow, value = flow, wy_month = 10)







}
