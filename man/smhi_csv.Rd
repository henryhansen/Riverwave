% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smhi_csv.R
\name{smhi_csv}
\alias{smhi_csv}
\title{Download Swedish Meteorological and Hydrological Institute as csv}
\usage{
smhi_csv(
  stationID,
  direc = NA,
  version = "latest",
  parameter = "1",
  period = "corrected-archive"
)
}
\arguments{
\item{stationID}{ID of gauge station}

\item{direc}{Directory where data will be stored}

\item{version}{Version of the data downloaded - latest}

\item{parameter}{Sets Parameter value for API call}

\item{period}{Periods for the data - latest-hour, latest-day, latest-months or corrected-archive}
}
\value{
a .csv file if directory is submitted otherwise returns a dataframe
}
\description{
Downloads a csv of gauge data for a specific gauge station
}
\examples{

# # Pull a Single CSV -------------------------------------------------------

directory <- getwd()

# exports the csv to a specific place
\dontrun{smhi_csv(stationID = 274, direc = directory)}

# returns the csv as an object when specified
test <- smhi_csv(stationID = 274)

# Pull Multiple CSVs ------------------------------------------------------

directory <- getwd()

IDs <- c(1015, 1019, 2154)

multi <- lapply(IDs, smhi_csv)
}
