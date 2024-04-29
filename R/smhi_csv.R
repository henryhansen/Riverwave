#' Download Swedish Meteorological and Hydrological Institute as csv
#'
#' Downloads a csv of gauge data for a specific gauge station
#'
#' @param stationID ID of gauge station
#' @param direc Directory where data will be stored
#' @param version Version of the data downloaded - latest
#' @param parameter Sets Parameter value for API call
#' @param period Periods for the data - latest-hour, latest-day, latest-months or corrected-archive
#'
#' @return a .csv file if directory is submitted otherwise returns a dataframe
#' @export
#'
#' @examples
#'
#' # # Pull a Single CSV -------------------------------------------------------
#'
#' directory <- getwd()
#'
#' # exports the csv to a specific place
#' \dontrun{smhi_csv(stationID = 274, direc = directory)}
#'
#' # returns the csv as an object when specified
#' test <- smhi_csv(stationID = 274)
#'
#' # Pull Multiple CSVs ------------------------------------------------------
#'
#' directory <- getwd()
#'
#' IDs <- c(1015, 1019, 2154)
#'
#' multi <- lapply(IDs, smhi_csv)
smhi_csv <- function(stationID, direc = NA, version = "latest", parameter = "1", period = "corrected-archive") {
  # API information: https://opendata.smhi.se/apidocs/hydroobs/index.html

  base_URL <- "https://opendata-download-hydroobs.smhi.se/api" # base url for api request
  call <- paste0(
    base_URL, # concatenated string to pull unique csv
    "/version/", version, # version part of string
    "/parameter/", parameter, # parameter part of string
    "/station/", stationID, # station part of string
    "/period/", period, # period part to string
    "/data.csv"
  )

  print(call)

  csv <- RCurl::getURL(call) # call to get the csv

  # grab the station details
  station <- utils::read.csv(
    text = csv,
    sep = ";", # specify separator
    header = T, # no header
    nrows = 1, # one row
    as.is = T
  ) # as is true

  # grab the station name and ID
  name_ID <- paste0(station[1], "_", station[2])

  print(name_ID)

  # grab the headers of the data
  headers <- utils::read.csv(
    text = csv,
    skip = 6, # skip meta data
    sep = ";", # specify separator
    header = F, # no header
    nrows = 1, # one row
    as.is = T
  ) # as is true

  # grab the full dataset
  data <- utils::read.csv(
    text = csv,
    sep = ";", # specify separator
    skip = 7, # skip meta data
    header = F, # no header
    colClasses = c(
      "character", # read date data as character
      "numeric", # reads velocity data numeric
      "character", # reads quality data as charater
      "NULL", # drops remaining colunmns
      "NULL",
      "NULL"
    )
  )
  # add headers to data
  colnames(data) <- headers[1:3]

  # export data
  if (!is.na(direc)) {
    utils::write.csv(x = data, file = paste0(direc, "/", name_ID[1], ".csv"))
    return(name_ID)
  } else {
    return(data)
  }
}


