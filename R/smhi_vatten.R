#' SMHI Gauge Inventory
#'
#' @return dataframe of gauge inventory
#' @importFrom magrittr "%>%"
#' @export
#'
#'
#'

smhi_vatten_inv <- function(){

    base_url <- 'https://vattenweb.smhi.se/station/rest/'

    #get json
    error <- httr::GET(url = base_url,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "smhi.json"),
                                        overwrite = TRUE))
    #read json
    properties <- jsonify::from_json(file.path(tempdir(),
                                               "smhi.json"))
    #convert to geojson and then sf
    properties_geo <- geojsonio::geojson_sf(geojsonio::as.json(properties)) %>%
        dplyr::mutate(dplyr::across(c('Type', 'Variables'), ~as.character(.x))) %>%
        dplyr::filter(Variables %in% 'Q')

    return(properties_geo)
}


#' SMHI Vatten Map
#' @param smhi_vatten_inv dataframe of gauge inventory from smhi_vatten_inv
#' @return A leaflet map.
#' @export
#'
smhi_vatten_map <- function(smhi_vatten_inv){

mapview::mapview(smhi_vatten_inv)

}
## now get data for stations


#' Get SMHI Vatten
#'
#' @param stn_no Numeric. Station Number.
#'
#' @return A data.frame
#' @export
#' @examples
#'
smhi_vatten_data <- function(stn_no){

base_url <- 'https://vattenweb.smhi.se/station/rest/report/'

data_url <- paste0(base_url, stn_no)
#get xls by station number
error <- httr::GET(url = data_url,
                   httr::write_disk(path = file.path(tempdir(),
                                                     "smhi_data.xls"),
                                    overwrite = TRUE))

data <- suppressMessages(readxl::read_xls(file.path(tempdir(),
                                           "smhi_data.xls"),
                         sheet = 1,
                         skip = 13))
column_names <- c('date','vattenforing_m3_s','datakontroll_vattenforing')
for(i in column_names){

  colnames(data)[which(column_names %in% i)]  <- i

}

data <- data %>% dplyr::mutate(date = lubridate::as_date(date))

}


#' Title
#'
#' @param inv dataframe of smhi inventory
#'
#' @return station numbers currently monitored
#' @export
#' @examples
#'
smhi_vatten_current <- function(inv) {
    latest <- Sys.Date()-1
    index <- which(inv$DataTo >= latest)
    return(sf::st_drop_geometry(inv[index,"Stnno"]))
}

#' Get all data from SMHI Vatten
#'
#' @param stn_nos vector of Numeric. Station Number.
#'
#' @return A data.frame
#' @export
#' @examples
#'
smhi_vatten_retrieve <- function(stn_nos){
    stn_data <- apply(stn_nos, 1, FUN = smhi_vatten_data)
    return(stn_data)
}


#' SMHI Natural Model
#'
#' @param stn_no Numeric. Station Number.
#' @return dataframe of natural modeled data from 2010-10-01 to 2023-09-30.
#' @importFrom magrittr "%>%"
#' @export
#'
#'
#'

smhi_vatten_natural <- function(stn_no){

    base_url <- 'https://vattenwebb.smhi.se/regulations/rest/basin?subid='

    data_url <- paste0(base_url, stn_no)
    #get json
    error <- httr::GET(url = data_url,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "smhi.json"),
                                        overwrite = TRUE))
    #read json
    properties <- jsonify::from_json(file.path(tempdir(),
                                               "smhi.json"))
    #convert to geojson and then sf
    properties_natural <- data.frame(properties$timeseries$natural)%>%
                          dplyr::mutate(date = seq(as.Date('2010-10-01'), as.Date('2023-09-30'), 1))

    return(properties_natural)
}


