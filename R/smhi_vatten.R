#' SMHI Gauge Inventory
#'
#' @return dataframe of gauge inventory
#'
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
#'
#' @examples {
#'
#' ellinge_vatten <- smhi_vatten(1132)
#'
#' }
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



#' Get all data from SMHI Vatten
#'
#' @param stn_nos vector of Numeric. Station Number.
#'
#' @return A data.frame
#' @export
#'
#' @examples {
#'
#' ellinge_vatten <- smhi_vatten(1132)
#'
#' }
#'
smhi_vatten <- function(stn_nos){
    stn_data <- sapply(stn_nos, smhi_vatten_data)
    return(stn_data)
}

# stns <- test[1:10,2] %>% st_drop_geometry()
all_data <- list()
for (i in 1:5) {
    inv_df <- inv %>% st_drop_geometry()
    temp <- smhi_vatten_data(inv_df[i,2])
    all_data[i]
}


