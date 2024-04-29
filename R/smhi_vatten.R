#' SMHI Gauge Inventory
#'
#' @return dataframe of gauge inventory
#' @export
#'
#'
#'

smhi_vatten_inv <- function(){

    base_url <- 'https://opendata-download-hydroobs.smhi.se/api/version/latest/parameter/1.json'

    #get json
    error <- httr::GET(url = base_url,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "smhi.json"),
                                        overwrite = TRUE))
    #read json
    properties <- jsonify::from_json(file.path(tempdir(),
                                               "smhi.json"))

    properties <- properties$station %>%
                    dplyr::tibble() %>%
                    dplyr::select(-link) %>%
                    dplyr::filter(latitude != 0)
    #convert to geojson and then sf
    properties_geo <- sf::st_as_sf(properties, coords = c('longitude', 'latitude'), crs = 4326)

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


#' SMHI Natural Model
#'
#' @param stn_no subid Numeric. Station Number.
#' @return dataframe of natural modeled data from 2010-10-01 to 2023-09-30.
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
                          dplyr::mutate(date = seq(as.Date('2010-01-01'), by = 1, length.out = nrow(.)))

    return(properties_natural)
}



#' SMHI Natural Model (spatial)
#'
#' @param point A sf object.
#' @return dataframe of natural modeled data from 2010-10-01 to 2023-09-30.
#' @export
#'
#'
#'

smhi_vatten_natural_pt <- function(point){

    point <- point %>% sf::st_transform(3006)

    x <- point$geometry[[1]][[1]]
    y <- point$geometry[[1]][[2]]

    base_url <- paste0('https://vattenwebb.smhi.se/regulations/rest/point?x=',x,'&y=',y)

    #get json
    error <- httr::GET(url = base_url,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "smhi.json"),
                                        overwrite = TRUE))
    #read json
    properties <- jsonify::from_json(file.path(tempdir(),
                                               "smhi.json"))
    #convert to geojson and then sf
    properties_natural <- data.frame(properties$timeseries$natural) %>%
        dplyr::mutate(date = seq(as.Date('2010-01-01'), by = 1, length.out = nrow(.)))

    return(properties_natural)
}


