#' Raster Hydrograph
#'
#' @param data A data.frame with date and flow columns.
#' @param value_name The variable to include as the y-axis.
#' @return A ggplot.
#' @importFrom dplyr "%>%"
#' @export
#'
#' @note The `date` column must be named `date` and is of origin "%Y-%m-%d".
#'
#' @examples \dontrun{

#' #provide a station that you want to look at visually (we need a date and flow column)
#' site_10 <- smhiDataClean[['10']]
#'
#' raster_hydrograph(data = site_10, value_name = vattenforing_m3_s)
#'
#' }
#'
raster_hydrograph <- function(data, value_name) {

    if (!requireNamespace("ggfx")) {

     tile <- ggplot2::geom_tile(ggplot2::aes(fill = {{value_name}}))

    } else {

     tile <- ggfx::with_outer_glow(ggplot2::geom_tile(ggplot2::aes(fill = {{value_name}})))

    }


    # prep for plotting
    data <- data %>% prep_flow()

    xbreaks <- data %>%
        dplyr::group_by(month) %>%
        dplyr::slice_min(day, with_ties = F) %>%
        dplyr::pull(wy_doy)

    dup_labels <- function(x) data$month_abb[match(x, data$wy_doy)]

    data %>%
        ggplot2::ggplot(ggplot2::aes(wy_doy, wy)) +
        tile +
        ggplot2::scale_y_continuous( breaks = seq(min(data$wy),
                                                  max(data$wy),
                                                  by = 10),
                                     name = 'Water Year') +
        ggplot2::scale_x_continuous(breaks = xbreaks,
                                    name = 'Day of Year',
                                    sec.axis = ggplot2::dup_axis(labels = dup_labels, name = NULL),
                                    expand = c(0,0)) +
        ggplot2::scale_fill_gradientn(colors = hcl.colors(11, palette = 'Spectral'),
                                      labels = scales::comma,
                                      name = 'Discharge',
                                      trans = 'log') +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
                       panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_blank(),
                       panel.background = ggplot2::element_rect(fill = NA),
                       panel.ontop = T,
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.text.x.bottom = ggplot2::element_text(vjust = 5.5,face = 'bold'),
                       axis.text.x.top = ggplot2::element_text(hjust = -.45, vjust = -5.5, face = 'bold'))
}


#' Latitude Rastergraph
#'
#' @param data
#'
#' @return
#' @export
#'
#' @note Very specific right now with what goes in because we need to join the station metadata latitudes.
latitude_rastergraph <- function(data) {

    getDailyAverage <- function(data) {
                       data %>%
                       dplyr::group_by(wy_doy) %>%
                       dplyr::mutate(mean_prop = mean(vattenforing_m3_s_max_prop, na.rm = TRUE)) %>%
                       dplyr::slice(1) %>%
                       dplyr::ungroup()
    }

    daily_average <- data %>%
                     purrr::map(~prep_flow(.)) %>%
                     purrr::map(~getDailyAverage(.))

    da_names <- names(daily_average)

    daily_average <- purrr::map2(daily_average, da_names, ~.x %>% mutate(station_number = as.numeric(.y)))

    stations_unfiltered <- readRDS('data/entire_inventory_unfiltered.rds')

    final_df <- daily_average %>%
                dplyr::bind_rows() %>%
                dplyr::left_join(stations_unfiltered %>%
                                 as.data.frame() %>%
                                 dplyr::select(lat, Stnno) %>%
                                 dplyr::mutate(station_number = dplyr::row_number()),
                                 by = 'station_number') %>%
                dplyr::arrange(lat)

    final_df_levels <- final_df %>%
                dplyr::group_by(Stnno) %>%
                dplyr::slice(1) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(lat) %>%
                dplyr::mutate(id = dplyr::row_number()) %>%
                dplyr::select(id, Stnno)

    final_df <- final_df %>% left_join(final_df_levels, by = 'Stnno')

    xbreaks <- final_df %>%
        dplyr::group_by(month) %>%
        dplyr::slice_min(day, with_ties = F) %>%
        dplyr::pull(wy_doy)

    dup_labels <- function(x) final_df$month_abb[match(x, final_df$wy_doy)]

    y_labels <- function(x) final_df$lat[match(x, final_df$id)]

    if (!requireNamespace("ggfx")) {

        tile <- ggplot2::geom_tile(ggplot2::aes(fill = mean_prop))

    } else {

        tile <- ggfx::with_outer_glow(ggplot2::geom_tile(ggplot2::aes(fill = mean_prop)))

    }

    final_df %>%
        ggplot2::ggplot(ggplot2::aes(wy_doy, id)) +
        tile +
        ggplot2::scale_y_continuous(breaks = seq(min(final_df$id), max(final_df$id), length.out = 5),
                                     labels = y_labels,
                                     name = 'Latitude') +
        ggplot2::scale_x_continuous(breaks = xbreaks,
                                    name = 'Day of Year',
                                    sec.axis = ggplot2::dup_axis(labels = dup_labels, name = NULL),
                                    expand = c(0,0)) +
        ggplot2::scale_fill_gradientn(colors = hcl.colors(11, palette = 'Spectral'),
                                      labels = scales::comma,
                                      name = 'Proportion to Max Flow',
                                      trans = 'log') +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
                       panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_blank(),
                       panel.background = ggplot2::element_rect(fill = NA),
                       panel.ontop = T,
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.text.x.bottom = ggplot2::element_text(vjust = 5.5,face = 'bold'),
                       axis.text.x.top = ggplot2::element_text(hjust = -.45, vjust = -5.5, face = 'bold'))


}
