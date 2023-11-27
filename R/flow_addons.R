
#' Richards-Baker Flashiness index (RBI)
#' @description Adds RBI of observation (flow).
#'
#' @param data A daily value data.frame.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s
#' @param ... values to pass to `dplyr::group_by()` to perform RBI calculation on. (optional)
#'
#' @return A rbi column within data.frame.
#' @note The user must specify the `flow_var` column but it's optional whether to use `dplyr::group_by()` or not.
#' @export
#'
#' @examples
#' stream_flow <- data.frame(flow = c(seq(30, 60), seq(60, 30, length.out = 60)))
#' stream_flow %>% get_rbi(flow)
#'
get_rbi <- function(data, value_name, ...) {

    data %>%
        dplyr::mutate(lagged_flow = dplyr::lag({{value_name}})) %>%
        dplyr::group_by(...) %>%
        dplyr::transmute(rbi = sum(abs({{value_name}}-lagged_flow), na.rm = T)/sum({{value_name}}, na.rm = T)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()

}

#' Flood frequency calculation with lognormal
#' @description calculates flood frequency from lognormal districution params
#'
#' @param m mean of lognormal estimate
#' @param s standard deviation of lognormal
#' @param p return interval value
#'
#' @return A probability
#' @note The user must enter p as a single value
#' @export
#'
FF_LogNormal <- function(m, s, p) {
    #calculate coefficient of variation
    cv <- s/m

    #calculate standard normal deviate
    z <- qnorm(1 - p)

    #calculate frequency factor for lognormal distribution
    kt <- (1/cv) * (exp(sqrt(log(1 + cv^2)) * z - 0.5 * log(1 + cv^2)) - 1)

    #calculate 100-year flood
    qvalue <- m + kt * s

    #return q100
    return(qvalue)
}

#' Prepping Flow Data
#'
#' @param data A data.frame with date and flow columns
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#'
#' @return
#' @export
#'
#' @note In data.frame `date` column must be named `date`.
#' @examples
prep_flow <- function(data, value_name = vattenforing_m3_s, wy_month = 10){

    leap_years <- c(seq(1832,by = 4, length.out = 2000))

    data %>%
        dplyr::mutate(date = lubridate::as_date(date),
               year = lubridate::year(date),
               month = lubridate::month(date),
               day = lubridate::day(date),
               doy=lubridate::yday(date),
               wy_doy = ifelse(!(year %in% leap_years),ifelse(doy >= month_to_doy(wy_month, leap = F),
                                                              doy-month_to_doy(wy_month, leap = F)+1,
                                                              (365-month_to_doy(wy_month, leap = F)+1+doy)),
                               ifelse(doy >= month_to_doy(wy_month, leap = T),
                                      doy-month_to_doy(wy_month, leap = T)+1,
                                      (366-month_to_doy(wy_month, leap = T)+1+doy))),
               month_day = str_c(month, day, sep = "-"),
               wy = waterYear(date, wy_month, TRUE),
               month_abb = factor(month.abb[month], levels = month.abb),
               month_day = str_c(month, day, sep = "-")) %>%
        add_proportion(value_name = {{value_name}})

}


#'Water Year These functions are hi-jacked from smwrBase package.
#'
#'Create an ordered factor or numeric values from a vector of dates based on
#'the water year.
#' @noRd
#' @param x an object of class "Date" or "POSIXt." Missing values are permitted and
#'result in corresponding missing values in the output.
#' @param wy_month A numeric indicating the month the water year begins.
#' @param numeric a logical value that indicates whether the returned values
#'should be numeric \code{TRUE} or an ordered factor \code{FALSE}. The default
#'value is \code{FALSE}.
#' @return An ordered factor or numeric vector corresponding to the water year.
#' @note The water year is defined as the period from October 1 to September 30.
#'The water year is designated by the calendar year in which it ends. Thus, the
#'year ending September 30, 1999, is the "1999 water year."
#' @seealso
#Flip for production/manual
#'\code{\link[lubridate]{year}}
#\code{year} (in lubridate package)

waterYear <- function(x, wy_month = 10, numeric=FALSE) {
    ## Coding history:
    ##    2005Jul14 DLLorenz Initial dated verion
    ##    2010Feb17 DLLorenz Added option to return numerics
    ##    2011Jun07 DLLorenz Conversion to R
    ##    2012Aug11 DLLorenz Integer fixes
    ##    2013Feb15 DLLorenz Prep for gitHub
    ##
    x <- as.POSIXlt(x)
    yr <- x$year + 1900L
    mn <- x$mon + 1L
    ## adjust for water year
    yr <- yr + ifelse(mn < as.integer(wy_month), 0L, 1L)
    if(numeric)
        return(yr)
    ordered(yr)
}

#' water year to months
#' @description Change wy_month to doy.
#' @param wy_month A numeric
#' @param leap Logical
#' @return A numeric value
#' @noRd
month_to_doy <- function(wy_month, leap = FALSE) {


    ifelse(isTRUE(leap),
           dplyr::case_when(wy_month == 1 ~ 1,
                            wy_month == 2 ~ 32,
                            wy_month == 3 ~ 61,
                            wy_month == 4 ~ 92,
                            wy_month == 5 ~ 122,
                            wy_month == 6 ~ 153,
                            wy_month == 7 ~ 183,
                            wy_month == 8 ~ 214,
                            wy_month == 9 ~ 245,
                            wy_month == 10 ~ 275,
                            wy_month == 11 ~ 306,
                            wy_month == 12 ~ 336,
                            TRUE ~ NA_real_)
           ,
           dplyr::case_when(wy_month == 1 ~ 1,
                            wy_month == 2 ~ 32,
                            wy_month == 3 ~ 60,
                            wy_month == 4 ~ 91,
                            wy_month == 5 ~ 122,
                            wy_month == 6 ~ 152,
                            wy_month == 7 ~ 182,
                            wy_month == 8 ~ 213,
                            wy_month == 9 ~ 244,
                            wy_month == 10 ~ 274,
                            wy_month == 11 ~ 305,
                            wy_month == 12 ~ 335,
                            TRUE ~ NA_real_)
    )

}

#' Add Proportion
#' @description Adds proportion of observation per water year based on the maximum.
#'
#' @param data A daily value df
#' @param value_name A unquoted expression for column name, e.g. vattenforing_m3_s.
#'
#' @return a proportion column within df per water year.
#' @noRd
add_proportion <- function(data, value_name) {

        dplyr::group_by(data, wy) %>%
        dplyr::mutate(dplyr::across({{value_name}},
                                    list(max_prop = ~.x/max(.x, na.rm = TRUE)
                                    ))) %>%
        dplyr::ungroup()
}
