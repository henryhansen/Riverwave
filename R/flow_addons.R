
#' Richards-Baker Flashiness index (RBI)
#' @description Adds RBI of observation (flow).
#'
#' @param data A daily value data.frame.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s
#' @param ... values to pass to `dplyr::group_by()` to perform RBI calculation on. (optional)
#'
#' @return A rbi column within data.frame.
#' @note The user must specify the `flow_var` column but it's optional whether to use `dplyr::group_by()` or not.
#' @importFrom dplyr "%>%"
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
    z <- stats::qnorm(1 - p)

    #calculate frequency factor for lognormal distribution
    kt <- (1/cv) * (exp(sqrt(log(1 + cv^2)) * z - 0.5 * log(1 + cv^2)) - 1)

    #calculate 100-year flood
    qvalue <- m + kt * s

    #return q100
    return(qvalue)
}

#' Prepping Flow Data
#'
#' @param data A data.frame with date and flow columns.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s.
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#'
#' @return A \code{tibble} with original data and added date columns.
#' @export
#'
#' @note In data.frame or \code{tibble} `date` column must be named `date`.
#'
#' @examples
#' stream_flow <- data.frame(flow = c(seq(30, 60), seq(60, 30, length.out = 60)),
#'                           date = seq(as.Date('2012-01-01'), by = "day", length.out = 91))
#'
#' stream_flow_prepped <- prep_flow(stream_flow, value = flow, wy_month = 10)
#'
#'
#'
#'
#'
#'
#'
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
               month_day = stringr::str_c(month, day, sep = "-"),
               wy = waterYear(date, wy_month, TRUE),
               month_abb = factor(month.abb[month], levels = month.abb),
               month_day = stringr::str_c(month, day, sep = "-")) %>%
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

#' Add Proportions via Theory
#' @description Adds proportion of observation per water year day based on user imputed recurrence intervals, which correspond to riverwave theory.
#'
#' @param data A daily value df
#' @param value_name A unquoted expression for column name, e.g. vattenforing_m3_s.
#' @param q1 Numeric. Peakflow for 1 year recurrence interval.
#' @param q2 Numeric. Peakflow for 2 year recurrence interval.
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#'
#' @return a proportion column within df per water year.
#' @noRd
add_wave_proportions <- function(data, value_name, q1, q2, wy_month = 10) {


        data <- prep_flow(data, {{value_name}}, wy_month)

        ff_vals <- dplyr::tibble(q1 = q1, q2 = q2)

        data %>%
        dplyr::group_by(wy_doy) %>%
        dplyr::mutate(cut_value = ifelse({{value_name}} < ff_vals$q1, 'Riverine Productivity',
                                  ifelse({{value_name}} >= ff_vals$q1 & {{value_name}} < ff_vals$q2, 'River Continuum', 'Flood Pulse')),
               cut_value = factor(cut_value, levels = c('Riverine Productivity', 'River Continuum', 'Flood Pulse'))) %>%
        dplyr::add_count() %>%
        dplyr::group_by(wy_doy, n) %>%
        dplyr::count(cut_value, .drop = FALSE, name = 'theory_count') %>%
        dplyr::mutate(proportions = theory_count/n)
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


#' Get DOY Summary Stats
#'
#' @param data A data.frame with date and flow columns.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s.
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#'
#' @return A \code{tibble} with percentiles of `wy_doy`.
#' @export
#'
summary_stats_doy <- function(data, value_name, wy_month = 10) {

        data <- prep_flow(data, {{value_name}}, wy_month)

        data  %>%
        dplyr::group_by(wy_doy) %>%
        dplyr::reframe(quantiles = stats::quantile({{value_name}},
                                            probs = c(0,0.05, 0.1,
                                                      0.2,0.25, 0.5,
                                                      0.75,0.80, 0.90,
                                                      0.95, 1), na.rm = TRUE),
                                   breaks =  c("p0_va","p05_va", "p10_va",
                                               "p20_va", "p25_va", "p50_va",
                                               "p75_va","p80_va", "p90_va",
                                               "p95_va", "p100_va")) %>%
        tidyr::pivot_wider(values_from = quantiles, names_from = breaks)  %>%
        dplyr::right_join(data, by = c("wy_doy")) %>%
        dplyr::ungroup()

}

#' Get FF Values
#'
#' @param data A data.frame with date and flow columns.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s.
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#'
#' @return A \code{tibble} with Q1, Q2, and Q5 flood frequency values
ff_vals <- function(data, value_name, wy_month = 10) {

    data <- prep_flow(data, {{value_name}}, wy_month)

    data_peak <- data %>%
        dplyr::group_by(wy) %>%
        dplyr::reframe(peak_flow = max({{value_name}}, na.rm = T)) %>%
        dplyr::ungroup()  %>%
        dplyr::reframe(mean_q = mean(log(peak_flow), na.rm = T),
                       sd_q = stats::sd(log(peak_flow), na.rm = T))
    dplyr::tibble(
        q1 = exp(FF_LogNormal(data_peak$mean_q, data_peak$sd_q, 0.9)),
        q2 = exp(FF_LogNormal(data_peak$mean_q, data_peak$sd_q, 0.5)),
        q5 = exp(FF_LogNormal(data_peak$mean_q, data_peak$sd_q, 0.2)),
    )

}

#' @title Get Peak Flows
#' @param data A data.frame with date and flow columns.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s.
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#' @return A data.frame with peak flows labelled `peak_flow`.
floowy <- function(data, value_name, wy_month = 10) {


    data <- prep_flow(data, {{value_name}}, wy_month)

    data_peak <- data %>%
                    dplyr::group_by(wy) %>%
                    dplyr::reframe(peak_flow = max({{value_name}}, na.rm = T)) %>%
                    dplyr::ungroup()
}

#' Exceedance Probabilities
#' @description
#' This function calculates exceedance probabilities using Gringorten plotting position formula.
#'
#' @param gaugedata data.frame with discharge column
#' @param discharge A column with discharge data
#' @param constant numeric. Gringorten plotting position formula
#'
#' @return exceedence table
#' @export
#'

excd <- function(gaugedata, discharge = "vattenforing_m3_s", constant = 0.3) {
    #rename for convenience
    df <- gaugedata

    #assign length of dataframe
    N <- nrow(df)

    #assign frequency factor constant
    a <- constant

    # assign ranks to discharges
    if (is.numeric(df[,discharge])) {
        dis <- df[,discharge]
        dis <- dis * -1
        df$ranks <- rank(dis)
        attr(df$ranks, "names") <- NULL
    } else {
        index <- df[discharge] == "Saknas" | is.na(df[discharge])
        df[index, discharge] <- NA
        df[[discharge]] <- as.double(df[[discharge]])
        dis <- df[[discharge]]
        dis <- dis * -1
        df$ranks <- rank(dis, na.last = F)
        attr(df$ranks, "names") <- NULL
    }


    #calculate Gringorten plotting position formula
    # df$qi <- sapply(df$ranks, function(i) (i - a) / (N + 1 - (2*a)))
    df <- transform(df, qi = (ranks - a) / (N + 1 - (2*a)))

    # #calculate non exceedence probability
    df$pi <- 1 - df$qi

    #calculate return period
    df$TpEst <- 1 / (1-df$pi)

    return(df)
}


