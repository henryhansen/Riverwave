#' Title
#'
#' @param smhidata
#'
#' @return gaugedata for bayesian ffa
#' @export
#'
#' @examples
floo <- function(smhidata) {
    # add year column based on dates
    smhidata$year <- lubridate::year(lubridate::ymd(smhidata$date))

    #add date column based on swedish dates
    smhidata$date <- lubridate::ymd(smhidata$date)

    #add julian day based on dates
    smhidata$yday <- lubridate::yday(smhidata$date)

    #grab only max discharge for each year
    gaugedata <- smhidata %>%
        dplyr::group_by(year) %>%
        dplyr::distinct("vattenforing_m3_s", .keep_all = T) %>%
        dplyr:: filter("vattenforing_m3_s" == max("vattenforing_m3_s"))

    #drop last column
    gaugedata <- gaugedata[1:(length(gaugedata)-1)]

    #return final dataset
    return(gaugedata)
}


#' Title
#'
#' @param gaugedata
#' @param discharge
#' @param constant
#'
#' @return exceedence table
#' @export
#'
#' @examples
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
        df[index, "vattenforing_m3_s"] <- NA
        df$vattenforing_m3_s_d <- as.double(df$vattenforing_m3_s)
        dis <- df$vattenforing_m3_s_d
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

