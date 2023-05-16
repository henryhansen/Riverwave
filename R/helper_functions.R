library(tidyverse)
library(lubridate)

# Flood Frequency Data Prep -----------------------------------------------

floo <- function(smhidata) {
    # add year column based on dates
    smhidata$year <- year(ymd(smhidata$Date))
    
    #add date column based on swedish dates
    smhidata$date <- ymd(smhidata$Date)
    
    #add julian day based on dates
    smhidata$yday <- yday(smhidata$Date)
    
    #grab only max discharge for each year
    gaugedata <- smhidata %>%
        group_by(year) %>%
        distinct(Discharge, .keep_all = T) %>% 
        filter(Discharge == max(Discharge))
    
    #return final dataset
    return(gaugedata)
}

# test new function
# ggdt <- floo(stn274)

# exceedence probability --------------------------------------------------
# gaugedata = formatted gauge data (annual maximum discharges)
# dishcarge = name of discharge column as string
# constant = constant used for gringorten plotting
# see here for overview of flood frequency approach - https://vt-hydroinformatics.github.io/floods.html#calculate-exceedance-probability-and-return-period

excd <- function(gaugedata, discharge, constant) {
    #rename for convenience
    df <- gaugedata
    
    #assign length of dataframe
    N <- nrow(df)
    
    #assign frequency factor constant
    a <- constant
    
    # assign ranks to discharges
    df$ranks <- rank(-df[,discharge])
    
    #calculate Gringorten plotting position formula
    # df$qi <- sapply(df$ranks, function(i) (i - a) / (N + 1 - (2*a)))
    df <- transform(df, qi = (ranks - a) / (N + 1 - (2*a)))
    
    # #calculate non exceedence probability
    df$pi <- 1 - df$qi
    
    #calculate return period
    df$TpEst <- 1 / (1-df$pi)
    
    return(df)
}

# # test exceedence function
#  out <- excd(ggdt, "Discharge", 0.44)
#
#
# # See exceedence plot
# plot(out$TpEst,out$Discharge)

# fit lognormal distribution to discharge ---------------------------------
#
# # load brms for bayesian fit
# library(brms)
#
# # fit lognormal distribution using a bayesian approach
# fit <- brm(Discharge ~ 1, data = out, family = lognormal())
#
# # see summary of model results
# summary(fit)
#
# # see plot of model results
# plot(fit)
#
# # plot posterior predictive checks
# pp_check(fit, ndraws = 50)
#
# # declare posterior values
# post <- posterior_summary(fit)
#
# # plot resulting distribution and fitted distribution
# hist(out$Discharge, freq = F, breaks = 10)
# curve(dlnorm(x, meanlog=post[1,1], sdlog=post[2,1]), from = 0, to = max(out$Discharge), add = T)

# calculate return period -------------------------------------------------


# Frequency factor for the log-normal distribution
# m - mean
# s - standard deviation
# p - exceedance probability
# see here for general description - https://tonyladson.wordpress.com/2017/09/30/log-normal-flood-frequency-analysis/

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

# # test function
# m <- post[1,1] #grab mean from posterior
# s <- post[2,1] #grad sd from posterior
#
# q100 <- FF_LogNormal(m, s , 0.01) #calculate q100
#
#
# ReturnInterval <- c(1.0101,2,5,10,15,25,30,35,40,45,50,60,70,80,90,100,150,200)
#
# (1/ReturnInterval)
# q100
# exp(q100)
#
# q2 <- FF_LogNormal(m, s , 0.5) #calculate flow at which a 2 year flood would happen




# convert model list into posterior estimates dataframe -------------------
brms.fit2post <- function(model.list) {
    temp <- lapply(model.list, posterior_summary)
    mus <- lapply(temp, `[[`, 1)
    sigmas <- lapply(temp, `[[`, 2)
    df <- data.frame(mus = unlist(mus),
                     sigmas = unlist(sigmas))
    return(df)
}



