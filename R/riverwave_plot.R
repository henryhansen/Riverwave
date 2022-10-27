# library(tidyverse)
# library(lubridate)
#
#
# # Grab some gauge data
# stn274 <- Riverwave::smhi_csv(274)
#
# # Flood Frequency Data Prep -----------------------------------------------
#
# floo <- function(smhidata) {
#     # add year column based on dates
#     smhidata$year <- year(ymd(smhidata$`Datum (svensk sommartid)`))
#
#     #add date column based on swedish dates
#     smhidata$date <- ymd(smhidata$`Datum (svensk sommartid)`)
#
#     #add julian day based on dates
#     smhidata$yday <- yday(smhidata$date)
#
#     #grab only max discharge for each year
#     gaugedata <- smhidata %>%
#         group_by(year) %>%
#         filter(Vattenföring == max(Vattenföring))
#
#     #return final dataset
#     return(gaugedata)
# }
#
# # test new function
# ggdt <- floo(stn274)
#
# # exceedence probability --------------------------------------------------
# # gaugedata = formatted gauge data (annual maximum discharges)
# # dishcarge = name of discharge column as string
# # constant = constant used for gringorten plotting
# # comes from - https://vt-hydroinformatics.github.io/floods.html#calculate-exceedance-probability-and-return-period
#
# excd <- function(gaugedata, discharge, constant) {
#     #rename for convenience
#     df <- gaugedata
#
#     #assign length of dataframe
#     N <- length(df)
#
#     #assign frequency factor constant
#     a <- constant
#
#     # assign ranks to discharges
#     df$ranks <- rank(-df[,discharge])
#
#     #calculate Gringorten plotting position formula
#     df$qi <- sapply(df$ranks, function(i) (i - a) / (N + 1 - (2*a)))
#
#     #calculate non exceedence probability
#     df$pi <-  1 - df$qi
#
#     #calculate return period
#     df$TpEst  <-  1/(1-df$pi)
#
#     return(df)
# }
#
# # test exceedence function
#  out <- excd(ggdt, "Vattenföring", 0.44)
#
# # See exceedence plot
# plot(out$TpEst,out$Vattenföring)
#
# # fit lognormal distribution to discharge ---------------------------------
#
# # load brms for bayesian fit
# library(brms)
#
# # fit lognormal distribution using a bayesian approach
# fit <- brm(Vattenföring ~ 1, data = out, family = lognormal())
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
# hist(out$Vattenföring, freq = F, breaks = 10)
# curve(dlnorm(x, meanlog=post[1,1], sdlog=post[2,1]), from = 0, to = max(out$Vattenföring), add = T)
#
# # calculate return period -------------------------------------------------
#
#
# # Frequency factor for the log-normal distribution
# # m - mean
# # s - standard deviation
# # p - exceedance probability
# # comes from - https://tonyladson.wordpress.com/2017/09/30/log-normal-flood-frequency-analysis/
#
# FF_LogNormal <- function(m, s, p) {
#     #calculate coefficient of variation
#     cv <- s/m
#
#     #calculate standard normal deviate
#     z <- qnorm(1 - p)
#
#     #calculate frequency factor for lognormal distribution
#     kt <- (1/cv) * (exp(sqrt(log(1 + cv^2)) * z - 0.5 * log(1 + cv^2)) - 1)
#
#     #calculate 100-year flood
#     qvalue <- m + kt * s
#
#     #return q100
#     return(qvalue)
# }
#
# # test function
# m <- post[1,1] #grab mean from posterior
# s <- post[2,1] #grad sd from posterior
#
# q100 <- FF_LogNormal(m, s , 0.01) #calculate q100
#
# q100
# exp(q100)
#
#
# # fit daily discharge across time? -----------------------------------------
#
#
#
# # plot riverwave given 100 year flood -------------------------------------
#
# # ggplot(data = stn274, aes(x = yday(`Datum (svensk sommartid)`), y = Vattenföring, color = year(`Datum (svensk sommartid)`))) +
# #     geom_point(alpha =0.5)
#
# # Create daily averages riverwave
# stn274 %>%
#     mutate(year = year(`Datum (svensk sommartid)`),
#            yday = yday(`Datum (svensk sommartid)`)) %>%
#     group_by(yday) %>%
#     summarise(avg = mean(Vattenföring)) -> daily_averages
#
# #generate values needed for color coding area
# mid <- daily_averages$yday/2
# vals <- lapply(daily_averages$avg, function(y) seq(0, y, by = 1))
# y <- unlist(vals)
# mid <- rep(daily_averages$yday, lengths(vals))
# rvwv <- data.frame(x = mid - 0.5,
#                     xend = mid + 0.5,
#                     y = y,
#                     yend = y)
#
# # plot riverwave using daily averages and assumed value
# ggplot(data = rvwv, aes(x = x, xend = xend, y = y, yend = yend, color = y)) +
#     geom_segment(size = 2,lineend = "round") +
#     scale_color_gradient2(low = "green", mid = "yellow", high = "red",
#                           midpoint = max(rvwv$y)/2)+
#     theme_minimal()
#
#
# # ggplot(data = daily_averages, aes(x = yday, y = avg)) +
# #     geom_line() +
# #     geom_segment(size = 1,aes(x = yday-0.5, xend = yday+0.5, y = avg, yend = avg, color = avg)) +
# #     scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = max(daily_averages$avg)/2)
