library(tidyverse)


# exceedence probability --------------------------------------------------


excd <- function(data, discharge, constant) {
    N <- length(data)
    a <- constant
    df <- data
    df$ranks <- rank(-df[,discharge])
    df$qi <- sapply(df$ranks, function(i) (i - a) / (N + 1 - (2*a)))
    df$pi <-  1 - df$qi
    df$TpEst  <-  1/(1-df$pi)
    return(df)
}
#
# test <- excd(stn274, "Vattenföring", 0.44)
#
# plot(test$TpEst,test$Vattenföring)
#
#
#
# # fit distribution to discharge -------------------------------------------
#
# library(brms)
#
# fit <- brm(Vattenföring ~ 1, data = test, family = lognormal())
#
# summary(fit)
#
# plot(fit)
#
# pp_check(fit, ndraws = 50)
#
# post <- posterior_summary(fit)
#
# hist(stn274$Vattenföring, freq = F, breaks = 100)
# curve(dlnorm(x, meanlog=post[1,1], sdlog=post[2,1]), from = 0, to = max(stn274$Vattenföring), add = T)
#
# qqplot()
# # calculate return period -------------------------------------------------
#
#
# # Frequency factor for the log-normal distribution
# # m - mean
# # s - standard deviation
# # p - exceedance probability
#
# FF_LogNormal <- function(m, s, p) {
#     cv <- s/m
#     z <- qnorm(1 - p)
#     (1/cv) * (exp(sqrt(log(1 + cv^2)) * z - 0.5 * log(1 + cv^2)) - 1)
# }
#
# m <- post[1,1]
# s <- post[2,1]
#
# kt <- FF_LogNormal(m, s , 0.01)
#
# q100 <- m + kt * s
# q100
# # plot riverwave given 100 year flood -------------------------------------
#
#
# ggplot(data = stn274, aes(x = Vattenföring)) +
#     geom_density() +
#     theme_minimal()
