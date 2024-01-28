
# load libraries ----------------------------------------------------------
library(Riverwave)
library(tidyverse)
library(purrr)
library(lubridate)
library(brms)


# load data ---------------------------------------------------------------
smhiDataClean <- read_rds("../Analysis/clean_data.rds")

# test --------------------------------------------------------------------
test <- smhiDataClean[["428"]]

# Run fitting procedure for each river ------------------------------------
# run flood data preparation
floosmhi <- map(smhiDataClean, ~floo(.))

#calculate mean flood day
meyday <- map_dbl(floosmhi, ~mean(.$yday))
write_rds(meyday, "../Analysis/meyday.rds")

# calculate coefficient of variation on yday
coefv <- map_dbl(floosmhi, ~sd(.$yday)/mean(.$yday))
write_rds(coefv, "../Analysis/coefv.rds")

# run exceedence calculations
excdsmhi <- map(floosmhi, ~excd(.))

# fit brms lognormal models to each dataset
# discharge_fit <- function(dis_df){
#     brm(vattenforing_m3_s_d ~ 1,
#         data = dis_df,
#         family = lognormal())
# }
# fits <- map(excdsmhi, ~discharge_fit(.))

fits <- brm_multiple(vattenforing_m3_s ~ 1,
    data = excdsmhi,
    family = lognormal(),
    recompile = F,
    combine = F)

write_rds(fits, "../Analysis/fits.rds")
# fits <- readRDS("../Analysis/fits.rds")

# calculate flood frequency for each station ------------------------------
muparams <- fits %>% map(summary) %>% map_dbl(list("fixed","Estimate"))
sdparams <- fits %>% map(summary) %>% map_dbl(list("spec_pars","Estimate"))
ri <- rep(1/100, length(muparams))
ests <- data.frame(muparams, sdparams, ri)
write_rds(ests, "../Analysis/ests.rds")
# ReturnInterval <- c(1.0101,2,5,10,15,25,30,35,40,45,50,60,70,80,90,100,150,200)
hundredyear <- exp(mapply(FF_LogNormal, ests$muparams, ests$sdparams, ests$ri))

# export 100 year flood discharges for all stations --------------------
write_rds(hundredyear, "../Analysis/hundredffa.rds")


# Calculate 5 year flood discharges ---------------------------------------
ri <- rep(1/5, length(muparams))
ests <- data.frame(muparams, sdparams, ri)
fiveyear <- exp(mapply(FF_LogNormal, ests$muparams, ests$sdparams, ests$ri))
write_rds(fiveyear, "../Analysis/fiveffa.rds")
