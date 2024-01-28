devtools::document()
library(tidyverse)

### read in sites

final_dataset <- read_rds('data/final_dataset.rds')


### loop through sites to get the natural flow

natural_flow <- final_dataset %>%
                sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
                split(.$Stnno) %>% map(safely(~smhi_vatten_natural_pt(.)))

stn_names <- names(natural_flow%>%
                       purrr::keep(~length(.) != 0) %>%
                       purrr::map(~.x[['result']])  %>%
                       purrr::keep(~!is.null(.)))

natural_flow_df <-  natural_flow %>%
                    purrr::keep(~length(.) != 0) %>%
                    purrr::map(~.x[['result']])  %>%
                    purrr::keep(~!is.null(.)) %>%
                    purrr::map2(., stn_names, ~ .x %>% mutate(Stnno = .y)) %>%
                    plyr::rbind.fill()

write_csv(natural_flow_df, 'data/natural_flow_df.csv')

### generate bayes' ffa
smhiDataClean <- read_rds("data/clean_data.rds")

# Run fitting procedure for each river ------------------------------------
# run flood data preparation
floosmhi <- map(smhiDataClean, ~floowy(., vattenforing_m3_s) %>% rename(vattenforing_m3_s = 'peak_flow'))

# run exceedence calculations
# also need this for names for fit
excdsmhi <- map(floosmhi, ~excd(.))

fits <- brm_multiple(vattenforing_m3_s ~ 1,
                     data = excdsmhi,
                     family = lognormal(),
                     recompile = F,
                     combine = F)

write_rds(fits, 'data/fitswy10.rds')

fits <- readRDS("data/fitswy10.rds")

# calculate flood frequency for each station ------------------------------
muparams <- fits %>% map(summary) %>% map_dbl(list("fixed","Estimate"))
sdparams <- fits %>% map(summary) %>% map_dbl(list("spec_pars","Estimate"))
ri1 <- rep(1/1.0101, length(muparams))
ri2 <- rep(1/2, length(muparams))
ri5 <- rep(1/5, length(muparams))
ests <- data.frame(muparams, sdparams, ri1, ri2, ri5)

oneyear <- exp(mapply(FF_LogNormal, ests$muparams, ests$sdparams, ests$ri1))
twoyear <- exp(mapply(FF_LogNormal, ests$muparams, ests$sdparams, ests$ri2))
fiveyear <- exp(mapply(FF_LogNormal, ests$muparams, ests$sdparams, ests$ri5))

station_ffa_results <- tibble(
    Stnno = names(excdsmhi),
    ffa_oneyear_og = oneyear,
    ffa_twoyear_og = twoyear,
    ffa_fiveyear_og = fiveyear,
)

### Now do the same for the natural flow estimates

# Run fitting procedure for each river ------------------------------------
# run flood data preparation

# since names of smhiDataClean are actually the rows and not the station we need to adjust

smhi_rows <- tibble(
    rowid = rownames(final_dataset),
    Stnno = as.character(final_dataset$Stnno)
)

floosmhi <- natural_flow_df %>%
            left_join(smhi_rows) %>%
            split(.$rowid) %>%
            map(~floowy(., values) %>% rename(vattenforing_m3_s = 'peak_flow'))

# run exceedence calculations
excdsmhi <- map(floosmhi, ~excd(.))

# now fit to natural flow
fits_nf <- brm_multiple(vattenforing_m3_s ~ 1,
                     data = excdsmhi,
                     family = lognormal(),
                     recompile = F,
                     combine = F)

write_rds(fits_nf, 'data/fitswy10_nf.rds')

fits_nf <- readRDS("data/fitswy10_nf.rds")

# calculate flood frequency for each station ------------------------------
muparams_nf <- fits_nf %>% map(summary) %>% map_dbl(list("fixed","Estimate"))
sdparams_nf <- fits_nf %>% map(summary) %>% map_dbl(list("spec_pars","Estimate"))
ri1_nf <- rep(1/1.0101, length(muparams_nf))
ri2_nf <- rep(1/2, length(muparams_nf))
ri5_nf <- rep(1/5, length(muparams_nf))
ests_nf <- data.frame(muparams_nf, sdparams_nf, ri1_nf, ri2_nf, ri5_nf)

oneyear_nf <- exp(mapply(FF_LogNormal, ests_nf$muparams_nf, ests_nf$sdparams_nf, ests_nf$ri1_nf))
twoyear_nf <- exp(mapply(FF_LogNormal, ests_nf$muparams_nf, ests_nf$sdparams_nf, ests_nf$ri2_nf))
fiveyear_nf <- exp(mapply(FF_LogNormal, ests_nf$muparams_nf, ests_nf$sdparams_nf, ests_nf$ri5_nf))

# create a tibble with natural flow estimates
station_ffa_results_nf <- tibble(
    Stnno = names(excdsmhi),
    ffa_oneyear_nf = oneyear_nf,
    ffa_twoyear_nf = twoyear_nf,
    ffa_fiveyear_nf = fiveyear_nf,
)


# now combine original with natural flow
station_ffa_results <- station_ffa_results %>% left_join(station_ffa_results_nf)

# five year
station_ffa_results %>%
    ggplot(aes(ffa_fiveyear_nf, ffa_fiveyear_og)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0)

# 2 year
station_ffa_results %>%
    ggplot(aes(ffa_twoyear_nf, ffa_twoyear_og)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0)

# year
station_ffa_results %>%
    ggplot(aes(ffa_oneyear_nf, ffa_oneyear_og)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0)

write_csv(station_ffa_results, 'data/combined_ffa.csv')


library(tidyverse)

station_ffa_results <- read_csv('data/combined_ffa.csv')

devtools::document()


riverwave_3d(smhiDataClean[[1]], vattenforing_m3_s, station_ffa_results[1,]$ffa_oneyear_nf,
             station_ffa_results[1,]$ffa_twoyear_nf)

riverwave_percentiles_plot(smhiDataClean[[1]],
                           vattenforing_m3_s,
                           station_ffa_results[1,]$ffa_oneyear_nf,
                           station_ffa_results[1,]$ffa_twoyear_nf,
                           station_ffa_results[1,]$ffa_fiveyear_nf)
