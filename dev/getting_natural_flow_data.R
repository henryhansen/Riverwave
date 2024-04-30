devtools::document()
library(tidyverse)
library(brms)

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

station_ffa_results <- read_csv('data/combined_ffa.csv')
### now get proportions

oneyear_og <- as.list(station_ffa_results$ffa_oneyear_og)
twoyear_og <- as.list(station_ffa_results$ffa_twoyear_og)

l <- list(smhiDataClean, oneyear_og,
          twoyear_og, names(smhiDataClean))

proportions_og <-  pmap(l,~add_wave_proportions(..1,
                                                vattenforing_m3_s,
                                                q1 = ..2,
                                                q2 = ..3) %>% mutate(Stnno = ..4) ) %>%
                    bind_rows()

oneyear_nf <- as.list(station_ffa_results$ffa_oneyear_nf)
twoyear_nf <- as.list(station_ffa_results$ffa_twoyear_nf)

l_nf <- list(smhiDataClean, oneyear_nf,
          twoyear_nf, names(smhiDataClean))

proportions_nf <-  pmap(l_nf,~add_wave_proportions(..1,
                                                vattenforing_m3_s,
                                                q1 = ..2,
                                                q2 = ..3) %>% mutate(Stnno = ..4) ) %>%
                   bind_rows()

write_csv(proportions_nf, 'data/proportions_nf.csv')
write_csv(proportions_og, 'data/proportions_og.csv')



# get the 3-d plots for both natural flow and original cutoffs

# Morrumsan = 186, Ronne = 2128, Vindelalven = 2237

final_dataset

# a pre-defined matrix for png output orientation
um <- readRDS('data/um.rds')

# because of the way it was coded with smhiDataClean we need to use `index` from `final_dataset` instead of `Stnno`

vec <- c(186, 2128, 2237, 186, 2128, 2237)

for(i in seq_along(vec)) {

i_count <- i

site = as.character(vec[i])

site_name = final_dataset[final_dataset$Stnno == vec[i],]$Section_of_river

index <- final_dataset[final_dataset$Stnno == vec[i],]$index

if (i_count %in% 1:3){

riverwave_3d(smhiDataClean[[as.character(index)]],
             vattenforing_m3_s,
             station_ffa_results[station_ffa_results$Stnno == index,]$ffa_oneyear_og,
             station_ffa_results[station_ffa_results$Stnno == index,]$ffa_twoyear_og,
             userMatrix = um)
} else {

    riverwave_3d(smhiDataClean[[as.character(index)]],
                 vattenforing_m3_s,
                 station_ffa_results[station_ffa_results$Stnno == index,]$ffa_oneyear_nf,
                 station_ffa_results[station_ffa_results$Stnno == index,]$ffa_twoyear_nf,
                 userMatrix = um)

}

rgl::title3d(sub = 'Day of Year (DOY)', line = 15, level = 0)

rgl::title3d(ylab = 'Water Year', line = 3.5)

rgl::title3d(zlab = 'Discharge (cms)', line = 3.5)

if (i_count  %in% 1:3) {
rgl::snapshot3d(paste0('images/rw3d/', site_name ,'_og.png'),
                height = 1000, width = 1100)

} else {

    rgl::snapshot3d(paste0('images/rw3d/', site_name ,'_nf.png'),
                    height = 1000, width = 1100)

}

}


# get the figures for the panel plot (figure-2)

# Karlslund 2 (SVARTÅN) = 2139

karlslund_index <- final_dataset[final_dataset$Stnno == 2139,]$index

riverwave_rastergraph(smhiDataClean[[as.character(karlslund_index)]],
             vattenforing_m3_s,
             station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_oneyear_og,
             station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_twoyear_og) %>%
    ggsave(filename = 'rastergraph_karlslund2_og.png', path = 'images/panel_plot_paper/', device = 'png',
           height = 75, width = 216, units = 'mm')

riverwave_rastergraph(smhiDataClean[[as.character(karlslund_index)]],
             vattenforing_m3_s,
             station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_oneyear_nf,
             station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_twoyear_nf)%>%
    ggsave(filename = 'rastergraph_karlslund2_nf.png', path = 'images/panel_plot_paper/', device = 'png',
           height = 75, width = 216, units = 'mm')

riverwave_percentiles_plot(smhiDataClean[[as.character(karlslund_index)]],
                           vattenforing_m3_s,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_oneyear_nf,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_twoyear_nf,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_fiveyear_nf) +
    theme_bw()

riverwave_percentiles_plot(smhiDataClean[[as.character(karlslund_index)]],
                           vattenforing_m3_s,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_oneyear_og,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_twoyear_og,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_fiveyear_og) +
    theme_bw()


vec <- c(2139, 2139)

for(i in seq_along(vec)) {

    i_count <- i

    site = as.character(vec[i])

    site_name = final_dataset[final_dataset$Stnno == vec[i],]$Section_of_river

    index <- final_dataset[final_dataset$Stnno == vec[i],]$index

    if (i_count == 1){

        riverwave_3d(smhiDataClean[[as.character(index)]],
                     vattenforing_m3_s,
                     station_ffa_results[station_ffa_results$Stnno == index,]$ffa_oneyear_og,
                     station_ffa_results[station_ffa_results$Stnno == index,]$ffa_twoyear_og,
                     userMatrix = um)
    } else {

        riverwave_3d(smhiDataClean[[as.character(index)]],
                     vattenforing_m3_s,
                     station_ffa_results[station_ffa_results$Stnno == index,]$ffa_oneyear_nf,
                     station_ffa_results[station_ffa_results$Stnno == index,]$ffa_twoyear_nf,
                     userMatrix = um)

    }

    rgl::title3d(sub = 'Day of Year (DOY)', line = 15, level = 0)

    rgl::title3d(ylab = 'Water Year', line = 3.5)

    rgl::title3d(zlab = 'Discharge (cms)', line = 3.5)

    if (i_count == 1) {
        rgl::snapshot3d(paste0('images/panel_plot_paper/rw3d', site_name ,'_og.png'),
                        height = 1000, width = 1100)

    } else {

        rgl::snapshot3d(paste0('images/panel_plot_paper/rw3d', site_name ,'_nf.png'),
                        height = 1000, width = 1100)

    }

}


# latitude graph
latitude_rastergraph(smhiDataClean) + theme(text = element_text(size = 12))


index_test <- final_dataset[final_dataset$Stnno == 2139,]$index

devtools::document()

riverwave_3d(data = smhiDataClean[[as.character(index_test)]],
             vattenforing_m3_s,
             q1 = station_ffa_results[station_ffa_results$Stnno == index_test,]$ffa_oneyear_og,
             q2 = station_ffa_results[station_ffa_results$Stnno == index_test,]$ffa_twoyear_og,
             userMatrix = um)


riverwave_percentiles_plot()

riverwave_percentiles_plot(smhiDataClean[[as.character(karlslund_index)]],
                           vattenforing_m3_s,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_oneyear_og,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_twoyear_og,
                           station_ffa_results[station_ffa_results$Stnno == karlslund_index,]$ffa_fiveyear_og) +
    theme_bw()

### just observing way it makes a difference between `natural_flow` and `regulated_flow`
natural_flow_df <- read_csv('data/natural_flow_df.csv')

natural_flow_df %>% filter(Stnno == '2139')%>% filter(date > '2010-01-01') %>%
    ggplot(aes(date, values)) +
    geom_line(data = smhiDataClean[['380']] %>% filter(date > '2010-01-01'), aes(date, vattenforing_m3_s)) +
    geom_line(color = 'red')




