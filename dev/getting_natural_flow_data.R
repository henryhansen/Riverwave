devtools::document()
library(tidyverse)

### read in sites

final_dataset <- read_rds('data/final_dataset.rds')


### loop through sites to get the natural flow

natural_flow <- final_dataset %>% split(.$Stnno) %>% map(safely(~smhi_vatten_natural(.$Stnno)))

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

