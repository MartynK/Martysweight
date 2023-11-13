# Doing stuff with tmy libre2 data

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

dat <- here::here("libre2_data.csv") %>%
  read_csv( skip = 1) %>%
  #.[,3:6] %>%
  janitor::clean_names() %>%
  select(  device_timestamp,
          record_type,
          historic_glucose_mmol_l,
          scan_glucose_mmol_l,
          non_numeric_food) %>%
  rename( time = `device_timestamp`,
          variable = `record_type`,
          avg_glucose = `historic_glucose_mmol_l`,
          cur_glucose = `scan_glucose_mmol_l`,
          meal = `non_numeric_food`) %>%
  mutate( time = as_datetime( time, format = "%d-%m-%Y %H:%M"),
          tseom = NA, #!
          last_meal = NA #!
          )

# Time since end of meal (lazy way)
.feeds <- dat$time[dat$variable == 5]
for (i in 1:nrow(dat)) {
  dat$tseom[i] <- difftime( dat$time[i], 
                            # max. feeding times smaller than actual time
                            max( .feeds[.feeds <= dat$time[i]]),
                            units = "hours")
  try( silent = TRUE, expr = {
    dat$last_meal[i] <-   dat$meal[ dat$time == max( .feeds[.feeds <= 
                                                              dat$time[i]]) &
                                    dat$variable == 5]
  })
  
}

dat_avg <- dat %>%
  select( !(c(cur_glucose, meal, variable))) %>%
  filter( is.na( avg_glucose) == FALSE)



dat_avg %>%
  ggplot( aes( x = time, y = avg_glucose)) +
    theme_bw() +
    geom_line() +
    geom_point( aes( x = time, y = cur_glucose), 
                #!
                data = dat) +
    geom_vline( aes(xintercept = time), 
                #!
                data = dat %>% filter(variable == 5),
                color = "salmon4")


dat %>%
  filter( is.na( avg_glucose) == FALSE) %>%
  ggplot( aes(x = tseom, y = avg_glucose, color = last_meal, group = last_meal)) +
  theme_bw() +
  geom_point(alpha = .3) +
  geom_point(alpha = .3, mapping = aes(y = cur_glucose), 
             data = dat, #!
             shape = 2
             ) +
  geom_line() +
  scale_x_continuous( breaks = c(1,2,4,8,12,16,20),minor_breaks = NULL) +
  geom_hline( yintercept = 5.6, color = "salmon4") +
  facet_wrap( facets = "last_meal", nrow = 5)
  

