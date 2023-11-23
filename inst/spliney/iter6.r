# Optimizing knot placement via rolling forecasts; best no. obs. before the end?

library(readxl)
library(splines)
library(nlme)
library(ggplot2)
library(dplyr)
library(lubridate)

setwd(here::here())

#####
dat <- here::here("inst","extdata","martysweight.xlsx") %>%
  read_excel( col_types = c("date", "numeric", "numeric", 
                            "numeric", "numeric", "numeric","numeric")) %>%
  filter(is.na(Mass) == FALSE) %>%
  mutate( Time_hours = ifelse( is.na(Time_hours)== TRUE, 12, Time_hours),
          Time_minutes = ifelse( is.na(Time_minutes)== TRUE, 0, Time_minutes),
          Time_fasted_hours = ifelse( is.na(Time_fasted_hours)== TRUE, 
                                      4, Time_fasted_hours),
          Time_fasted_minutes = ifelse( is.na(Time_fasted_minutes)== TRUE, 
                                        0, Time_fasted_minutes),
          numd = as.numeric( Date 
                             + Time_hours * 3600 
                             + Time_minutes * 60
                             - min(Date)) / 24,
          fastd = Time_fasted_hours + Time_fasted_minutes / 60,
          fastd_truncated = ifelse( fastd > 16, 16, fastd),
          maxinlast = 0
  )   %>%
  arrange(., numd)

##########
# Episodes setup

# Assuming 'dat' is your data frame
dat <- dat %>%
  mutate(
    fast_start =  round((numd - fastd /24)*7
                        , digits = 1)/7,
    fast_diff =  fast_start - lag(fast_start)
    
  )

# Initialize the first episode
act_episode    <- 1
dat$Episode[1] <- 1

# Loop through the rows to assign episodes
for(i in 2:nrow(dat)) {
  if ( dat$fast_diff[i] > 0.5) {
    act_episode <- act_episode + 1
  }
  dat$Episode[i] <- act_episode
}

# Convert 'Episode' back to factor if needed
dat$Episode <- as.factor(dat$Episode)

rolling_errors <- function(
    k = 20,
    end_date = as_date("2021-05-01"),
    window = as.period("30 days"),
    n_obs_min = 100,
    days_before_last = 20
    #,dat = dat
) {
  
  if ( days_before_last > k) {
    return(      
      data.frame(
        rmse = NA,
        mad  = NA,
        cor. = NA
      ))
  }
  
  dat_train <- dat[dat$Date <= end_date,]
  dat_test  <- dat[dat$Date >= end_date &
                     dat$Date <= end_date + window,]
  
  if ( nrow(dat_test) == 0 |
       nrow(dat_train) <  n_obs_min) {
    return(      
      data.frame(
        rmse = NA,
        mad  = NA,
        cor. = NA
    ))
  }
  
  ##########
  
  days_unique <- unique(floor(dat_train$numd))
  days_as_knots <- seq(k,length(days_unique),by= k)
  days_before_last_act <- length(days_unique) - dplyr::last(days_as_knots)
  days_before_diff <- days_before_last_act - days_before_last
  days_as_knots_corr <- days_as_knots + days_before_diff
  days_as_knots_corr <- 
    days_as_knots_corr[ days_as_knots_corr > 10 & # hard coded
                        days_as_knots_corr < length(days_unique)]
  
  knot <- dat_train %>%
    filter(floor(numd) %in% days_unique[days_as_knots_corr]) %>%
    .$numd %>%
    floor %>%
    unique

  
  expre <- paste(knot,collapse=",") %>%
    paste( "c(", ., ")") %>%
    paste("Mass ~ ns( fastd, df = 1) + ns(numd, knots =", ., ")" 
    ) %>%
    as.formula(.)
  
  try(silent = TRUE, 
      {
        mod <- gls( expre,
                    #random = ~ 0 + fastd_truncated|Episode,
                    weights = varPower(),
                    #correlation = corAR1( form = ~1|Episode, value = .13),
                    control =  glsControl(maxIter = 2000,
                                          msMaxIter = 3000,
                                          #niterEM = 400,
                                          msVerbose = FALSE,
                                          tolerance = 1e-6,
                                          msTol = 1e-6),
                    data = dat)
        
        dat_test$pred <- predict(mod, newdata = dat_test)
        })
  
  if (!(exists("mod") & "pred" %in% colnames(dat_test))) {
    return(
      data.frame(
        rmse = NA,
        mad  = NA,
        cor. = NA
      ))
  } else {
    return(
      data.frame(
        rmse = sum((dat_test$Mass - dat_test$pred)^2),
        mad  = sum(abs(dat_test$Mass - dat_test$pred)),
        cor. = cor(dat_test$numd, dat_test$Mass - dat_test$pred)
      )
    )
  }
}

dates_rolling <- seq( min(dat$Date) + 3600*24*30, 
                      max(dat$Date), by = 3600*24*30)

res_path <- here::here("inst","spliney","crossval_results",
                       "roll_forecast_lastday_results.rdata")

if (file.exists( res_path)) {
  load(file = res_path)
  out_old <- out
} else {
  out_old <- ""
}


out <- expand.grid(rmse = NA,
                  mad = NA,
                  cor. = NA,
                  k = c(
                    50,60,70,80
                    ),
                  days_before_last_perc = c(.15,.3,.4,.5,.6,.7,.8,.9,1)
                  ) %>%
  mutate(days_before_last = round(k * days_before_last_perc)) %>%
  group_by(k) %>%
  distinct(days_before_last, .keep_all = TRUE) %>% # remove possible duplicate levels
  tidyr::crossing(., data.frame(end_date = dates_rolling)) %>% # getting all possible combinations
  anti_join(.,out_old, by = c("k","end_date","days_before_last")) # remove already calculated results
  
# Keep only those records in out which are not present in out_old



pb <- txtProgressBar(style = 3)
for (i in 1:nrow(out)) {

    out[i,1:3] <- rolling_errors(end_date = out$end_date[i],
                             k = out$k[i],
                             days_before_last = out$days_before_last[i]
                             ) 
  setTxtProgressBar(pb, i/nrow(out))
}
close(pb)


res_path <- here::here("inst","spliney","crossval_results",
                       "roll_forecast_lastday_results.rdata")

if (file.exists( res_path)) {
  out_new <- out
  load(file = res_path)
  out <- bind_rows( out, out_new)
  save(out, file = res_path)  
} else {
  save(out, file = res_path)
}


out_groupped <- out %>%
  group_by(k,days_before_last) %>%
  mutate(mad_mean  = mean(mad, na.rm = TRUE),
         rmse_mean = mean(rmse, na.rm = TRUE),
         cor_mean  = mean(abs(cor.), na.rm = TRUE),
          nas = sum(is.na(mad))
         ) %>%
  slice(1)


out %>%
  ggplot(aes(x = days_before_last, y = rmse, group = k)) +
    theme_bw() +
    geom_point() +
    geom_line(mapping = aes(group=end_date),alpha = .25) +
    geom_point( mapping=aes(y = rmse_mean), color = "red", 
                size = 3,
                data = out_groupped) +
    geom_line( mapping=aes(y = rmse_mean), color = "red", 
                linewidth = 1.5,
                data = out_groupped) +
    geom_point( mapping=aes(y = mad_mean), color = "cyan", 
                size = 3,
                data = out_groupped) +
    geom_line( mapping=aes(y = mad_mean, group = ""), color = "cyan", 
               linewidth = 1.5,
               data = out_groupped) +
    scale_y_sqrt() +
    facet_wrap(facets="k")# days_before_last")


out_groupped %>%
  ggplot(aes(x = days_before_last_perc, y = rmse_mean, group = days_before_last)) +
  theme_bw() +
  geom_point() +
  geom_line(mapping = aes(group=k, color = k),alpha = .5) 