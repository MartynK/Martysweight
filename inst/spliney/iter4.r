# Optimizing knot placement via rolling forecasts

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
    n_obs_min = 100
    #,dat = dat
) {
  
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
  
  knot <- c()
  dayswithdp <- 0
  days <- floor( dat_train$numd)
  for (i in seq(1,max(floor(dat_train$numd)))) {
    if(dayswithdp>=k) {
      knot_act <- (  dat_train$numd[ which(days == i)] 
                     + dat_train$numd[ which(days == i)-1]
      ) / 2 # Gives singularities
      knot <- c(knot,i)# knot_act would have gone here
      dayswithdp <- 0
    }
    dayswithdp <- ifelse(i %in% days, dayswithdp+1, dayswithdp)
  }
  
  expre <- paste(knot,collapse=",") %>%
    paste( "c(", ., ")") %>%
    paste("Mass ~ ns( fastd, df = 1) + ns(numd, knots =", ., ")" 
    ) %>%
    as.formula(.)
  
  tryCatch({
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
  }, error = function(e) {
    return(
      data.frame(
        rmse = NA,
        mad  = NA,
        cor. = NA
      )
    )
  })
  
  
  dat_test$pred <- predict(mod, newdata = dat_test)
  
  return(
    data.frame(
      rmse = sum((dat_test$Mass - dat_test$pred)^2),
      mad  = sum(abs(dat_test$Mass - dat_test$pred)),
      cor. = cor(dat_test$numd, dat_test$Mass - dat_test$pred)
    )
  )
}

dates_rolling <- seq( min(dat$Date) + 3600*24*30, 
                      max(dat$Date), by = 3600*24*30)

out <- expand.grid(rmse = NA,
                  mad = NA,
                  cor. = NA,
                  end_date = dates_rolling,
                  k = c(
                    25,35,45,55
                    ))
pb <- txtProgressBar()
for (i in 1:nrow(out)) {
 out[i,1:3] <- rolling_errors(end_date = out$end_date[i],
                           k = out$k[i]) 
 setTxtProgressBar(pb, i/nrow(out))
}
close(pb)




res_path <- here::here("inst","spliney","crossval_results",
                       "roll_forecast_results.rdata")

if (file.exists( res_path)) {
  out_new <- out
  load(file = res_path)
  out <- bind_rows( out, out_new)
  save(out, file = res_path)  
} else {
  save(out, file = res_path)
}


out_groupped <- out %>%
  group_by(k) %>%
  mutate(mad_mean  = mean(mad, na.rm = TRUE),
         rmse_mean = mean(rmse, na.rm = TRUE),
         cor_mean  = mean(abs(cor.), na.rm = TRUE)
         ) %>%
  slice(1)


out %>%
  ggplot(aes(x = k, y = rmse, group = k)) +
    theme_bw() +
    geom_point() +
    geom_line(mapping = aes(group=end_date),alpha = .25) +
    geom_point( mapping=aes(y = rmse_mean), color = "red", 
                size = 3,
                data = out_groupped) +
    geom_line( mapping=aes(y = rmse_mean, group = ""), color = "red", 
                linewidth = 1.5,
                data = out_groupped) +
    geom_point( mapping=aes(y = mad_mean), color = "cyan", 
                size = 3,
                data = out_groupped) +
    geom_line( mapping=aes(y = mad_mean, group = ""), color = "cyan", 
               linewidth = 1.5,
               data = out_groupped) +
    scale_y_sqrt()



# dat_p <- expand.grid( fastd = c(0,8,16,24),
#                       numd = c(seq(1,10,.1),seq(35,max(dat$numd)+1,.1)),
#                       maxinlast = 12,
#                       Episode = 14) %>%
#   mutate( fastd_truncated = fastd)
# dat_p$pred <- predict(mod, newdata = dat_p)
# 
# dat_p$numd <- as.Date( dat_p$numd, origin = min(dat$Date))
# dat$Date2  <- as.Date( dat$numd,   origin = min(dat$Date))
# 
# 
# ggplot(dat_p[dat_p$fastd==8,], 
#        aes(x=numd, y = pred, group = fastd, color = fastd)) +
#   theme_bw() +
#   theme(panel.grid.major.x = element_line(colour = "grey40"),
#         panel.grid.minor.x = element_line(colour = "grey40")) +
#   stat_summary(data = dat, 
#                aes(x = Date2, y = Mass, group = Episode),
#                fun=mean, geom="line", colour="blue") +
#   geom_point(data = dat, 
#              aes(x = Date2, y = Mass, 
#                  color=fastd, group = Episode)
#              , size = 1, alpha = .5) +
#   #geom_line(data = dat, aes(x = Date2, y = Mass, group = Episode), size = .5, color = "grey50") +
#   scale_y_continuous(limits = c(60,89)) +
#   geom_vline(xintercept = days(floor(knot)) + min(dat$Date2), alpha = .5, 
#              color = "#AEEEEE", linetype = "dashed") +
#   geom_line(size=1.1, color = "salmon4") +
#   scale_x_date(date_breaks = "2 years",date_minor_breaks = "1 year", date_labels = "%y") + 
#   labs( x = "Date",
#         y = "Mass")
# 
# 
# pic <- ggplot(dat_p, aes(x=numd, y = pred, group = fastd, color = fastd)) +
#   theme_bw() +
#   geom_line(size=.9) +
#   geom_point(data = dat, aes(x = Date2, y = Mass, color=fastd, group = Episode), size = 1) +
#   geom_line(data = dat, aes(x = Date2, y = Mass, group = Episode), size = .5, color = "grey50") +
#   scale_y_continuous(limits = c(80,87)) +
#   geom_vline(xintercept = knot) +
#   scale_x_date(date_breaks = "1 week",date_minor_breaks = "1 week", date_labels = "%m-%d") + 
#   labs( x = "Date",
#         y = "Mass") +
#   scale_x_date(date_breaks = "1 week",
#                date_minor_breaks = "1 day", 
#                date_labels = "%m-%d",
#                limits = c(max(dat_p$numd)-14, max(dat_p$numd)))
# 
# 
