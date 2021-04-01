library(readr)
library(effects)
library(nlme)
library(ggplot2)
library(splines)


fast_sleep      <- read_delim("~/V2 Docs/Bistat képzés/Misc/fasting/fast_sleep_data.txt", 
                              "\t", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE)

fs <- read_delim("~/V2 Docs/Bistat képzés/Misc/fasting/fast_sleep_data.txt", 
                  "\t", escape_double = FALSE, locale = locale(), 
                  trim_ws = TRUE)

#write.table(fast_sleep, file = "data.txt", sep = ";", dec = ",")

#View(fast_sleep)

fast_sleep$Date_beginning <- as.numeric( as.Date( fs$Date_beginning, format = "%Y.%m.%d"))
fast_sleep$time <- as.numeric(
                     fast_sleep$Day_offset * 24 * 3600 + 
                     fast_sleep$Time_of_measurement - 
                     fast_sleep$Finished_eating
                   ) / 3600

fast_sleep$twu     <- as.numeric(fast_sleep$Time_of_measurement - fast_sleep$Woke_up) / 3600
fast_sleep$twu     <- ifelse( fast_sleep$twu > fast_sleep$time, fast_sleep$time, fast_sleep$twu)
fast_sleep         <- fast_sleep[is.na(fast_sleep$Mass)==FALSE,]
fast_sleep$change  <- fast_sleep$Mass - fast_sleep$Last_fed_weight
fast_sleep$date    <- fast_sleep$Date_beginning + fast_sleep$Day_offset


fsn <- fast_sleep[ , unlist(lapply(fast_sleep, is.numeric)) == TRUE]
fsn <- fsn[ , -6]
cor( fsn)
pairs( fsn)

fast_sleep$Episode <- as.factor( fast_sleep$Episode)

ggplot(fast_sleep, aes( x = time, y = Mass, group = Episode, color = Episode)) +
  geom_line(size = 2)


mod1 <- lm( Mass ~ Last_fed_weight + twu + time , data = fast_sleep)
summary(mod1)
plot(mod1,1)
plot(predictorEffects(mod1,residuals=TRUE))
BIC(mod1)

### NLME

mod2 <- lme( Mass ~ Last_fed_weight + twu + time , 
             random = ~1|Episode,
             data = fast_sleep)

summary(mod2)
plot(mod2)
plot(predictorEffects(mod2,residuals=TRUE))
BIC(mod2)


#####
### Change (ez a kérdés voltaképpen...)

mod3 <- lme( change ~ twu + time, 
             random = ~1|Episode,
             data = fast_sleep)

summary(mod3)
plot(mod3)
plot(predictorEffects(mod3,residuals=TRUE))
BIC(mod3)

### Másképpen

mod4 <- lme( change ~ time, # + Last_fed_weight, 
             random = ~1|Episode,
             data = fast_sleep)

summary(mod4)
plot(mod4)
plot(predictorEffects(mod4,residuals=TRUE))
BIC(mod4)

### Splineokkal

mod5 <- lme( change ~ ns(time,4) + ns(twu,3) + Last_fed_weight, 
             random = ~1|Episode,
             data = fast_sleep)

# summary(mod5)
plot(mod5)
plot(predictorEffects(mod5,residuals=TRUE))
BIC(mod5)

#### Optimalizáljuk BIC-re

df <- expand.grid( time = 1:5,
                   twu  = 1:5,
                   lfw  = 1,
                   BIC  = 0)

for ( i in 1:nrow(df)) {
  f <- formula( paste( "change ~ ns(time,", 
                       df$time[i],
                       ") + ns(twu,", 
                       df$twu[i], 
                       ") + Last_fed_weight"))
  m <- lme( f, random = ~1|Episode, data = fast_sleep)
  df$BIC[i] <- BIC(m)
  
}

ggplot( df, aes( x = time, y = BIC, group = twu, color = as.factor(twu))) +
  geom_line(size=2)

mod5 <- lme( change ~ ns(time, 4) + ns(twu, 3) + Last_fed_weight, 
             random = ~1|Episode,
             data = fast_sleep)

# summary(mod5)
plot(mod5)
plot(predictorEffects(mod5,residuals=TRUE))
BIC(mod5)


### Splineokkal, change-re

df <- expand.grid( time = 1:5,
                   twu  = 1:4,
                   lfw  = 1:4,
                   BIC  = 0)

for ( i in 1:nrow(df)) {
  f <- formula( paste( "change ~ ns(time,", 
                       df$time[i],
                       ") + ns(twu,", 
                       df$twu[i], 
                       ") + ns(Last_fed_weight,",
                       df$lfw[i], ")"))
  m <- lme( f, random = ~1|Episode, data = fast_sleep)
  df$BIC[i] <- BIC(m)
  
}

ggplot( df, aes( x = time, y = BIC, group = twu, color = as.factor(twu))) +
  geom_line(size=2) +
  facet_wrap(facets="lfw")

mod7 <- lme( change ~ ns(time, 4)+ ns(twu, 4) + ns(Last_fed_weight, 3), 
             random = ~1|Episode,
             data = fast_sleep)

summary(mod7)$BIC
plot(mod7)
plot(predictorEffects(mod7,residuals=TRUE))

#####
### Splineokkal, nem change-re

df <- expand.grid( time = 1:5,
                   twu  = 1:4,
                   lfw  = 1:5,
                   BIC  = 0)

for ( i in 1:nrow(df)) {
  f <- formula( paste( "Mass ~ ns(time,", 
                       df$time[i],
                       ") + ns(twu,", 
                       df$twu[i], 
                       ") + ns(Last_fed_weight,",
                       df$lfw[i], ")"))
  m <- lme( f, random = ~1|Episode, data = fast_sleep)
  df$BIC[i] <- BIC(m)
  
}

ggplot( df, aes( x = time, y = BIC, group = twu, color = as.factor(twu))) +
  geom_line(size=2) +
  facet_wrap(facets="lfw")

mod6 <- lme( Mass ~ ns(time, 4) + ns(Last_fed_weight,3) + ns(twu, 4) + ns(date,3), 
             random = ~1|Episode,
             data = fast_sleep)

summary(mod6)$BIC
plot(mod6)
plot(predictorEffects(mod6,residuals=TRUE))


plot(fast_sleep$date,fast_sleep$time)


