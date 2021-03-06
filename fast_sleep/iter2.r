library(readr)
library(effects)
library(nlme)
library(ggplot2)
library(splines)
library(performance)
library(mgcv)
library(mgcViz)



fast_sleep      <- read_delim("fast_sleep_data.txt", 
                              "\t", escape_double = FALSE, locale = locale(date_format = "%Y.%m.%d", 
                                                                           decimal_mark = ",", 
                                                                           grouping_mark = ""), trim_ws = TRUE)
  
  
  # read_delim("~/V2 Docs/Bistat képzés/Misc/fasting/fast_sleep_data.txt", 
  #                             "\t", escape_double = FALSE, locale = locale(decimal_mark = ","), 
  #                             trim_ws = TRUE)

#write.table(fast_sleep, file = "data.txt", sep = ";", dec = ",")
#View(fast_sleep)

fast_sleep$time <- as.numeric(
                     fast_sleep$Day_offset * 24 * 3600 + 
                     fast_sleep$Time_of_measurement - 
                     fast_sleep$Finished_eating
                   ) / 3600

fast_sleep$twu     <- as.numeric(fast_sleep$Time_of_measurement - fast_sleep$Woke_up) / 3600
fast_sleep$twu     <- ifelse( fast_sleep$twu > fast_sleep$time, fast_sleep$time, fast_sleep$twu)
fast_sleep         <- fast_sleep[is.na(fast_sleep$Mass)==FALSE,]
fast_sleep$change  <- fast_sleep$Mass - fast_sleep$Last_fed_weight
fast_sleep$date    <- as.numeric( difftime( fast_sleep$Date_beginning + fast_sleep$Day_offset, fast_sleep$Date_beginning[1])/ (24*3600))
fast_sleep$Episode <- as.factor( fast_sleep$Episode)


### Descriptive stuff
#####
# fsn <- fast_sleep[ , unlist(lapply(fast_sleep, is.numeric)) == TRUE]
# cor( fsn)
# pairs( fsn)
# 
# ggplot(fast_sleep, aes( x = time, y = Mass, group = Episode, color = Episode)) +
#   geom_line(size = 2)


#####
### Splineokkal, nem change-re

Spline_BIC <- expand.grid( 
                   time = 1:4,
                   twu  = 0:4,
                   lfw  = 0:4,
                   dat  = 1:12,
                   interact = c(F),
                   BIC  = 0,
                   AIC  = 0,
                   f = "")
Spline_BIC$f <- as.character(Spline_BIC$f)
pb   <- txtProgressBar()

for ( i in 1:nrow(Spline_BIC)) {
  
  ##### 
  ###Generáljuk a "formulát"
  f1 <- ifelse( Spline_BIC$time[i] == 0,
                paste( "Mass ~ "),
                paste( "Mass ~ ns(time,", 
                       Spline_BIC$time[i],
                      ")"))
  f2 <- ifelse( Spline_BIC$interact[i] == TRUE, "*", "+")
  
  f3 <- ifelse( Spline_BIC$dat[i] == 0, "",
                paste( " ns(date,", 
                       Spline_BIC$dat[i],")"))

  f4 <- ifelse( Spline_BIC$lfw[i] == 0, "",
                paste( " + ns(Last_fed_weight,", 
                       Spline_BIC$lfw[i],")"))
  
  f5 <- ifelse( Spline_BIC$twu[i] == 0, "",
                paste( " + ns(twu,", 
                       Spline_BIC$twu[i],")"))
  

  f <- paste(f1,f2,f3,f4,f5)
  Spline_BIC$f[i] <- f
  f       <- as.formula(f)
  #####
  
  .b <- -1000
  .a <- -1000
  try( silent = TRUE, expr = {
    m <- lme( f, 
              random = ~1|Episode, 
              data = fast_sleep,
              weight = varExp()
              #corARMA(p=2,q=2,form=~time)
              #corAR1( form = ~ time)
                      )
    .b <- BIC(m)
    .a <- AIC(m)
  })
  
  Spline_BIC$BIC[i] <- .b
  Spline_BIC$AIC[i] <- .a
  setTxtProgressBar(pb, i/nrow(Spline_BIC))
  
}
close(pb)
Spline_BIC <- Spline_BIC[Spline_BIC$BIC != -1000,]

ggplot( Spline_BIC, aes( x = time, y = BIC, group = twu, color = as.factor(twu))) +
  geom_point(size=2) +
  facet_wrap(facets="lfw")
  
  mod <- lme( as.formula( Spline_BIC$f[Spline_BIC$BIC==sort(Spline_BIC$BIC)[1]]), 
               random = ~1|Episode,
               #correlation = corARMA(p=2,q=2,form=~time), 
               #corExp( form = ~time),  
               #corCompSymm(form = ~ time), 
               #corAR1( form = ~ time ),
               weights = varExp(),
               data = fast_sleep)
  
  Spline_BIC$f[Spline_BIC$AIC==sort(Spline_BIC$AIC)[1]]
summary(mod)$BIC
plot(mod)
check_collinearity(mod)
#hist(mod$residuals)
ks.test(mod$residuals, y = pnorm)
#hist(ranef(mod)[[1]])
ks.test(ranef(mod)[[1]], y = pnorm)
plot(predictorEffects(mod,residuals=TRUE))

sp <- Spline_BIC[Spline_BIC$time == 1 & Spline_BIC$twu == 0 & Spline_BIC$lfw == 1,]
sp <- mutate(sp, 
             BICr = BIC/sp$BIC[sp$dat ==1],
             AICr = AIC/sp$AIC[sp$dat ==1],)
sp  %>%
  ggplot( data = ., aes( x = dat, y = BICr)) +
  geom_line() +
  geom_point() +
  geom_line( aes( y = AICr)) +
  geom_point( aes( y = AICr)) +
  geom_hline(yintercept = 1, color = "salmon")

#plot(fast_sleep$date,fast_sleep$time)
#plot(Spline_BIC[,3],Spline_BIC$BIC)

#####



mod2a <- gam(Mass ~ s(time, bs = "ts") + s(date, bs = "ts") + s(Last_fed_weight, bs = "ts") + s(twu, bs = "ts"), data = fast_sleep)
mod2a
viz <- getViz(mod2a)

p <- plot(viz, allTerms = T) +
  theme_bw() +
  l_points() +
  l_fitLine() +
  l_ciLine(colour = 2)

print(p, pages = 1)

qq(viz, rep = 50, showReps = T, CI = "none", 
   a.qqpoi = list("shape" = 19), 
   a.replin = list("alpha" = 0.1)) +
  title("QQplot of the deviance residuals based on the GAM")

# refitting without twu
mod2 <- gam(Mass ~ s(time, bs = "ts") + s(date, bs = "ts") + s(Last_fed_weight, bs = "ts"), data = fast_sleep)
mod2
summary(mod2)
plot(mod2)
#
gam.check(mod2)


plot(predict(mod2), predict(mod2a))
plot(predict(mod), predict(mod2))
abline(a=0,b=1)
anova(mod2)
anova(mod2,mod2a)



library(visreg)
visreg2d(mod2, xvar= 'time', yvar='date', scale='linear')
visreg2d(mod2, xvar= 'time', yvar='Last_fed_weight', scale='linear')
visreg2d(mod2, xvar= 'Last_fed_weight', yvar='date', scale='linear')


