library(readxl)
library(splines)
library(effects)
library(nlme)
library(ggplot2)
library(dplyr)
library(MuMIn)
library(lubridate)

setwd(here::here())
#setwd("~/V2 Docs/R Git/Martysweight/spliney")

#####

k <- 2
#####
dat <- here::here("inst","extdata","martysweight.xlsx") %>%
   read_excel( col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric","numeric")) %>%
  #arrange(., numd) %>%
  mutate( #Episode = as.factor(Episode),
             numd = as.numeric( Date 
                        + Time_hours * 3600 
                        + Time_minutes * 60
                        - min(Date)) / 24,
            fastd = Time_fasted_hours + Time_fasted_minutes / 60,
          fastd_truncated = ifelse( fastd > 16, 16, fastd),
        maxinlast = 0
  ) 

##########
# Episodes setup

# Assuming 'dat' is your data frame
dat <- dat %>%
  mutate(
    # Convert 'Time_fasted' to minutes
    Time_fasted_total_minutes = Time_fasted_hours * 60 + Time_fasted_minutes,
    
    # Calculate the time difference between consecutive observations in minutes
    Time_diff = c(NA, diff(as.numeric(Date)) * 24 * 60 + diff(Time_hours) * 60 + diff(Time_minutes))
  )

# Initialize the first episode
dat$Episode[1] <- 1

# Loop through the rows to assign episodes
for(i in 2:nrow(dat)) {
  if(!is.na(dat$Time_diff[i]) && dat$Time_diff[i] <= dat$Time_fasted_total_minutes[i] + 30) {
    dat$Episode[i] <- dat$Episode[i-1]
  } else {
    dat$Episode[i] <- as.numeric(dat$Episode[i-1]) + 1
  }
}

# Convert 'Episode' back to factor if needed
dat$Episode <- as.factor(dat$Episode)



#########


dat_0mass <- dat %>%
               group_by( Episode) %>%
               arrange( numd) %>%
               slice(1) %>%
               filter(fastd < 2) %>% 
               select(Episode)


dat <- dat %>%
  # calculating change
  group_by(Episode) %>%
  mutate( mass_change =  Mass - first( Mass, fastd),
          mass_change = ifelse( Episode %in% dat_0mass$Episode, mass_change, NA))

wind <- 1

for (i in 1:nrow(dat)) {
  dat$maxinlast[i] <- dat$fastd[dat$numd > dat$numd[i] - wind &
                            dat$numd < dat$numd[i]] %>%
                        max(.)
}
dat$maxinlast[dat$maxinlast == -Inf] <- 0


# Producing a nice plot to investigate change
dat %>%
  ggplot( aes( x = fastd, y = mass_change, color = numd)) +
  theme_bw() +
  geom_point() +
  geom_line(mapping= aes(group = Episode)) +
  geom_smooth(formula = y~x, method = 'loess', span = .20, color = "pink", se = FALSE, size = 2) +
  geom_smooth(formula = y~x, method = 'gam', color = "salmon4", se = FALSE, size = 2)

  
modch <- lm( mass_change ~ fastd,
             dat)

acf( residuals( modch))
plot( modch, 3)

modch2 <- lme( mass_change ~ fastd,
               random = ~0+fastd|Episode,
               #weights = varPower(),
               #random = ~1|Episode,
               #correlation = corAR1( form = ~1|Episode, value = .13),
               control =  lmeControl(maxIter = 2000,
                                     msMaxIter = 3000,
                                     niterEM = 400,
                                     msVerbose = FALSE,
                                     tolerance = 1e-6,
                                     msTol = 1e-6),
               data = dat %>% filter( is.na(mass_change) == FALSE))

ACF(modch2) %>% plot
plot(modch2)
ggpubr::ggqqplot( resid(modch2))


modch3 <- lme( mass_change ~ fastd,
               random = ~0+fastd|Episode,
               weights = varExp(), # varPower does not converge
               #random = ~1|Episode,
               #correlation = corAR1( form = ~1|Episode, value = .13),
               control =  lmeControl(maxIter = 2000,
                                     msMaxIter = 3000,
                                     niterEM = 400,
                                     msVerbose = FALSE,
                                     tolerance = 1e-6,
                                     msTol = 1e-6),
               data = dat %>% filter( is.na(mass_change) == FALSE))

ACF(modch3) %>% plot
plot(modch3)
ggpubr::ggqqplot( resid(modch3))

anova(modch2, modch3) # second model better


modch4 <- lme( mass_change ~ fastd,
               random = ~0+fastd_truncated|Episode,
               weights = varExp(), # varPower does not converge
               #random = ~1|Episode,
               #correlation = corAR1( form = ~1|Episode, value = .13),
               control =  lmeControl(maxIter = 2000,
                                     msMaxIter = 3000,
                                     niterEM = 400,
                                     msVerbose = FALSE,
                                     tolerance = 1e-6,
                                     msTol = 1e-6),
               data = dat %>% filter( is.na(mass_change) == FALSE))

anova(modch3, modch4) # second model better(?) - but harder to interpret




knot <- c()
dayswithdp <- 0
days <- floor( dat$numd)
for (i in seq(1,max(floor(dat$numd)))) {
  if(dayswithdp>=k) {
    knot <- c(knot,i)
    dayswithdp <- 0
  }
  dayswithdp <- ifelse(i %in% days, dayswithdp+1, dayswithdp)
}
#knot <- knot[knot<max(dat$numd) - 1]

expre <- paste(knot,collapse=",") %>%
  paste( "c(", ., ")") %>%
  paste("Mass ~ ns( fastd, df = 1) + ns(numd, knots =", ., ")" 
  ) %>%
  as.formula(.)

# mod <- gls( expre,
#             #random = ~0+fastd|Episode,
#             #random = ~1|Episode,
#             correlation = corAR1( form = ~1|Episode),
#             weights = varExp(),
#             control =  lmeControl(maxIter = 2000,
#                                   msMaxIter = 3000,
#                                   niterEM = 400,
#                                   msVerbose = FALSE,
#                                   tolerance = 1e-6,
#                                   msTol = 1e-6),
#             data = dat)
# 
# 
# summary(mod)
# car::vif(mod)

# cooks <- predictmeans::CookD(mod,
#               newwd = FALSE)
# 
# hist(cooks)
# q <- quantile(cooks, probs = .99)
# abline(v=q, col = 'red')
# dat_full <- dat
#dat  <- dat[ cooks < q, ] # removing outliers


mod <- lme( expre,
            random = ~ 0 + fastd_truncated|Episode,
            weights = varPower(),
            #correlation = corAR1( form = ~1|Episode, value = .13),
            control =  lmeControl(maxIter = 2000,
                                  msMaxIter = 3000,
                                  niterEM = 400,
                                  msVerbose = FALSE,
                                  tolerance = 1e-6,
                                  msTol = 1e-6),
            data = dat)

ACF(mod) %>% plot
# plot(ranef(mod))
# hist(ranef(mod)[,1])
#hist(ranef(mod)[,2])
intervals(mod)


plot(predictorEffects(mod, 
                      residuals = TRUE),
     ylim = c(79.5,86))


#summary(mod)
plot(mod)
ggpubr::ggqqplot(residuals(mod))


BIC(mod)
#r.squaredGLMM(mod)
rcompanion::nagelkerke(mod)

car::vif(mod)
anova(mod)


# ggplot(dat, aes(x=numd, y = Episode)) +
#   geom_point()
# 
# 
# ggplot(dat, aes(x=numd)) +
#   geom_histogram(binwidth = 1)
# 
# plot(ecdf(dat$numd))
# 
# plot(ecdf(floor(dat$numd)))


# dat_p <- dat
# dat_p$fastd <- 8
# dat_p$pred <- predict(mod, newdata = dat_p)
# dat_p$Episode <- 14

dat_p <- expand.grid( fastd = c(0,8,16,24),
                      numd = c(seq(1,10,.1),seq(35,max(dat$numd)+1,.1)),
                      maxinlast = 12,
                      Episode = 14) %>%
                      mutate( fastd_truncated = fastd)
dat_p$pred <- predict(mod, newdata = dat_p)

dat_p$numd <- as.Date( dat_p$numd, origin = "2021-04-11")
dat$Date2  <- as.Date( dat$numd,   origin = "2021-04-11")

ggplot(dat_p[dat_p$fastd==8,], 
       aes(x=numd, y = pred, group = fastd, color = fastd)) +
  theme_bw() +
  geom_line(size=.9) +
  stat_summary(data = dat, 
               aes(x = Date2, y = Mass, group = Episode),
               fun=mean, geom="line", colour="blue") +
  geom_point(data = dat, aes(x = Date2, y = Mass, color=fastd, group = Episode), size = 1) +
  #geom_line(data = dat, aes(x = Date2, y = Mass, group = Episode), size = .5, color = "grey50") +
  scale_y_continuous(limits = c(80,87)) +
  geom_vline(xintercept = knot) +
  scale_x_date(date_breaks = "1 month",date_minor_breaks = "2 week", date_labels = "%m-%d") + 
  labs( x = "Date",
        y = "Mass")


pic <- ggplot(dat_p, aes(x=numd, y = pred, group = fastd, color = fastd)) +
  theme_bw() +
  geom_line(size=.9) +
  geom_point(data = dat, aes(x = Date2, y = Mass, color=fastd, group = Episode), size = 1) +
  geom_line(data = dat, aes(x = Date2, y = Mass, group = Episode), size = .5, color = "grey50") +
  scale_y_continuous(limits = c(80,87)) +
  geom_vline(xintercept = knot) +
  scale_x_date(date_breaks = "1 week",date_minor_breaks = "1 week", date_labels = "%m-%d") + 
  labs( x = "Date",
        y = "Mass") +
  scale_x_date(date_breaks = "1 week",
               date_minor_breaks = "1 day", 
               date_labels = "%m-%d",
               limits = c(max(dat_p$numd)-14, max(dat_p$numd)))

pic

######

# dat %>%
#   group_by(Episode) %>%
#     summarise(minlast = max(fastd))

wind <- 0.01#.5
ste  <- 0.01

dat_d <-   expand.grid( fastd = c(0,8),
                        numd = c(seq(1,10,.01),seq(35,max(dat$numd),.01))) %>%
             mutate(fastd_truncated = fastd)
dat_d$pred <- predict(mod, newdata = dat_d, level = 0)
dat_d      <- dat_d[dat_d$fastd==8,]


dat_d$dif <- 0
dat_d$maxinlast <- 0
dat_d$mininlast <- 0
for ( i in 2:nrow(dat_d)) {
  #dat_d$dif[i] <- (dat_d$pred[i] - dat_d$pred[i-1]) / (as.numeric(dat_p$numd[i]) - as.numeric(dat_p$numd[i-1]))
  dat_d$dif[i] <- (dat_d$pred[i] - dat_d$pred[i-1]) / ste
  dat_d$maxinlast[i] <- max(dat$fastd[dat$numd >= dat_d$numd[i] - wind &
                                        dat$numd <= dat_d$numd[i]])
  dat_d$mininlast[i] <- min(dat$fastd[dat$numd >= dat_d$numd[i] - wind &
                                        dat$numd <= dat_d$numd[i]])
  
}
dat_d$dif[dat_d$dif > 4] <- 0
dat_d$maxinlast[dat_d$maxinlast==-Inf] <- NA
dat_d$maxinlast[dat_d$maxinlast==0] <- NA
dat_d$mininlast[dat_d$mininlast==Inf] <- NA
#dat_d$mininlast[dat_d$mininlast==0] <- NA

dat_d$miinlast <- dat_d$mininlast
summary(lm(dat_d$dif~dat_d$maxinlast))
summary(lm(dif~miinlast,
           dat_d[complete.cases(dat_d),]))


plot( dat_d$numd, dat_d$dif, pch = 19, type= 'b')
abline(a=0,b=0, col = 'red')

plot( dat_d$maxinlast, dat_d$dif,
      pch = 19, type= 'p')

cor( dat_d$dif, 
     dat_d$maxinlast,
     use = "complete.obs")

cor( dat_d$dif, 
     dat_d$mininlast,
     use = "complete.obs")

corrv <- c()
for (i in 0:300) {
  dat_d$dif2 <- NA
  dat_d$dif2[1:(nrow(dat_d)-i)] <- dat_d$dif[(i+1):nrow(dat_d)]
  dat_d$dif2[dat_d$dif2==0] <- NA
  corrv <-  c(corrv,
               cor(dat_d$dif2, 
                   dat_d$maxinlast,
                 use="complete.obs",
                 method="kendall"))
}
plot(corrv)
max(corrv)
min(corrv)
which( corrv == max(corrv))
which( corrv == min(corrv))

#####

i <- 100
dat_d$dif2 <- 0
dat_d$dif2[1:(nrow(dat_d)-i)] <- dat_d$dif[(i+1):nrow(dat_d)]
plot(dat_d$maxinlast,dat_d$dif2)

modch <- lm(dif2 ~ ns(maxinlast, 
                      df = 3),
            dat_d[complete.cases(dat_d),])

plot(modch,1)
summary(modch)

pr <- modch %>%
  effects::predictorEffects( residuals=TRUE) %>%
    plot()
pr

prtxt <- predict(modch,newdata = data.frame(maxinlast = seq(0,48,2)))
names(prtxt) <- seq(0,48,2)
prtxt


modch <- gls(dif2 ~ ns(maxinlast, 
                       df = 4),
             #correlation = corAR1(form = ~1|Episode),
             weights = varExp(),
             dat_d[complete.cases(dat_d),],
             control = lmeControl(maxIter = 200,
                                  msMaxIter = 300,
                                  niterEM = 10,
                                  msVerbose = FALSE,
                                  tolerance = 1e-6,
                                msTol = 1e-6))


plot(modch)
summary(modch)

cor(dat_d$dif2, dat_d$maxinlast,
    use="complete.obs")

anova(modch)

modch %>%
  predictorEffects(residuals=TRUE) %>%
    plot()

prtxt <- predict(modch,newdata = data.frame(maxinlast = seq(0,32,2)))
names(prtxt) <- seq(0,32,2)
prtxt


#####

dat_e <- dat %>%
           group_by(Episode) %>%
           slice_tail( n=1) %>%
           mutate( dif = dat_d$dif[round( numd) == dat_d$numd]#,
                   #dif_1 = dat_d$dif[ (round( numd) +1) == dat_d$numd]
                   )


with( dat_e, cor( fastd, dif))
with( dat_e, plot( fastd, dif))


modch <- lm(dif ~ ns(maxinlast, 
                      df = 3),
            dat_e)

modch <- gls(dif ~ ns(maxinlast, 
                       df = 4),
             # correlation = corARMA(form = ~1|Episode,
             #                       p = 1,
             #                       q = 0,
             #                       value = .2),
             weights = varExp(),
             dat_e,
             control = lmeControl(maxIter = 200,
                                  msMaxIter = 300,
                                  niterEM = 10,
                                  msVerbose = FALSE,
                                  tolerance = 1e-6,
                                  msTol = 1e-6))

plot(modch)#,1)
plot(pacf(resid(modch)))
plot(pacf(resid(modch, type = 'normalized')))


summary(modch)

pr <- modch %>%
  effects::predictorEffects( residuals=TRUE) %>%
  plot()
pr

prtxt <- predict(modch,newdata = data.frame(maxinlast = seq(0,48,2)))
names(prtxt) <- seq(0,48,2)
prtxt





