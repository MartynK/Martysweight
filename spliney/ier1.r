library(readxl)
library(splines)
library(effects)
library(nlme)
library(ggplot2)
library(dplyr)
library(MuMIn)

#setwd("~/R/Projects/Martysweight/spliney")

dat <- read_excel("martysweight.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric","numeric"))


dat$Episode <- as.factor(dat$Episode)

#View(dat)

dat$numd <- as.numeric( dat$Date 
                        + dat$Time_hours * 3600 
                        + dat$Time_minutes * 60
                        - min(dat$Date)) / 24
dat$fastd <- dat$Time_fasted_hours + dat$Time_fasted_minutes / 60

wind <- 1
dat$maxinlast <- 0

for (i in 1:nrow(dat)) {
  dat$maxinlast[i] <- dat$fastd[dat$numd > dat$numd[i] - wind &
                            dat$numd < dat$numd[i]] %>%
                        max(.)
}
dat$maxinlast[dat$maxinlast == -Inf] <- 0

dat <- dat %>%
         arrange(., numd)

knot <- c()
dayswithdp <- 0
days <- floor( dat$numd)
for (i in seq(1,max(floor(dat$numd)))) {
  if(dayswithdp>1) {
    knot <- c(knot,i)
    dayswithdp <- 0
  }
  dayswithdp <- ifelse(i %in% days, dayswithdp+1, dayswithdp)
}
#knot <- knot[knot<max(dat$numd) - 1]


mod <- lm( Mass ~ ns( fastd, df = 2) + ns(numd, knots=knot), dat)

summary(mod)
car::vif(mod)
plot(predictorEffects(mod, residuals = TRUE))

plot(cooks.distance(mod))
hist(cooks.distance(mod))
q <- quantile(cooks.distance(mod), probs = .99)
abline(v=q, col = 'red')
dat_full <- dat
dat  <- dat[ cooks.distance(mod) < q, ] # removing outliers

expre <- paste(knot,collapse=",") %>%
  paste( "c(", ., ")") %>%
    paste("Mass ~ ns( fastd, df = 2) + ns(numd, knots =", ., ")" 
          ) %>%
      as.formula(.)

mod <- lme( expre,
            random = ~0+fastd|Episode,
            #random = ~1|Episode,
            correlation = corAR1( form = ~1|Episode),
            control =  lmeControl(maxIter = 2000,
                                  msMaxIter = 3000,
                                  niterEM = 400,
                                  msVerbose = TRUE,
                                  tolerance = 1e-6,
                                  msTol = 1e-6),
            data = dat)

plot(ranef(mod))
hist(ranef(mod)[,1])
#hist(ranef(mod)[,2])
intervals(mod)

summary(mod)
plot(predictorEffects(mod, residuals = TRUE))
plot(mod)
ggpubr::ggqqplot(residuals(mod))

predictmeans::CookD(mod,newwd = FALSE)

BIC(mod)
r.squaredGLMM(mod)

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
                      Episode = 14)
dat_p$pred <- predict(mod, newdata = dat_p)

dat_p$numd <- as.Date( dat_p$numd, origin = "2021-04-11")
dat$Date2  <- as.Date( dat$numd,   origin = "2021-04-11")

ggplot(dat_p, aes(x=numd, y = pred, group = fastd, color = fastd)) +
  theme_bw() +
  geom_line(size=.7) +
  geom_point(data = dat, aes(x = Date2, y = Mass, color=fastd, group = Episode), size = 1) +
  geom_line(data = dat, aes(x = Date2, y = Mass, group = Episode), size = .5, color = "grey50") +
  scale_y_continuous(limits = c(80,87)) +
  geom_vline(xintercept = knot) +
  scale_x_date(date_breaks = "1 week",date_minor_breaks = "1 week", date_labels = "%m-%d") + 
  labs( x = "Date",
        y = "Mass")


dat_d <-  expand.grid( fastd = c(8),
                       numd = c(seq(1,10,.1),seq(35,max(dat$numd),.1)),
                       Episode = 14)
dat_d$pred <- predict(mod, newdata = dat_d)

dat_d$dif <- 0
dat_d$maxinlast <- 0
for ( i in 2:nrow(dat_d)) {
  dat_d$dif[i] <- (dat_d$pred[i] - dat_d$pred[i-1]) / (as.numeric(dat_p$numd[i]) - as.numeric(dat_p$numd[i-1]))
  dat_d$maxinlast[i] <- max(dat$fastd[dat$numd > dat_d$numd[i] - 2 &
                                        dat$numd <= dat_d$numd[i]])
}
dat_d$dif[dat_d$dif==-Inf|
            dat_d$dif==Inf] <- 0
dat_d$maxinlast[dat_d$maxinlast==-Inf] <- 0

summary(lm(dat_d$dif~dat_d$maxinlast))

plot( dat_d$numd, dat_d$dif, pch = 19, type= 'b')
abline(a=0,b=0, col = 'red')

plot( dat_d$numd, dat_d$maxinlast, 
      pch = 19, type= 'b',
      ylim = c(-5,26))
points(dat_d$numd, dat_d$pred-mean(dat_d$pred))




