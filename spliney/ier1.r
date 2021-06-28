library(readxl)
library(splines)
library(effects)
library(nlme)
library(ggplot2)

setwd("~/R/Projects/Martysweight/spliney")

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


knot <- c()
dayswithdp <- 0
days <- floor( dat$numd)
for (i in 1:max(floor(dat$numd))) {
  if(dayswithdp>1) {
    knot <- c(knot,i)
    dayswithdp <- 0
  }
  dayswithdp <- ifelse(i %in% days, dayswithdp+1, dayswithdp)
}
knot <- knot[knot<max(dat$numd) - 1]


mod <- lm( Mass ~ ns( fastd, df = 2) + ns(numd, knots=knot), dat)

summary(mod)
car::vif(mod)
plot(predictorEffects(mod, residuals = TRUE))


mod <- lme( Mass ~ ns( fastd, df = 2) 
            + ns(numd, knots=c(4,7,38,41,44,47,53,56,59,62,65)), #!!! 
            random = ~1|Episode,
            correlation = corAR1( ),
            control =  lmeControl(maxIter = 1000,msMaxIter = 3000,
                                  niterEM = 200,
                                  msVerbose = TRUE,
                                  tolerance = 1e-3,
                                  msTol = 1e-3),
            data = dat)

plot(ranef(mod))
hist(ranef(mod)[,1])
#hist(ranef(mod)[,2])

summary(mod)
plot(predictorEffects(mod, residuals = TRUE))
plot(mod)
ggpubr::ggqqplot(residuals(mod))

BIC(mod)

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
                      numd = c(seq(1,10,.1),seq(35,67,.1)),
                      Episode = 14)
dat_p$pred <- predict(mod, newdata = dat_p)

dat_p$numd <- as.Date( dat_p$numd, origin = "2021-04-13")
dat$Date2  <- as.Date( dat$numd,   origin = "2021-04-13")

ggplot(dat_p, aes(x=numd, y = pred, group = fastd, color = fastd)) +
  theme_bw() +
  geom_point(size=1) +
  geom_point(data = dat, aes(x = Date2, y = Mass, color=fastd), size = 1) +
  scale_y_continuous(limits = c(80,87)) +
  geom_vline(xintercept = knot) +
  scale_x_date(date_breaks = "1 week",date_minor_breaks = "1 week", date_labels = "%m-%d") + 
  labs( x = "Date",
        y = "Mass")


dat_d <-  expand.grid( fastd = c(8),
                       numd = c(seq(1,10,.1),seq(35,67,.1)),
                       Episode = 14)
dat_d$pred <- predict(mod, newdata = dat_d)

dat_d$dif <- 0
for ( i in 2:nrow(dat_d)) {
  dat_d$dif[i] <- (dat_d$pred[i] - dat_d$pred[i-1]) / (as.numeric(dat_p$numd[i]) - as.numeric(dat_p$numd[i-1]))
  
}

plot( dat_d$numd, dat_d$dif)
abline(a=0,b=0)

window <- 1




