library(readr)
library(splines)
library(nlme)
library(effects)
library(dplyr)
library(ggplot2)

studtab2 <- readr::read_delim(here::here("inst", "extdata", "studtab2.csv"), 
                       ";", escape_double = FALSE, trim_ws = TRUE)
studtab2$sex <- as.factor(studtab2$sex)

mod <- lm(wpost ~ (wpr + length) * sex, studtab2)
summary(mod)
anova(mod)
mod2 <- lm(wpost ~ wpr + length * sex, studtab2)
anova(mod,mod2)
summary(mod2)
#plot(mod2)
# No.1371
studtab2[1371,]
#obviously wrong
studtab2 <- studtab2[-1371,]
mod2 <- lm(wpost ~ wpr + length * sex, studtab2)
mod1_refit <- update(mod,data=studtab2)
anova(mod1_refit,mod2)
summary(mod2)
plot(mod2,3)

mod3 <- gls(wpost ~ wpr + length * sex, 
            data = studtab2,
            weights = varPower(form = ~ length),
            na.action = na.omit)

plot(predictorEffects(mod3))

pr <- expand.grid( sex = "M",
                   length = 0:10,
                   wpr = 86)

pr$pr <- predict(mod2,newdata=pr,se.fit=TRUE)$fit
pr$se <- predict(mod2,newdata=pr,se.fit=TRUE)$se.fit

pr %>%
  ggplot(aes(x= length, y = I(pr - 86))) +
    theme_bw() +
    geom_line() +
    scale_x_continuous(breaks = c(1,3,6,9))

