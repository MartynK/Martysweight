library(readr)
library(splines)
library(effects)
studtab2 <- read_delim(here::here("inst", "extdata", "studtab2.csv"), 
                       ";", escape_double = FALSE, trim_ws = TRUE)
studtab2$sex <- as.factor(studtab2$sex)
#View(studtab2)


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
anova(mod,mod2)
summary(mod2)
plot(mod2)
plot(predictorEffects(mod2))

pr <- expand.grid( sex = "M",
                   length = c(1,2,3,4),
                   wpr = 77.8)

predict(mod2,newdata=pr,se.fit=TRUE)
