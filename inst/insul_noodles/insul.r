library(readxl)
library(dplyr)
library(ggplot2)
library(splines)
library(nlme)

dat <- here::here("insulindata.xlsx") %>% 
                 read_excel(range = "A1:E85") %>%
                 mutate( food = as.factor(food),
                         variable = as.factor(variable))

dat2 <- dat %>%
          filter( variable == "glucose_mg_dl") %>%
          left_join( ., y = dat %>% 
                           filter( variable == "insulin_pm_l"),
                     by = c("t","food"))


mod <- lm( measure.y ~ measure.x + ns(t, df = 3) + food
            #,correlation = corAR1( form = ~ t | food) # phi = 0
            ,dat2)

summary(mod)

# plot(mod)

mod %>% resid %>% acf
mod %>% resid %>% pacf


mod %>% 
  effects::predictorEffects( partial.residuals = TRUE) %>% 
  plot


