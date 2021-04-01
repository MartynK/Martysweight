library(readr)
library(ggplot2)
library(splines)

feed <- read_delim("B:/R stuff/Copy of feeding.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE,locale=locale(decimal_mark = ","))
# View(Copy_of_feeding)

feed <- as.data.frame(feed)

feed$tseom <- as.numeric(as.POSIXct( feed[,1], format = "%Y.%m.%d %H:%M") - as.POSIXct( feed[8,1], format = "%Y.%m.%d %H:%M"))
feed$tseom <- feed$tseom + 1140

ggplot( feed[feed$tseom > -3000 & is.na(feed$`Blood Glucose`) == FALSE,], aes(x = tseom, y = `Blood Glucose`)) +
  geom_point() +
  geom_smooth(formula=y~x,method='loess',span=.15)

feed$bg <- feed$`Blood Glucose`
mod <- lm( bg ~ ns(tseom, df = 8), feed)

plot(effects::predictorEffects(mod, residuals = TRUE))

f2 <- feed[complete.cases(feed[,c(9,10)]) == TRUE & feed$Period == 1,]

x <- seq(-2500,75000,length.out = 100)

y <- predict(smooth.spline( f2$bg ~ f2$tseom),x= x)$y

plot(x,y,type='l')
points(f2$tseom,f2$`Blood Glucose`)

ggplot( feed[is.na(feed$Mass) == FALSE & feed$tseom > 0,], 
        aes(x = tseom, y = Mass)) +
  geom_point() +
  geom_smooth(formula=y~x,method='loess',span=.8)

f3 <- feed[complete.cases(feed[,c(9,10)]) == TRUE & feed$Period == 2,]

x <- seq(82000,170000,length.out = 100)

y <- predict(smooth.spline( f3$bg ~ f3$tseom),x= x)$y

plot(x,y,type='l', ylim = c(3,11))
points(f3$tseom,f3$`Blood Glucose`)
abline(h=7.8, col = 'red') # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4749644/ Nem jó a finger prick

feed$tseom[feed$Period == 2] <- feed$tseom[feed$Period == 2] - feed$tseom[feed$Period == 2][1]


ggplot( feed[feed$tseom > -3000 & is.na(feed$`Blood Glucose`) == FALSE,], 
        aes(x = tseom, y = `Blood Glucose`, group = Period, color = Period)) +
  geom_point() +
  geom_smooth(formula=y~x,method='loess',span=.2)






