library(nlme)
library(ggplot2)
library(dplyr)

cal <- readxl::read_xlsx("scales_calib.xlsx")
cal$Situation <- as.factor( cal$Situation)
cal$Offset <- cal$Offset/1000

cal$rep <- ""
cal$repno <- 0

nrep <- cal %>%
  group_by(Offset,Situation) %>% 
  slice(1)


for (i in 1:nrow(nrep)){
  cal$rep[cal$Situation == nrep$Situation[i] &
            cal$Offset == nrep$Offset[i]] <- i
  cal$repno[cal$Situation == nrep$Situation[i] &
              cal$Offset == nrep$Offset[i]] <- 1:nrow(  cal[cal$Situation == nrep$Situation[i] &
                                  cal$Offset == nrep$Offset[i],])
}
cal$rep <- as.factor(cal$rep)

mod_ols <- lm( Measurement ~ Offset + Situation + repno, cal)

plot(effects::predictorEffects(mod_ols,residuals=TRUE))

plot(mod_ols,1)

cal$wts <- 1/fitted(lm(abs(residuals(mod_ols)) ~ cal$Measurement))^2

ggplot(cal, aes(x=Measurement)) +
  geom_point(aes(y= sqrt(1/wts))) +
  geom_smooth(aes(y = sqrt(1/wts)))

wts_mod <- lm(sqrt(1/wts) ~ Measurement,cal)
plot(wts_mod,4) # Reported utlier is not in fact an utlier (Obs.No.1?)

mod_wls <- lm( Measurement ~ Offset + Situation + repno, cal, weights = wts)
mod_wls_red <- lm( Measurement ~ Offset + Situation, cal[cal$repno ==1,], weights = wts)
#!!!!!!!!!!!!!!
#mod_wls <- mod_wls_red

plot(effects::predictorEffects(mod_wls,residuals=TRUE))

plot(mod_wls,1)
acf(residuals(mod_wls))

summary(mod_ols)
summary(mod_wls)

predict( mod_ols, newdata = data.frame( Situation = "M", Offset = 0, repno = 1), 
         se.fit = TRUE, interval = "prediction")

predict( mod_wls, newdata = data.frame( Situation = "M", Offset = 0), 
         se.fit = TRUE, interval = "prediction", weights = cal$wts[1])

######
# #Some whacky attempt for gls() but lost heart when trying to extract the pred. interval
# mod_gls <- gls( Measurement ~ Offset + Situation, cal,
#                 weights = varExp())
# 
# plot(mod_gls)
# plot(effects::predictorEffects(mod_gls,residuals=TRUE))
# plot(effects::predictorEffect("Offset",mod_gls,residuals=TRUE),ylim=c(70,90))
# 
# predict( mod_gls, newdata = cal[1,], se.fit=TRUE) # doesn't work
# 
# emmeans::emmeans(mod_gls,specs="Situation",by="Offset",at=list(Offset=c(0)))
# 
# emmeans::emmeans(mod_gls,specs="Situation",by="Offset",at=list(Offset=c(0))) %>%
# #plot(lme4 gls.,PIs=TRUE)
# 
# AICcmodavg::predictSE(mod_gls,newdata = cal[1,])
######

newd <- cal[0,-2]
newd <- rbind( newd, expand.grid( Situation = c("M","Zs","S"),
                                     Offset = seq( -22, 27, 
                                                   length.out = 300),
                                  rep = as.factor("1"),
                                  repno = 1))

pr_wght <- predict(mod_wls,newdata=data.frame(Situation=c("M","Zs","S"),
                                                Offset = 0,
                                              rep = as.factor("1"),
                                              repno = 1))
pr_wght[3] <- 0 # from its definition

cal$realweight <- ifelse( cal$Situation == "M", pr_wght[1], 
                            ifelse( cal$Situation == "Zs", pr_wght[2], 0)) +
                  cal$Offset

cal$res_wls <- cal$Measurement - predict( mod_wls, newdata = cal)

newd$realweight <- pr_wght[as.numeric(newd$Situation)] + newd$Offset

newd <- newd[newd$realweight>=0]

# They overlap slightly, fixing manually...
newd <- newd[ !(newd$Offset < -20.770 & newd$Situation == "Zs"),]
newd <- newd[ !(newd$Offset < -21.260 & newd$Situation == "M"),]
newd <- newd[ !(newd$Offset > 21.830 & newd$Situation == "Zs"),]

newd$pred_ols       <- predict( mod_ols,newdata = newd)
newd$pred_ols_lowci <- predict( mod_ols,
                                newdata = newd, 
                                se.fit = TRUE, 
                                interval = "prediction")$fit[,2]
newd$pred_ols_higci <- predict( mod_ols,
                                newdata = newd, 
                                se.fit = TRUE, 
                                interval = "prediction")$fit[,3]



newd$pred_wls       <- predict( mod_wls,newdata = newd)
newd$pred_wls_wght  <- 1/predict( wts_mod, newdata = data.frame( Measurement = newd$pred_wls))^2

newd$pred_wls_lowci <- predict( mod_wls,
                                   newdata = newd, 
                                   se.fit = TRUE, 
                                   interval = "prediction",
                                   weights = newd$pred_wls_wght)$fit[,2]

newd$pred_wls_higci <- predict( mod_wls,
                                 newdata = newd, 
                                 se.fit = TRUE, 
                                 interval = "prediction",
                                 weights = newd$pred_wls_wght
                                 )$fit[,3]

ggplot( newd, aes( x = realweight)) +
  theme_bw() +
  geom_point(aes(x = realweight, y = Measurement), data = cal, alpha = .25) +
  # geom_line(aes(y=pred_ols)) +
  # geom_line(aes(y=pred_ols_lowci)) +
  # geom_line(aes(y=pred_ols_higci)) +
  geom_line(aes(y=pred_wls)) +
  geom_line(aes(y=pred_wls_lowci), linetype = "dashed") +
  geom_line(aes(y=pred_wls_higci), linetype = "dashed") +
  xlim(0,110) +
  ylim(0,110)


ggplot( newd, aes( x = realweight)) +
  theme_bw() +
  geom_point(aes(x = realweight, y = Measurement), data = cal, alpha = .25) +
  geom_line(aes(y=pred_ols), color = "salmon") +
  geom_line(aes(y=pred_wls)) +
  xlim(0,110) +
  ylim(0,110)

 
ggplot( newd, aes( x = realweight)) +
  theme_bw() +
  geom_point(aes(x = realweight, y = Measurement), data = cal, alpha = .25) +
  geom_line(aes(y=pred_ols), color = "salmon", size = 1.1) +
  geom_line(aes(y=pred_wls), size = 1.1) +
  xlim(70,75) +
  ylim(70,75) 
  

ggplot( newd, aes( x = realweight)) +
  theme_bw() +
  geom_point(aes(x = realweight, y = res_wls), data = cal, alpha = .25) +
  geom_line(aes(y=pred_wls - pred_wls), color = "salmon4") +
  geom_line(aes(y=pred_wls_lowci - pred_wls), linetype = "dashed") +
  geom_line(aes(y=pred_wls_higci - pred_wls), linetype = "dashed") +
  xlim(0,110)

#plot(newd$pred_wls_higci-newd$pred_wls_lowci)

#acf...
acf(residuals(mod_wls))


mod_lmer <- lme( Measurement ~ Situation + Offset,
                 random =~1|rep, 
                  weights = varPower(),
                  cal)

mod_lmer <- lmer( Measurement ~ Situation + Offset + (1|rep), 
                 weights = wts,
                 cal)


acf(residuals(mod_lmer))
plot(mod_lmer)

plot(effects::predictorEffects(mod_lmer, residuals = TRUE))

confint(mod_lmer,method="boot")

plot(ranef(mod_lmer))

merpred <- merTools::predictInterval(mod_lmer,newdata = newd)

newd <- cbind( newd, merpred)

ggplot( newd, aes( x = realweight)) +
  theme_bw() +
  geom_point(aes(x = realweight, y = res_wls), data = cal, alpha = .25) +
  geom_line(aes(y= fit - realweight), color = "salmon4") +
  geom_line(aes(y= lwr - realweight), linetype = "dashed") +
  geom_line(aes(y= upr - realweight), linetype = "dashed") +
  xlim(0,110)


mod_lme <- gls( Measurement ~ Situation + Offset,
                #random = ~ 0 , 
                #random=list(rep = pdDiag(~ rep)),
                weights = varPower(form =~fitted(.)),
                #correlation = corCompSymm(form= ~rep),
                #method="ML",
                cal)
intervals(mod_lme)
intervals(mod_lme,which="fixed")
plot(ranef(mod_lme))

plot(Variogram(mod_lme))

plot(predictorEffects(mod_lme,residuals=TRUE))
plot(predictorEffect("Offset",mod_lme,residuals=TRUE), ylim = c(74,86))

mySumm <- function(.) { 
  i <- intervals(.)
  return(i$reStruct$rep$est.)
}
library(lmeresampler)
boo1 <- bootstrap(model = mod_lme, fn = mySumm, type = "reb", 
                  B = 100)

boot::boot.ci(boo1, index = 1, type=c("norm", "basic", "perc"))

## you can also examine the bootstrap samples graphically
plot(boo1, index = 1)

plot(mod_lme)
cal$res_lme <- residuals(mod_lme)
newd$lme_pred <- predict(mod_lme,newdata=newd)

# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions

Designmat <- model.matrix(formula(mod_lme)[-2], newd)
predvar <- diag(Designmat %*% vcov(mod_lme) %*% t(Designmat)) 
newd$lme_SE2 <- sqrt(predvar+mod_lme$sigma^2)

newd$lme_pred_lo <- with(newd, lme_pred - 1.96 * lme_SE2)
newd$lme_pred_hi <- with(newd, lme_pred + 1.96 * lme_SE2)


ggplot( newd, aes( x = realweight)) +
  theme_bw() +
  geom_point(aes(x = realweight, y = res_lme), data = cal, alpha = .25) +
  geom_line(aes(y= lme_pred - realweight), color = "salmon4") +
  geom_line(aes(y= lme_pred_hi - realweight), linetype = "dashed") +
  geom_line(aes(y= lme_pred_lo - realweight), linetype = "dashed") +
  xlim(0,110)

library(ggeffects)
#ggpredict(mod_lme, "Measurement", type = "re") %>% plot(rawdata = T, dot.alpha = 0.2)


# 
# mod_gls <- gls(Measurement ~ Situation + Offset,
#                weights = varPower(),
#                correlation=corAR1(),
#                cal)
# 
# acf(residuals(mod_gls))


