library(readr)
library(ggplot2)
library(dplyr)

def_th <- theme_bw() +
  theme( legend.position = "none",
         plot.caption = element_text( vjust = 0,
                                      hjust = 0,
                                      margin(t=15)),
         text = element_text( family = "serif"))

feed <- read_delim(here::here("Fast_2x24h_OGTT_study/Copy of feeding.csv"), 
                              ";", escape_double = FALSE, trim_ws = TRUE,locale=locale(decimal_mark = ","))

feed <- as.data.frame(feed)

feed$tseom <- as.numeric(as.POSIXct( feed[,1], format = "%Y.%m.%d %H:%M") - as.POSIXct( feed[8,1], format = "%Y.%m.%d %H:%M"))
feed$tseom <- feed$tseom + 1140
feed$tseom <- feed$tseom / 3600

feed[feed$tseom > -10 ,] %>%
  .[is.na(.$`Blood Glucose`) == FALSE,] %>%
    .[.$Period != 2,] %>%
ggplot( ., aes(x = tseom, y = `Blood Glucose`)) +
  def_th +
  geom_point(color = "dodgerblue", size = 2.5) +
  stat_summary(aes(y = `Blood Glucose`), fun=mean, colour="grey50", geom="line", size = 1.3,  linetype = 'dashed') +
  geom_hline(yintercept = 3.5, color = 'salmon4', linetype = "dashed") +
  geom_hline(yintercept = 11, color = 'salmon4') +
  xlab("Time since the end of meal (hours)") +
  ylab("Blood Glucose (mmol/l)") +
  scale_y_continuous( breaks = c(3.5, 5.6, 7, 11)) +
  geom_label( label = "Sleeping", aes(x =x,y = y), 
              data = data.frame(x=10.5,y=3.5), 
              hjust = 0, vjust = 0)


feed %>%
  filter( tseom > -10,
          is.na(`Blood Glucose`) == FALSE,
          Period == 2) %>%
  ggplot( ., aes(x = tseom, y = `Blood Glucose`)) +
  def_th +
  geom_point(color = "dodgerblue", size = 2.5) +
  stat_summary(aes(y = `Blood Glucose`), fun=mean, colour="grey50", geom="line", size = 1.3,  linetype = 'dashed') +
  geom_hline(yintercept = 3.5, color = 'salmon4', linetype = "dashed") +
  geom_hline(yintercept = 11, color = 'salmon4') +
  xlab("Time since the end of meal (hours)") +
  ylab("Blood Glucose (mmol/l)") +
  scale_y_continuous( breaks = c(3.5, 5.6, 7, 11)) +
  geom_label( label = "Sleeping", aes(x =x,y = y), 
              data = data.frame(x=10.5,y=3.5), 
              hjust = 0, vjust = 0)


