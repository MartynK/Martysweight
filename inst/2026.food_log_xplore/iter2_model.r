# iter2_model.r — Model intra-day intake dynamics
#
# Question: do the DYNAMICS of eating differ by week? By weekend/weekday?
# Model: cum_prop ~ ns(time_frac, df = 2) * week_id + weekend
#        (per macro; week-interaction dropped if only one week of data)
#
# Effect plots: fitted cum_prop curves for each (week_id, weekend) combination.
# An ANOVA on the week interaction is dumped to model_summaries.txt as a
# significance check.

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(splines)
library(here)
library(scales)

OUT_DIR  <- here::here("inst", "2026.food_log_xplore")
dat_long <- readRDS(file.path(OUT_DIR, "dat_long.rds"))

# Week ID as a factor, alphabetically ordered (ISO week number)
dat_long <- dat_long %>%
  mutate(week_id = factor(paste0("W", sprintf("%02d", iso_week))))

# ---- Fit lm per macro & predict on a fine grid ----

fit_macro <- function(df) {
  single_week <- n_distinct(df$week_id) < 2

  fm <- if (single_week) {
    cum_prop ~ ns(time_frac, df = 2) + weekend
  } else {
    cum_prop ~ ns(time_frac, df = 2) * week_id + weekend
  }

  mod <- lm(fm, data = df)

  grid <- expand.grid(
    time_frac = seq(0, 1, by = 0.02),
    week_id   = levels(droplevels(df$week_id)),
    weekend   = c("weekday", "weekend"),
    stringsAsFactors = FALSE
  )
  grid$pred <- predict(mod, newdata = grid)

  list(model = mod, grid = grid, formula = format(fm))
}

macros_list <- split(dat_long, dat_long$macro)
fits        <- lapply(macros_list, fit_macro)

effect_df <- lapply(names(fits), function(m) {
  cbind(fits[[m]]$grid, macro = m)
}) %>% bind_rows() %>%
  mutate(macro = factor(macro, levels = levels(dat_long$macro)))

# ---- Effect plot: fitted curves per week, dashed = weekend ----

fig_eff <- effect_df %>%
  ggplot(aes(x = time_frac, y = pred,
             group = interaction(week_id, weekend),
             color = week_id,
             linetype = weekend)) +
  theme_bw() +
  geom_line(linewidth = 0.9) +
  facet_wrap(~macro, ncol = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x        = "Fraction of day elapsed",
    y        = "Fitted cumulative % of daily total",
    color    = "ISO week",
    linetype = "Day type",
    title    = "Modelled intra-day intake dynamics",
    subtitle = "cum_prop ~ ns(time_frac, df = 2) * week + weekend  [per macro]"
  )

ggsave(file.path(OUT_DIR, "effects_by_week.png"),
       fig_eff, width = 12, height = 8, dpi = 300)

# ---- Dump summaries + ANOVA ----

sink(file.path(OUT_DIR, "model_summaries.txt"))
for (m in names(fits)) {
  cat("\n================ ", m, " ================\n")
  cat("Formula: ", fits[[m]]$formula, "\n\n")
  print(summary(fits[[m]]$model))
  cat("\n-- ANOVA (sequential; week interaction at end) --\n")
  print(anova(fits[[m]]$model))
}
sink()

saveRDS(fits,      file.path(OUT_DIR, "fits.rds"))
saveRDS(effect_df, file.path(OUT_DIR, "effect_df.rds"))

message("Done. Outputs in: ", OUT_DIR)
