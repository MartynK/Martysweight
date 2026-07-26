# iter1_explore.r — Intra-day macro intake burndown / cumulative curves
#
# The food log has no timestamps, so within each date the items are ordered by
# row index. `time_frac = (item_idx - 1) / (n_items - 1)` maps 0 = first item
# of the day, 1 = last item. Cumulative intake per macro is then computed per
# day and faceted across macros (+ sat fat, sugar, free sugar, kcal).

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(here)
library(scales)

OUT_DIR <- here::here("inst", "2026.food_log_xplore")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

path <- here::here("inst", "extdata", "2026.food_logging",
                   "macro_tracking_week3.xlsx")
dat_raw <- read_excel(path, sheet = "Daily Items")

# Strip units from column names
names(dat_raw) <- c("day", "date", "item", "protein", "fat", "sat_fat",
                    "carbs", "sugar", "free_sugar", "kcal", "zone")

MACROS <- c("protein", "fat", "sat_fat", "carbs", "sugar", "free_sugar", "kcal")

MACRO_LABELS <- c(
  protein    = "Protein (g)",
  fat        = "Fat (g)",
  sat_fat    = "Sat fat (g)",
  carbs      = "Carbs (g)",
  sugar      = "Sugar (g)",
  free_sugar = "Free sugar (g)",
  kcal       = "kcal"
)

# ---- Compute time_frac per day ----

dat <- dat_raw %>%
  mutate(date = as.Date(date)) %>%
  # Each day has a trailing "TOTAL" row holding the daily sum; drop it,
  # otherwise the totals get double-counted.
  filter(!is.na(date), day != "TOTAL") %>%
  arrange(date) %>%
  group_by(date) %>%
  mutate(
    item_idx  = row_number(),
    n_items   = n(),
    time_frac = if_else(n_items == 1, 0.5,
                        (item_idx - 1) / (n_items - 1))
  ) %>%
  ungroup()

# ---- Long format + cumulative per day per macro ----

dat_long <- dat %>%
  select(date, day, time_frac, all_of(MACROS)) %>%
  pivot_longer(all_of(MACROS), names_to = "macro", values_to = "value") %>%
  mutate(value = replace_na(value, 0)) %>%
  group_by(date, macro) %>%
  arrange(time_frac, .by_group = TRUE) %>%
  mutate(
    cum       = cumsum(value),
    day_total = sum(value),
    cum_prop  = if_else(day_total == 0, 0, cum / day_total),
    iso_week  = isoweek(date),
    weekend   = if_else(wday(date, week_start = 1) >= 6,
                        "weekend", "weekday")
  ) %>%
  ungroup() %>%
  mutate(macro = factor(macro,
                        levels = names(MACRO_LABELS),
                        labels = MACRO_LABELS))

saveRDS(dat_long, file.path(OUT_DIR, "dat_long.rds"))

# ---- Plot 1: absolute cumulative intake ----

fig_abs <- dat_long %>%
  ggplot(aes(x = time_frac, y = cum,
             group = date, color = factor(date))) +
  theme_bw() +
  geom_line(alpha = 0.7) +
  geom_point(size = 0.7, alpha = 0.5) +
  facet_wrap(~macro, scales = "free_y", ncol = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x     = "Fraction of day elapsed (item order)",
    y     = "Cumulative intake",
    color = "Date",
    title = "Intra-day cumulative macro intake — absolute"
  )

ggsave(file.path(OUT_DIR, "burndown_absolute.png"),
       fig_abs, width = 12, height = 8, dpi = 300)

# ---- Plot 2: cumulative % of daily total ----

fig_prop <- dat_long %>%
  ggplot(aes(x = time_frac, y = cum_prop,
             group = date, color = factor(date))) +
  theme_bw() +
  geom_line(alpha = 0.7) +
  geom_point(size = 0.7, alpha = 0.5) +
  facet_wrap(~macro, ncol = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x     = "Fraction of day elapsed (item order)",
    y     = "Cumulative % of daily total",
    color = "Date",
    title = "Intra-day cumulative macro intake — % of daily total"
  )

ggsave(file.path(OUT_DIR, "burndown_proportional.png"),
       fig_prop, width = 12, height = 8, dpi = 300)

# ---- Plot 3: same as #2 but colored by weekend/weekday ----

fig_wkd <- dat_long %>%
  ggplot(aes(x = time_frac, y = cum_prop,
             group = date, color = weekend)) +
  theme_bw() +
  geom_line(alpha = 0.6) +
  facet_wrap(~macro, ncol = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c(weekday = "steelblue", weekend = "salmon4")) +
  labs(
    x     = "Fraction of day elapsed",
    y     = "Cumulative % of daily total",
    color = "Day type",
    title = "Weekday vs weekend intake dynamics"
  )

ggsave(file.path(OUT_DIR, "burndown_weekday_vs_weekend.png"),
       fig_wkd, width = 12, height = 8, dpi = 300)

message("Done. Outputs in: ", OUT_DIR)
message("Days: ",   n_distinct(dat_long$date),
        ", weeks: ", n_distinct(dat_long$iso_week),
        ", items: ", nrow(dat))
