# iter1.r
# Produces a clean weight model prediction plot (no data points, no knot markers)
# and a quarterly summary table (pred, deriv, change, high, low, range).
# Outputs: weight_model_plot.png, quarterly_summary.csv, quarterly_summary.xlsx

library(readxl)
library(splines)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(here)
library(openxlsx)

# ---- Constants ----

K                <- 13  # knot interval (days of unique observations)
DAYS_BEFORE_LAST <- 9   # target days of data after the last knot
FASTD_FOR_PRED   <- 8   # fasting hours used for the prediction line

OUT_DIR <- here::here("inst", "simple_summ_plot")

# ---- Load & wrangle data ----

dat <- here::here("inst", "extdata", "martysweight.xlsx") %>%
  read_excel(col_types = c("date", "numeric", "numeric",
                            "numeric", "numeric", "numeric", "numeric")) %>%
  filter(!is.na(Mass)) %>%
  mutate(
    Time_hours          = ifelse(is.na(Time_hours), 12, Time_hours),
    Time_minutes        = ifelse(is.na(Time_minutes), 0, Time_minutes),
    Time_fasted_hours   = ifelse(is.na(Time_fasted_hours), 4, Time_fasted_hours),
    Time_fasted_minutes = ifelse(is.na(Time_fasted_minutes), 0, Time_fasted_minutes),
    numd = as.numeric(Date
                      + Time_hours * 3600
                      + Time_minutes * 60
                      - min(Date)) / 24,
    fastd           = Time_fasted_hours + Time_fasted_minutes / 60,
    fastd_truncated = ifelse(fastd > 16, 16, fastd),
    maxinlast       = 0
  ) %>%
  arrange(numd)

dat_origin <- min(dat$Date)  # used for numd <-> Date conversion

# ---- Knot placement (same algorithm as vignette) ----

days_unique          <- unique(floor(dat$numd))
days_as_knots        <- seq(K, length(days_unique), by = K)
days_before_last_act <- length(days_unique) - dplyr::last(days_as_knots)
days_before_diff     <- days_before_last_act - DAYS_BEFORE_LAST
days_as_knots_corr   <- days_as_knots + days_before_diff
days_as_knots_corr   <- days_as_knots_corr[
  days_as_knots_corr > 10 &
  days_as_knots_corr < length(days_unique)
]

knot <- dat %>%
  filter(floor(numd) %in% days_unique[days_as_knots_corr]) %>%
  .$numd %>%
  floor() %>%
  unique()

# ---- Fit model ----

expre <- paste(knot, collapse = ",") %>%
  paste("c(", ., ")") %>%
  paste("Mass ~ ns(fastd, df = 1) + ns(numd, knots =", ., ")") %>%
  as.formula()

mod <- lm(expre, data = dat)

# ---- Daily prediction grid ----
# One row per day; integer steps let us use simple indexing for derivatives.

dat_p <- expand.grid(
    fastd     = FASTD_FOR_PRED,
    numd      = seq(1, max(dat$numd) + 30, 1),
    maxinlast = 12
  ) %>%
  mutate(fastd_truncated = fastd) %>%
  arrange(numd)

dat_p$pred <- predict(mod, newdata = dat_p)
dat_p$date <- as.Date(dat_p$numd, origin = dat_origin)

# Numerical derivative: central difference (kg/day), forward/backward at edges
n              <- nrow(dat_p)
dat_p$deriv_kgd <- c(
  dat_p$pred[2]   - dat_p$pred[1],                        # forward at start
  (dat_p$pred[3:n] - dat_p$pred[1:(n - 2)]) / 2,         # central
  dat_p$pred[n]   - dat_p$pred[n - 1]                     # backward at end
)

# ---- Clean plot ----

fig_plot <- dat_p %>%
  ggplot(aes(x = date, y = pred)) +
  theme_bw() +
  geom_line(linewidth = 1.1, color = "salmon4") +
  scale_x_date(
    date_breaks       = "2 years",
    date_minor_breaks = "1 year",
    date_labels       = "%Y"
  ) +
  scale_y_continuous(limits = c(60, 90)) +
  labs(
    x        = "Year",
    y        = "Predicted mass (kg)",
    title    = "Weight model prediction",
    subtitle = paste0("Natural spline, k = ", K, ", d = ", DAYS_BEFORE_LAST,
                      ", fasting = ", FASTD_FOR_PRED, " h")
  )

ggsave(
  filename = file.path(OUT_DIR, "weight_model_plot.png"),
  plot     = fig_plot,
  width    = 10,
  height   = 5,
  dpi      = 300
)

message("Plot saved.")

# ---- Quarterly summary table ----

# Annotate each day with year + quarter label, then summarise.
# pred, deriv, change are taken at the last day of each quarter.
# high, low, range span the whole quarter.

dat_p_annotated <- dat_p %>%
  mutate(
    year    = year(date),
    quarter = paste0("Q", quarter(date))
  )

# Per-quarter end-of-quarter values (last row in each quarter group)
dat_qend <- dat_p_annotated %>%
  group_by(year, quarter) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(year, quarter, date_qend = date, pred, deriv_kgd)

# Per-quarter range stats
dat_qrange <- dat_p_annotated %>%
  group_by(year, quarter) %>%
  summarise(
    high  = max(pred),
    low   = min(pred),
    range = high - low,
    .groups = "drop"
  )

# Combine, add change-from-previous-quarter, and round
dat_summary <- dat_qend %>%
  left_join(dat_qrange, by = c("year", "quarter")) %>%
  arrange(year, quarter) %>%           # Q1 < Q2 < Q3 < Q4 alphabetically fine
  mutate(
    change = pred - lag(pred)
  ) %>%
  select(year, quarter, date_qend, pred, deriv_kgd, change, high, low, range) %>%
  mutate(across(c(pred, deriv_kgd, change, high, low, range), ~round(., 3)))

# ---- Save outputs ----

write.csv(
  dat_summary,
  file      = file.path(OUT_DIR, "quarterly_summary.csv"),
  row.names = FALSE
)

write.xlsx(
  dat_summary,
  file = file.path(OUT_DIR, "quarterly_summary.xlsx")
)

message("Table saved. Done.")
message("Outputs in: ", OUT_DIR)
