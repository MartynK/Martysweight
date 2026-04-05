# iter2.r — Anthropometrics: daily linear interpolation for all metrics
#
# Reads the transposed anthropometrics.xlsx, pivots to long format,
# interpolates every metric daily, and outputs a faceted time-series plot
# plus CSV/XLSX of the interpolated wide table.
#
# Outputs:
#   anthropometrics_plot.png
#   anthropometrics_interpolated.csv
#   anthropometrics_interpolated.xlsx
#
# ---- Opinion: best metrics for health & body composition tracking ----
#
# TIER 1 — track these first:
#   waist (Haskörfogat)    Best single anthropometric predictor of
#                          cardiometabolic risk and visceral adiposity.
#                          WHO/AHA/IDF all endorse it.
#   body_mass (Testtömeg)  Fundamental; trend over time matters more than
#                          any single reading.
#   bf_jp3 (Jackson/Pollock 3)  Most validated 3-site skinfold equation;
#                          captures body fat % better than individual folds.
#   ffmi / ffmi_std        Fat-Free Mass Index. Best composite for lean mass
#                          tracking, independent of height. A rising FFMI
#                          with stable or falling BF% = real muscle gain.
#
# TIER 2 — useful supporting context:
#   hip (Csípőkörfogat)    Enables waist-to-hip ratio (WHR >0.90 in men =
#                          increased CVD risk per WHO).
#   bmi                    Imperfect but universal population reference.
#   bf_navy / bf_mod_jp3   Alternative BF% estimates; comparing them to JP3
#                          gives a sense of estimation uncertainty.
#   fat_mass / ffm         Useful for absolute kg-level tracking.
#   sf_abdomen / sf_chest / sf_thigh  The three JP3 input sites; watching
#                          them individually shows where fat is mobilised.
#
# TIER 3 — low priority for health tracking:
#   forearm                Barely changes; mostly useful as a frame-size ref.
#   Individual skinfolds with many NAs (lower_back, subscapular, midaxillary,
#   triceps) — too sparse here to interpolate reliably.
#   bf_ymca                Least validated circumference-based estimate.

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(here)
library(openxlsx)

OUT_DIR <- here::here("inst", "simple_summ_plot")

# ---- Hungarian -> English metric name mapping ----

METRIC_DICT <- tribble(
  ~hu_pattern,           ~en_name,           ~unit,
  "Testtömeg",           "body_mass",        "kg",
  "^'Has$",              "abdomen_narrow",   "cm",
  "Haskörfogat",         "waist",            "cm",
  "Csípőkörfogat",       "hip",              "cm",
  "Nyak",                "neck",             "cm",
  "Comb",                "mid_thigh",        "cm",
  "Alkar",               "forearm",          "cm",
  "^ZS: has$",           "sf_abdomen",       "mm",
  "ZS: mell",            "sf_chest",         "mm",
  "ZS: comb",            "sf_thigh",         "mm",
  "ZS: biceps",          "sf_biceps",        "mm",
  "ZS: suprailiaca",     "sf_suprailiac",    "mm",
  "ZS: vádli",           "sf_calf",          "mm",
  "ZS: triceps",         "sf_triceps",       "mm",
  "ZS: alsó",            "sf_lower_back",    "mm",
  "ZS: subscapula",      "sf_subscapular",   "mm",
  "ZS: midaxillary",     "sf_midaxillary",   "mm",
  "^BMI$",               "bmi",              "kg/m2",
  "US\\.Navy",           "bf_navy",          "fraction",
  "Jackson",             "bf_jp3",           "fraction",
  "Mod\\.JP3",           "bf_mod_jp3",       "fraction",
  "^YMCA$",              "bf_ymca",          "fraction",
  "^Zsír$",              "fat_mass",         "kg",
  "^nemzsír$",           "ffm",              "kg",
  "^FFMI \\(",           "ffmi",             "kg/m2",
  "^FFMI_std",           "ffmi_std",         "kg/m2"
)

translate_metric <- function(hu_label) {
  for (j in seq_len(nrow(METRIC_DICT))) {
    if (grepl(METRIC_DICT$hu_pattern[j], hu_label)) {
      return(METRIC_DICT$en_name[j])
    }
  }
  hu_label  # fallback: keep original
}

# ---- Read transposed spreadsheet ----

raw <- read_excel(
  here::here("inst", "extdata", "anthropometrics.xlsx"),
  col_names = FALSE
)

labels <- as.character(raw[[1]])

# ---- Extract dates from the DÁTUM row ----

date_row <- which(grepl("DÁTUM", labels))
n_cols   <- ncol(raw)

dates_raw  <- suppressWarnings(as.numeric(unlist(raw[date_row, 2:n_cols])))
valid_cols <- which(!is.na(dates_raw))          # indices within 2:n_cols
dates      <- as.Date(dates_raw[valid_cols], origin = "1899-12-30")

# actual column positions in `raw` (col 1 = labels, so offset by 1)
data_cols <- valid_cols + 1

# ---- Extract every metric row that has >= 3 observations ----

# Rows to skip even if they have numbers
skip_re <- paste0(
  "Életkor|^-+$|^delta|legszűk|1cm-rel|",
  "fenék|Ádámcsutka|^NÉV|Születési|^Nem:|Magasság|linear-software"
)

dat_long <- tibble()

for (i in (date_row + 1):nrow(raw)) {
  lbl <- trimws(labels[i])
  if (is.na(lbl) || lbl == "") next
  if (grepl(skip_re, lbl, ignore.case = TRUE)) next

  vals <- suppressWarnings(as.numeric(unlist(raw[i, data_cols])))
  n_valid <- sum(!is.na(vals))
  if (n_valid < 3) next

  dat_long <- bind_rows(dat_long, tibble(
    date   = dates,
    metric = lbl,
    value  = vals
  ))
}

# Apply English translation
dat_long <- dat_long %>%
  mutate(metric_en = sapply(metric, translate_metric, USE.NAMES = FALSE))

message(length(unique(dat_long$metric_en)), " metrics extracted.")

# ---- Daily interpolation per metric ----
#
# Strategy:
#   - Constant (step, f=0.5) interpolation as the baseline everywhere.
#   - Where consecutive observations are <= 10 days apart ("dense runs"),
#     overlay a LOESS smooth on the raw points.
#   - A linear edge correction forces the LOESS to exactly match the raw
#     first/last values of each run, so the join to the step segments is
#     continuous.

DENSE_GAP      <- 10    # days; consecutive obs <= this are "dense"
SPLINE_TENSION <- 0.33  # 0 = pure linear (no overshoot), 1 = full monoH.FC spline

full_dates <- seq(min(dates), max(dates), by = 1)

dat_interp <- dat_long %>%
  filter(!is.na(value)) %>%
  group_by(metric_en) %>%
  group_modify(~ {
    obs <- .x %>% arrange(date)
    obs_d <- as.numeric(obs$date)
    obs_v <- obs$value
    n_obs <- length(obs_d)

    # daily target range
    target <- full_dates[
      full_dates >= min(obs$date) & full_dates <= max(obs$date)
    ]
    target_d <- as.numeric(target)

    # Step 1: blend of linear and monoH.FC spline (SPLINE_TENSION controls shape)
    if (n_obs >= 2) {
      lin <- approx(obs_d, obs_v, xout = target_d, method = "linear", rule = 1)$y
      spl <- splinefun(obs_d, obs_v, method = "monoH.FC")(target_d)
      result <- (1 - SPLINE_TENSION) * lin + SPLINE_TENSION * spl
    } else {
      result <- rep(obs_v, length(target_d))
    }

    # Step 2: identify dense runs (consecutive gaps <= DENSE_GAP days)
    if (n_obs >= 3) {
      gaps   <- diff(obs_d)
      run_id <- cumsum(c(1, gaps > DENSE_GAP))

      for (rid in unique(run_id)) {
        idx   <- which(run_id == rid)
        if (length(idx) < 3) next  # loess needs >= 3 points

        run_d   <- obs_d[idx]
        run_v   <- obs_v[idx]
        d_range <- max(run_d) - min(run_d)
        if (d_range < 2) next  # all points within 1 day

        # Adaptive span: wider for fewer points
        lo_span <- max(0.75, 3 / length(idx))
        df_run  <- data.frame(d = run_d, v = run_v)
        lo <- tryCatch(
          loess(v ~ d, data = df_run, span = lo_span),
          error = function(e) NULL
        )
        if (is.null(lo)) next

        # Daily targets inside this run
        run_mask     <- target_d >= min(run_d) & target_d <= max(run_d)
        run_target_d <- target_d[run_mask]
        if (length(run_target_d) == 0) next

        lo_pred <- predict(lo, newdata = data.frame(d = run_target_d))

        # Linear edge correction: anchor first/last to raw values
        v_first  <- run_v[1]
        v_last   <- run_v[length(run_v)]
        lo_first <- predict(lo, newdata = data.frame(d = min(run_d)))
        lo_last  <- predict(lo, newdata = data.frame(d = max(run_d)))

        frac       <- (run_target_d - min(run_d)) / d_range
        correction <- (v_first - lo_first) * (1 - frac) +
                      (v_last  - lo_last)  * frac

        smoothed <- lo_pred + correction

        # Overwrite baseline where loess succeeded
        valid_lo <- !is.na(smoothed)
        run_idx  <- which(run_mask)
        result[run_idx[valid_lo]] <- smoothed[valid_lo]
      }
    }

    tibble(date = target, value = result)
  }) %>%
  ungroup()

message("Interpolation done: ", nrow(dat_interp), " daily rows.")

# ---- Faceted plot: tier 1 & 2 metrics only, interpolated ----

# Metrics for plot + quarterly summary:
# Exclude body_mass (iter1), bf_jp3 (redundant w/ mod), bf_navy,
# ffmi (keep ffmi_std), and individual skinfolds.
PLOT_METRICS <- c(
  "waist", "hip", "bmi", "bf_mod_jp3",
  "fat_mass", "ffm", "ffmi_std"
)

fig_anthro <- dat_interp %>%
  filter(metric_en %in% PLOT_METRICS) %>%
  ggplot(aes(x = date, y = value)) +
  theme_bw() +
  theme(strip.text = element_text(size = 7)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  geom_point(
    data = dat_obs %>% filter(metric_en %in% PLOT_METRICS),
    aes(x = date, y = value),
    color = "salmon4", size = 1, alpha = 0.6
  ) +
  facet_wrap(~metric_en, scales = "free_y", ncol = 4) +
  labs(
    x        = "Date",
    y        = NULL,
    title    = "Anthropometric trends (interpolated)",
    subtitle = "Blue = interpolated, brown dots = raw observations"
  )

ggsave(
  file.path(OUT_DIR, "anthropometrics_plot.png"),
  plot  = fig_anthro,
  width = 16, height = 14, dpi = 300
)

message("Plot saved.")

# ---- Quarterly summary (same style as iter1) ----

# Add per-metric daily derivative via central difference
dat_interp_deriv <- dat_interp %>%
  filter(metric_en %in% PLOT_METRICS) %>%
  arrange(metric_en, date) %>%
  group_by(metric_en) %>%
  mutate(
    n   = n(),
    idx = row_number(),
    deriv = case_when(
      idx == 1 ~ value[2] - value[1],
      idx == n ~ value[n] - value[n - 1],
      TRUE     ~ (lead(value) - lag(value)) / 2
    )
  ) %>%
  ungroup() %>%
  select(-n, -idx)

# Quarter-end values (last day of each quarter per metric)
dat_qend <- dat_interp_deriv %>%
  mutate(year = year(date), quarter = paste0("Q", quarter(date))) %>%
  group_by(metric_en, year, quarter) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(metric_en, year, quarter) %>%
  group_by(metric_en) %>%
  mutate(change = value - lag(value)) %>%
  ungroup()

# Pivot to wide: one row per year-quarter, one column per metric_stat
dat_q_summary <- dat_qend %>%
  select(year, quarter, metric_en, value, deriv, change) %>%
  pivot_wider(
    names_from  = metric_en,
    values_from = c(value, deriv, change),
    names_glue  = "{metric_en}_{.value}"
  ) %>%
  arrange(year, quarter)

write.csv(
  dat_q_summary,
  file      = file.path(OUT_DIR, "quarterly_summary_anthro.csv"),
  row.names = FALSE
)

write.xlsx(
  dat_q_summary,
  file = file.path(OUT_DIR, "quarterly_summary_anthro.xlsx")
)

message("Quarterly anthro summary saved.")

# ---- Wide-format export ----

dat_wide <- dat_interp %>%
  pivot_wider(names_from = metric_en, values_from = value) %>%
  arrange(date)

write.csv(
  dat_wide,
  file      = file.path(OUT_DIR, "anthropometrics_interpolated.csv"),
  row.names = FALSE
)

write.xlsx(
  dat_wide,
  file = file.path(OUT_DIR, "anthropometrics_interpolated.xlsx")
)

message("Tables saved. Done.")
message("Outputs in: ", OUT_DIR)
