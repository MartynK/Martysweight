# app.R — Shiny: intra-day macro intake explorer
#
# Run from R:
#   shiny::runApp(here::here("inst", "2026.food_log_xplore"))

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(here)
library(scales)

# ---- Data (same wrangling as iter1_explore.r) ----

path <- here::here("inst", "extdata", "2026.food_logging",
                   "macro_tracking_week3.xlsx")
dat_raw <- read_excel(path, sheet = "Daily Items")
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

dat <- dat_raw %>%
  mutate(date = as.Date(date)) %>%
  # Drop the per-day TOTAL row (otherwise daily sums double-count).
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

dat_long <- dat %>%
  select(date, day, time_frac, item, all_of(MACROS)) %>%
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

ALL_DATES <- sort(unique(dat_long$date))
ALL_WEEKS <- sort(unique(dat_long$iso_week))

# ---- UI ----

ui <- fluidPage(
  titlePanel("Intra-day macro intake dynamics"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "mode", "Filter by:",
        choices = c("Date range"     = "range",
                    "Specific dates" = "dates",
                    "ISO week"       = "week")
      ),
      conditionalPanel(
        "input.mode == 'range'",
        dateRangeInput("date_range", "Date range",
                       start = min(ALL_DATES), end = max(ALL_DATES))
      ),
      conditionalPanel(
        "input.mode == 'dates'",
        checkboxGroupInput("dates_pick", "Dates",
                           choices  = as.character(ALL_DATES),
                           selected = as.character(ALL_DATES))
      ),
      conditionalPanel(
        "input.mode == 'week'",
        selectInput("week_pick", "ISO week",
                    choices  = ALL_WEEKS,
                    selected = ALL_WEEKS[1])
      ),
      radioButtons(
        "yscale", "Y-axis:",
        choices = c("% of daily total" = "prop",
                    "Absolute"         = "abs"),
        selected = "prop"
      ),
      checkboxInput("color_weekend",
                    "Color by weekend (ignore date)", FALSE),
      width = 3
    ),
    mainPanel(
      plotOutput("burndown", height = "620px"),
      hr(),
      h4("Daily totals"),
      tableOutput("daily_totals"),
      width = 9
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {

  filtered <- reactive({
    d <- dat_long
    if (input$mode == "range") {
      d <- d %>% filter(date >= input$date_range[1],
                        date <= input$date_range[2])
    } else if (input$mode == "dates") {
      d <- d %>% filter(as.character(date) %in% input$dates_pick)
    } else {
      d <- d %>% filter(iso_week == as.integer(input$week_pick))
    }
    d
  })

  output$burndown <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No data for current selection."))

    yvar <- if (input$yscale == "prop") "cum_prop" else "cum"
    ylab <- if (input$yscale == "prop") "Cumulative % of daily total"
            else                        "Cumulative intake"

    aes_color <- if (input$color_weekend) aes(color = weekend)
                 else                      aes(color = factor(date))

    p <- ggplot(d, aes(x = time_frac, y = .data[[yvar]], group = date)) +
      theme_bw() +
      aes_color +
      geom_line(alpha = 0.8) +
      geom_point(size = 1, alpha = 0.6) +
      facet_wrap(~macro,
                 scales = if (input$yscale == "prop") "fixed" else "free_y",
                 ncol = 3) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Fraction of day elapsed (item order)",
           y = ylab,
           color = if (input$color_weekend) "Day type" else "Date")

    if (input$yscale == "prop") {
      p <- p + scale_y_continuous(labels = percent_format(accuracy = 1))
    }
    p
  })

  output$daily_totals <- renderTable({
    filtered() %>%
      group_by(date, macro) %>%
      summarise(total = round(max(cum), 1), .groups = "drop") %>%
      pivot_wider(names_from = macro, values_from = total) %>%
      arrange(date) %>%
      mutate(date = as.character(date))
  })
}

shinyApp(ui, server)
