library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

# define flow events 
# Find best event for each flow regime
regime_events <- results_wide %>%
  filter(horizon == 1) %>%
  arrange(time) %>%
  mutate(
    date = as.Date(time),
    rate_of_change = c(0, diff(Actual))
  ) %>%
  group_by(date) %>%
  summarise(
    min_height = min(Actual, na.rm = TRUE),
    max_height = max(Actual, na.rm = TRUE),
    range = max_height - min_height,
    peak = max(Actual, na.rm = TRUE),
    max_rate = max(abs(rate_of_change), na.rm = TRUE),
    first_time = min(time),
    n_obs = n()
  ) %>%
  mutate(
    regime = case_when(
      peak > 5.0 ~ "Extreme",
      peak > 3.2 ~ "High",
      peak > 1.9 ~ "Moderate",
      TRUE ~ "Low"
    ),
    score = range * max_rate
  ) %>%
  filter(range > 0.5) %>%
  group_by(regime) %>%
  arrange(desc(score)) %>%
  slice_head(n = 5)

# Select best event from each regime
best_events <- regime_events %>%
  slice_head(n = 1) %>%
  ungroup()


# Use moderate flow event
moderate_event <- best_events %>% filter(regime == "Moderate")
forecast_time <- moderate_event$first_time - hours(1)
forecast_time <- as.POSIXct(forecast_time)

print(paste("Moderate event date:", moderate_event$date))
print(paste("Forecast time (t=0):", forecast_time))

# Get event data
event_data <- results_wide %>%
  filter(time >= forecast_time + hours(1) & 
           time <= forecast_time + hours(48)) %>%
  mutate(hours_from_forecast = as.numeric(difftime(time, forecast_time, units = "hours")))

# Actual values
actual_data <- event_data %>%
  filter(horizon == 1) %>%
  select(hours_from_forecast, time, Actual) %>%
  arrange(hours_from_forecast)

# Forecast data
get_horizon_data <- function(data, h) {
  data %>%
    filter(horizon == h, hours_from_forecast <= h) %>%
    select(hours_from_forecast, time, Predicted, CI_low_95, CI_high_95, Actual) %>%
    arrange(hours_from_forecast)
}

h6_data <- get_horizon_data(event_data, 6)
h12_data <- get_horizon_data(event_data, 12)
h24_data <- get_horizon_data(event_data, 24)
h48_data <- get_horizon_data(event_data, 48)

# Set common y-axis limits
y_min <- min(c(actual_data$Actual, h48_data$CI_low_95), na.rm = TRUE) * 0.95
y_max <- max(c(actual_data$Actual, h48_data$CI_high_95), na.rm = TRUE) * 1.05
panel_a <- ggplot() +
  geom_ribbon(data = h48_data, aes(x = hours_from_forecast, ymin = CI_low_95, ymax = CI_high_95, fill = "h=48"),
              alpha = 0.12) +
  geom_ribbon(data = h24_data, aes(x = hours_from_forecast, ymin = CI_low_95, ymax = CI_high_95, fill = "h=24"),
              alpha = 0.15) +
  geom_ribbon(data = h12_data, aes(x = hours_from_forecast, ymin = CI_low_95, ymax = CI_high_95, fill = "h=12"),
              alpha = 0.18) +
  geom_ribbon(data = h6_data, aes(x = hours_from_forecast, ymin = CI_low_95, ymax = CI_high_95, fill = "h=6"),
              alpha = 0.2) +
  geom_line(data = h48_data, aes(x = hours_from_forecast, y = Predicted, color = "h=48"),
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data = h24_data, aes(x = hours_from_forecast, y = Predicted, color = "h=24"),
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data = h12_data, aes(x = hours_from_forecast, y = Predicted, color = "h=12"),
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data = h6_data, aes(x = hours_from_forecast, y = Predicted, color = "h=6"),
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data = actual_data, aes(x = hours_from_forecast, y = Actual, linetype = "Observed"),
            color = "#2C2C2A", linewidth = 1) +
  scale_fill_manual(values = c("h=6" = "#639922", "h=12" = "#BA7517", "h=24" = "#378ADD", "h=48" = "#E24B4A"),
                    name = "Forecast horizon (h)") +
  scale_color_manual(values = c("h=6" = "#639922", "h=12" = "#BA7517", "h=24" = "#378ADD", "h=48" = "#E24B4A"),
                     name = "Forecast horizon (h)") +
  scale_linetype_manual(values = c("Observed" = "solid"),
                        name = NULL) +
  scale_x_continuous(breaks = seq(0, 48, 12), limits = c(1, 48)) +
  scale_y_continuous(limits = c(y_min, y_max)) +
  labs(
    x = expression("Hours from event onset (" * italic(t) * "=0)"),
    y = "River height (m)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )
print(panel_a)
# Save for A4 page
ggsave("Panel_A_forecast_only.png", panel_a, width = 9, height = 4, dpi = 300, bg = "white")
ggsave("Panel_A_forecast_only.pdf", panel_a, width = 9, height = 4)
