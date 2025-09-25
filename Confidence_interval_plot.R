library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

make_plot <- function(t0, label) {
  start <- t0 - days(1)
  end   <- t0 + days(6)
  
  sum_by_h <- results_wide %>%
    filter(horizon %in% horizons,
           time >= start, time <= end) %>%
    group_by(horizon) %>%
    summarise(
      Pred_mean   = mean(Predicted,  na.rm = TRUE),
      Actual_mean = mean(Actual,     na.rm = TRUE),
      CI_low_mean  = mean(CI_low_95,  na.rm = TRUE),
      CI_high_mean = mean(CI_high_95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(as.numeric(horizon))
  
  ggplot(sum_by_h, aes(x = as.numeric(horizon))) +
    geom_ribbon(aes(ymin = CI_low_mean, ymax = CI_high_mean, fill = "95% CI"), alpha = 0.3) +
    geom_line(aes(y = Pred_mean, color = "Predicted"), linewidth = 1.5) +
    geom_point(aes(y = Pred_mean, color = "Predicted"), size = 2) +
    geom_line(aes(y = Actual_mean, color = "Observed"), linewidth = 1.5) +
    geom_point(aes(y = Actual_mean, color = "Observed"), size = 2) +
    scale_x_continuous(breaks = horizons) +
    scale_y_continuous(limits = c(0, 9)) +
    scale_color_manual(name = NULL, values = c("Predicted" = "orange", "Observed" = "black")) +
    scale_fill_manual(name = NULL, values = c("95% CI" = "grey70")) +
    labs(x = "Forecast Horizon (Hour)", y = "River Level (m)") +
    annotate("text", x = -1, y = 9, label = label, hjust = 0, vjust = 1, size = 12 / .pt) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none", # hide duplicate legends
      legend.text = element_text(size = 10)
    )
}

# horizons to show
horizons <- c(1, 2, 3, 4, 5, 6, 12, 24, 48)

# define t0 for each case (replace with your chosen percentile times)
t0_5   <- as.POSIXct("2021-12-16 19:00:00")  # 5th percentile event
t0_50  <- as.POSIXct("2024-01-09 04:00:00")  # median event
t0_95  <- as.POSIXct("2023-12-14 18:00:00")  # 95th percentile event

# make three plots
p1 <- make_plot(t0_5,  "a) 5th Percentile")
p2 <- make_plot(t0_50, "b) Median")
p3 <- make_plot(t0_95, "c) 95th Percentile")

# stack them vertically
final_plot <- p1 / p2 / p3

final_plot
