
library(dplyr)
library(ggplot2)

results_wide <- read.csv("results_wide.csv")
fdc <- read.csv("EuramoFDC.CSV", skip = 6)

breaks <- c(0, 5, 20, 80, Inf)
labels <- c("a) Extreme (95-100th Percentile)", 
            "b) High (80-95th Percentile)", 
            "c) Moderate (20-80th Percentile)", 
            "d) Low (0-20th Percentile)")

# Optional: custom labels for the ranges

# horizons to show
horizons <- c(1, 2, 3, 4, 5, 6, 12, 24, 48)

data.frame(breaks = 100-breaks, Height = spline(fdc$X., fdc$Total, xout = breaks)$y)

sum_by_h <- results_wide %>% mutate(fdc = spline(fdc$Total, fdc$X., xout = Actual)$y)  %>%
  mutate(value_range = cut(fdc, breaks = breaks, labels = labels, right = FALSE)) %>%
  mutate(Residual = Actual - Predicted) %>%
  mutate(CI_low_95 = Actual - CI_low_95) %>%
  mutate(CI_high_95 = Actual - CI_high_95) %>%
  group_by(value_range, horizon) %>%
  summarise(
    Residual   = mean(Residual,  na.rm = TRUE),
    CI_low_mean  = mean(CI_low_95,  na.rm = TRUE),
    CI_high_mean = mean(CI_high_95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(as.numeric(horizon))


facet_labels <- setNames(
  paste0(letters[1:4], ") "),   # "a) ", "b) ", "c) ", "d) "
  unique(sum_by_h$value_range)  # original facet levels
)


ggplot(sum_by_h, aes(x = as.numeric(horizon))) +
  geom_ribbon(aes(ymin = CI_low_mean, ymax = CI_high_mean, fill = "95% CI"), alpha = 0.3) +

  geom_line(aes(y = Residual, color = "Predicted"), linewidth = 1.5) +
  geom_point(aes(y = Residual, color = "Predicted"), size = 2) +
  geom_hline(yintercept = 0) +
  
  #geom_line(aes(y = Actual_mean, color = "Observed"), linewidth = 1.5) +
  #geom_point(aes(y = Actual_mean, color = "Observed"), size = 2) +
  scale_x_log10(
    breaks = horizons,
    labels = horizons
  ) +
  #scale_y_continuous(limits = c(0, 9)) +
  scale_color_manual(name = NULL, values = c("Predicted" = "orange", "Observed" = "black")) +
  scale_fill_manual(name = NULL, values = c("95% CI" = "grey70")) +
  labs(x = "Forecast Horizon (Hour)", y = "Residual (Observed - Predicted) (m)") +

  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top", # hide duplicate legends
    legend.text = element_text(size = 10),
    axis.title = element_text(size=11)
  ) +
  facet_wrap(~value_range, ncol = 1,strip.position = "top") +
  theme(strip.text = element_text(hjust = 0))

ggsave("residuals.png", width = 7.5, height = 8, dpi = 600)



