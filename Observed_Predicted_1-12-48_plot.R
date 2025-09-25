
#### figure 2 
plot_data <- results_wide %>%
  filter(horizon == 48) %>% #change filter based on what horizon you want to select 
  select(time, Predicted, CI_low_95, CI_high_95, horizon)

# 2) Actual data: collapse to ONE row per time + sort
actual_data <- results_wide %>%
  group_by(time) %>%
  summarise(Actual = mean(Actual, na.rm = TRUE), .groups = "drop") %>%  # or first(na.omit(Actual))
  arrange(time)

color_vals <- c("Actual" = "orange", "48" = "blue")
fill_vals  <- c("48" = "blue")

library(ggplot2)

ggplot() +
  geom_ribbon(
    data = plot_data,
    aes(x = time, ymin = CI_low_95, ymax = CI_high_95, fill = as.factor(horizon)),
    alpha = 0.25
  ) +
  geom_line(
    data = plot_data,
    aes(x = time, y = Predicted, color = as.factor(horizon)),
    linewidth = 1
  ) +
  # NOTE: inherit.aes = FALSE ensures no horizon/group leaks in
  geom_line(
    data = actual_data,
    aes(x = time, y = Actual, color = "Actual"),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    name = NULL,
    values = color_vals,
    breaks = c("Actual", "48"),
    labels = c("Actual", "Predicted")
  ) +
  scale_fill_manual(
    name = NULL,
    values = fill_vals,
    breaks = c("48"),
    labels = c("95% CI")
  ) +
  scale_x_datetime(date_breaks = "60 days", date_labels = "%d %b %Y") +
  ylim(0, 10) +
  labs(title = "Forecast Horizon: 48 Hour", y = "River Level (m)", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position.inside = c(1, 1),          # top right inside plot panel
    legend.justification = c("top"), 
    legend.box = "vertical",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
