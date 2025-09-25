# 1) Predicted data )
plot_data <- results_wide %>%
  filter(horizon %in% c(1, 12, 48),
         time >= as.POSIXct("2023-11-10"),
         time <= as.POSIXct("2024-012-24")) %>%
  select(time, Predicted, CI_low_95, CI_high_95, horizon)

# 2) Actual data
actual_data <- results_wide %>%
  filter(time >= as.POSIXct("2023-11-10"),
         time <= as.POSIXct("2024-012-24")) %>%
  group_by(time) %>%
  summarise(Actual = mean(Actual, na.rm = TRUE), .groups = "drop") %>%  # or first(na.omit(Actual))
  arrange(time)


color_vals <- c("Actual" = "blue", "1" = "red", "12" = "gray20", "48" = "orange")
fill_vals  <- c("1" = "red", "12" = "grey20", "48" = "orange")

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
    linewidth = 2
  ) +
  # NOTE: inherit.aes = FALSE ensures no horizon/group leaks in
  geom_line(
    data = actual_data,
    aes(x = time, y = Actual, color = "Actual"),
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    name = NULL,
    values = color_vals,
    breaks = c("Actual", "1", "12", "48"),
    labels = c("Actual", "Predicted h=1", "Predicted h=12", "Predicted h=48")
  ) +
  scale_fill_manual(
    name = "95% CI",
    values = fill_vals,
    breaks = c("1", "12", "48"),
    labels = c("h=1", "h=12", "h=48")
  ) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b") +
  ylim(0, 10) +
  labs(title = "Wet Season", y = "River Level (m)", x = NULL) +
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