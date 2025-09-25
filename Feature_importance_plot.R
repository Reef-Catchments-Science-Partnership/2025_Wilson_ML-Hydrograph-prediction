
library(ggplot2)
library(catboost)
library(tidyverse)

     

# Store feature importance for each horizon
feature_importance_list <- list()

for (h in horizons) {
  model_path <- paste0("catboost_model_RMSEWithUncertainty_h", h, ".cbm")
  model <- catboost.load_model(model_path)
  
  importance_df <- catboost.get_feature_importance(model, 
                                                   pool = NULL, 
                                                   type = "FeatureImportance") %>%
    as.data.frame() %>%
    setNames("Importance") %>%
    mutate(
      Feature = features,
      Horizon = paste0("h", h)
    ) %>%
    arrange(desc(Importance))
  
  feature_importance_list[[paste0("h", h)]] <- importance_df
}

# Combine into one long dataframe
feature_importance_all <- bind_rows(feature_importance_list)

# --- Plot: Top 15 features for each horizon ---
library(forcats)

plot_feature_importance <- function(h) {
  feature_importance_all %>%
    filter(Horizon == h) %>%
    mutate(Feature = fct_reorder(Feature, Importance)) %>%
    top_n(15, Importance) %>%
    ggplot(aes(x = Feature, y = Importance)) +
    geom_col(fill = "#0072B2") +
    coord_flip() +
    labs(
      title = paste("Feature Importance for Horizon", h),
      x = NULL,
      y = "Importance"
    ) +
    theme_minimal(base_size = 14)
}

avg_feature_importance <- feature_importance_all %>%
  group_by(Feature) %>%
  summarise(
    MeanImportance = mean(Importance, na.rm = TRUE),
    SDImportance = sd(Importance, na.rm = TRUE)
  ) %>%
  arrange(desc(MeanImportance))



# ---- 3) Plot with safe fallback ----
avg_feature_importance %>%
  arrange(desc(MeanImportance)) %>%
  slice(1:20) %>%
  mutate(FeatureLabel = recode(Feature, !!!feature_labels, .default = Feature)) %>%
  ggplot(aes(x = fct_reorder(FeatureLabel, MeanImportance), y = MeanImportance)) +
  geom_col(fill = "grey20", width = 0.6) +
  coord_flip() +
  labs(x = NULL, y = "Mean Feature Importance") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

