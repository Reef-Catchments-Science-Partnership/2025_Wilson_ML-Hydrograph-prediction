# --- Libraries ---
library(catboost)
library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(plotly)
library(yardstick)
library(tibble)
library(ggpmisc)
library(ggthemes)
library(scoringRules)

## Data used to create statistics tables for all data and data > 95th percentile 

# --- Load data ---
dat <- read_csv("Data/dat2.csv")

# Add time context features
dat <- dat %>%
  mutate(
    hour = hour(time),
    month = month(time),
    day = day(time)
  ) %>%
  filter(!is.na(TRE_height_metres_value))  # remove rows with NA in target

# Define features
lagged_features <- grep("_lag_h[0-9]+$", names(dat), value = TRUE)

context_features <- c("hour", "month", "day")

discharge_features <- c(
  "TRG_discharge_cumecs_value",
  "CC_discharge_cumecs_value",
  "TRE_discharge_cumecs_value"
)

rainfall_features <- c("MRU_rainfall_mm_value", "TRE_rainfall_mm_value")

level_features <- c(
  "CC_height_deriv_value", "CC_height_metres_value",
  "TRE_height_deriv_value", "TRE_height_metres_value",
  "TRG_height_deriv_value", "TRG_height_metres_value"
)

features <- c(
  lagged_features,
  context_features,
  discharge_features,
  rainfall_features,
  level_features
)

# Clean up non-finite and NaN values
dat <- dat %>%
  mutate(across(all_of(features), ~ ifelse(is.nan(.), NA, .))) %>%
  filter(if_all(all_of(features), ~ is.finite(.) | is.na(.)))

# --- Forecast horizons to evaluate ---
horizons <- c(1, 2, 3, 4, 5, 6, 12, 24, 48)

h <- horizons 


# Add lead columns for all horizons
for (h in horizons) {
  dat[[paste0("TRE_height_metres_value_lead", h)]] <- dplyr::lead(dat$TRE_height_metres_value, h)
}

# --- Train/Validation/Test Split ---
split_index <- floor(0.8 * nrow(dat))
dat_train <- dat[1:split_index, ]
dat_test  <- dat[(split_index + 1):nrow(dat), ]

train_index <- floor(0.75 * nrow(dat_train))
dat_train_final <- dat_train[1:train_index, ]
dat_val         <- dat_train[(train_index + 1):nrow(dat_train), ]



# --- Loop for model prediction and storing test prediction data frames ---
library(dplyr)
library(tidyr)
library(purrr)

horizons <- c(1, 2, 3, 4, 5, 6, 12, 24, 48)

res_list <- list()

for (h in horizons) {
  target_col <- paste0("TRE_height_metres_value_lead", h)
  model_path <- paste0("catboost_model_RMSEWithUncertainty_h", h, ".cbm")
  
  # Guardrails
  if (!target_col %in% names(dat_test)) {
    warning(sprintf("Missing %s", target_col)); next
  }
  
  test_df <- dat_test %>% filter(!is.na(.data[[target_col]]))
  if (nrow(test_df) == 0) {
    warning(sprintf("No rows for h=%d (all targets NA?)", h)); next
  }
  
  if (!file.exists(model_path)) {
    warning(sprintf("Missing model for h=%d: %s", h, model_path)); next
  }
  
  # Predict
  model <- catboost.load_model(model_path)
  test_pool <- catboost.load_pool(test_df[, features, drop = FALSE])
  
  raw_pred <- catboost.predict(model, test_pool, prediction_type = "RawFormulaVal")
  
  if (is.vector(raw_pred)) {
    mu <- as.numeric(raw_pred)
    logvar <- rep(NA_real_, length(mu))
  } else {
    raw_pred <- as.data.frame(raw_pred)
    if (ncol(raw_pred) == 1) {
      mu <- as.numeric(raw_pred[[1]])
      logvar <- rep(NA_real_, length(mu))
    } else if (ncol(raw_pred) == 2) {
      names(raw_pred) <- c("mu","logvar")
      mu <- as.numeric(raw_pred$mu)
      logvar <- as.numeric(raw_pred$logvar)
    } else stop("Unexpected prediction shape")
  }
  
  if (length(mu) != nrow(test_df)) {
    stop(sprintf("Length mismatch h=%d: mu=%d rows=%d", h, length(mu), nrow(test_df)))
  }
  
  sigma <- ifelse(is.na(logvar), NA_real_, sqrt(exp(logvar)))
  z <- 1.96
  ci_low  <- ifelse(is.na(sigma), NA_real_, mu - z * sigma)
  ci_high <- ifelse(is.na(sigma), NA_real_, mu + z * sigma)
  
  # Wide table for this horizon
  df_wide <- test_df %>%
    select(time, all_of(target_col)) %>%
    transmute(
      horizon   = h,                      # <- FIXED
      time,
      Actual    = .data[[target_col]],
      Predicted = mu,
      Sigma     = sigma,
      CI_low_95  = ci_low,
      CI_high_95 = ci_high
    )
  
  res_list[[as.character(h)]] <- df_wide
}

# ===== ONE TABLE WITH ALL HORIZONS =====
results_wide <- bind_rows(res_list) %>%
  arrange(as.numeric(horizon), time)

# If you also want a long version for plotting:
results_long <- results_wide %>%
  pivot_longer(c(Actual, Predicted), names_to = "Type", values_to = "Value") %>%
  arrange(as.numeric(horizon), time)



#### r2 and rmse and me results 

library(yardstick)
library(dplyr)

metrics_tbl <- results_wide %>%
  group_by(horizon) %>%
  summarise(
    RMSE = rmse_vec(Actual, Predicted),
    ME = mean(Actual - Predicted, na.rm = TRUE),  # positive = underestimation
    R2   = rsq_vec(Actual, Predicted),
    .groups = "drop"
  ) %>%
  arrange(as.numeric(horizon))

metrics_tbl







library(dplyr)
library(purrr)

horizons <- c(1, 2, 3, 4, 5, 6, 12, 24, 48)

safe_sd <- function(x) { s <- sd(x, na.rm = TRUE); ifelse(is.finite(s) && s > 0, s, NA_real_) }
rmse_vec_simple <- function(obs, pred) sqrt(mean((pred - obs)^2, na.rm = TRUE))

metrics_hydro_tbl <- results_wide %>%
  group_by(horizon) %>%
  summarise(
    obs_mean = mean(Actual, na.rm = TRUE),
    sse = sum((Predicted - Actual)^2, na.rm = TRUE),
    sst = sum((Actual - obs_mean)^2, na.rm = TRUE),
    NSE  = ifelse(sst > 0, 1 - sse/sst, NA_real_),
    RMSE = sqrt(mean((Predicted - Actual)^2, na.rm = TRUE)),
    RSR  = { sd_o <- sd(Actual, na.rm = TRUE); ifelse(is.finite(sd_o) && sd_o > 0, RMSE / sd_o, NA_real_) },
    # PBIAS: Observed − Predicted (positive = underestimation)
    PBIAS = 100 * sum(Actual - Predicted, na.rm = TRUE) / sum(Actual, na.rm = TRUE),
    p_factor = {
      idx <- !is.na(CI_low_95) & !is.na(CI_high_95) & !is.na(Actual)
      if (any(idx)) mean(Actual[idx] >= CI_low_95[idx] & Actual[idx] <= CI_high_95[idx]) else NA_real_
    },
    r_factor = {
      idx <- !is.na(CI_low_95) & !is.na(CI_high_95) & !is.na(Actual)
      if (any(idx)) {
        width <- CI_high_95[idx] - CI_low_95[idx]
        sd_o  <- sd(Actual[idx], na.rm = TRUE)
        ifelse(is.finite(sd_o) && sd_o > 0, mean(width, na.rm = TRUE) / sd_o, NA_real_)
      } else NA_real_
    },
    .groups = "drop"
  ) %>%
  arrange(as.numeric(horizon))


metrics_hydro_tbl

######## 95th percentile 

library(dplyr)
library(tidyr)
library(yardstick)

# Horizons present (optional explicit ordering)
horizons <- c(1, 2, 3, 4, 5, 6, 12, 24, 48)

# 95th-percentile threshold from the test data
threshold <- quantile(results_wide$Actual, probs = 0.95, na.rm = TRUE)

# ---------- Table A: core metrics on high flows (>= 95th pct) ----------
high_df <- results_wide %>%
  filter(Actual >= threshold)

event_metrics <- high_df %>%
  group_by(horizon) %>%
  summarise(
    # Hydrology sign: Observed - Predicted (positive = underestimation)
    ME   = mean(Actual - Predicted, na.rm = TRUE),
    RMSE = sqrt(mean((Actual - Predicted)^2, na.rm = TRUE)),
    # R^2 as Pearson correlation-squared (distinct from NSE)
    R2   = rsq_vec(Actual, Predicted, na_rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(as.numeric(horizon)) %>%
  complete(horizon = horizons, fill = list(ME = NA_real_, RMSE = NA_real_, R2 = NA_real_))

event_metrics


# ---------- Table B: hydrology + uncertainty metrics on high flows ----------
metrics_tbl_high <- high_df %>%
  group_by(horizon) %>%
  summarise(
    # NSE = 1 - SSE/SST (SST uses the mean of high-flow observations for that horizon)
    NSE = {
      sse <- sum((Actual - Predicted)^2, na.rm = TRUE)
      sst <- sum((Actual - mean(Actual, na.rm = TRUE))^2, na.rm = TRUE)
      if (is.finite(sst) && sst > 0) 1 - sse/sst else NA_real_
    },
    # PBIAS (Observed - Predicted): positive = underestimation
    PBIAS = 100 * sum(Actual - Predicted, na.rm = TRUE) / sum(Actual, na.rm = TRUE),
    # RMSE and RSR = RMSE / sd(Actual)
    RMSE = sqrt(mean((Actual - Predicted)^2, na.rm = TRUE)),
    RSR  = {
      sd_o <- sd(Actual, na.rm = TRUE)
      if (is.finite(sd_o) && sd_o > 0) RMSE / sd_o else NA_real_
    },
    # P-factor: coverage of the predictive interval
    `P FACTOR` = {
      valid <- !is.na(Actual) & !is.na(CI_low_95) & !is.na(CI_high_95)
      if (any(valid)) mean(Actual[valid] >= CI_low_95[valid] & Actual[valid] <= CI_high_95[valid])
      else NA_real_
    },
    # R-factor: mean PI width / sd(Actual)
    `R FACTOR` = {
      valid <- !is.na(CI_low_95) & !is.na(CI_high_95) & !is.na(Actual)
      sd_o <- sd(Actual[valid], na.rm = TRUE)
      mean_width <- if (any(valid)) mean(CI_high_95[valid] - CI_low_95[valid], na.rm = TRUE) else NA_real_
      if (is.finite(sd_o) && sd_o > 0) mean_width / sd_o else NA_real_
    },
    .groups = "drop"
  ) %>%
  arrange(as.numeric(horizon)) %>%
  complete(horizon = horizons, fill = list(
    NSE = NA_real_, PBIAS = NA_real_, RSR = NA_real_, RMSE = NA_real_,
    `P FACTOR` = NA_real_, `R FACTOR` = NA_real_
  ))

metrics_tbl_high


