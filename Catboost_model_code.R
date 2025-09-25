# --- Full Forecast Setup for Multiple Horizons with Save Points ---

library(catboost)
library(tidyverse)
library(yardstick)
library(rsample)
library(scoringRules)
library(rBayesianOptimization)
library(readr)
library(dplyr)
library(lubridate)

# ------------------ Data ------------------
dat <- read_csv("Data/dat2.csv")

dat <- dat %>%
  mutate(
    hour  = hour(time),
    month = month(time),
    day   = day(time)
  ) %>%
  filter(!is.na(TRE_height_metres_value))  # target cannot be NA

# Forecast horizons
horizons <- c(48)

# Feature sets
target_base <- "TRE_height_metres_value"

lagged_features <- grep("_lag_h[0-9]+$", names(dat), value = TRUE)
context_features <- c("hour", "month", "day")

discharge_features <- c(
  "TRG_discharge_cumecs_value",
  "CC_discharge_cumecs_value",
  "TRE_discharge_cumecs_value"
)

rainfall_features <- c("MRU_rainfall_mm_value", "TRE_rainfall_mm_value")

level_features <- c(
  "CC_height_deriv_value",
  "CC_height_metres_value",
  "TRE_height_deriv_value",
  "TRE_height_metres_value",
  "TRG_height_deriv_value",
  "TRG_height_metres_value"
)

features <- c(
  lagged_features,
  context_features,
  discharge_features,
  rainfall_features,
  level_features
)

# Clean NaN/Inf
dat <- dat %>%
  mutate(across(all_of(features), ~ ifelse(is.nan(.), NA, .))) %>%
  filter(if_all(all_of(features), ~ is.finite(.) | is.na(.)))

# Lead columns for all horizons
for (h in horizons) {
  dat[[paste0(target_base, "_lead", h)]] <- dplyr::lead(dat[[target_base]], h)
}

# ------------------ Splits ------------------
split_index <- floor(0.8 * nrow(dat))
dat_train <- dat[1:split_index, ]                 # 80%
dat_test  <- dat[(split_index + 1):nrow(dat), ]   # 20%

train_index <- floor(0.75 * nrow(dat_train))      # 75% of 80% = 60%
dat_train_final <- dat_train[1:train_index, ]     # 60%
dat_val         <- dat_train[(train_index + 1):nrow(dat_train), ]  # 20%

# ------------------ Helpers ------------------

# Always return a numeric vector of mean predictions of length nrow(df)
safe_predict_mean <- function(model, df, features) {
  pool <- catboost.load_pool(df[, features, drop = FALSE])
  preds <- catboost.predict(model, pool, prediction_type = "RawFormulaVal")
  # Some builds may still return a matrix; take first column if so
  if (is.matrix(preds)) preds <- preds[, 1]
  as.numeric(preds)
}

# Chunked prediction for RMSEWithUncertainty (returns mean & aleatoric SD)
predict_mu_sigma_chunks <- function(model, df, features, chunk = 5000) {
  n <- nrow(df)
  mu <- numeric(n)
  sd <- numeric(n)
  i <- 1
  while (i <= n) {
    j <- min(i + chunk - 1, n)
    pool <- catboost.load_pool(df[i:j, features, drop = FALSE])
    ms   <- catboost.predict(model, pool, prediction_type = "RMSEWithUncertainty")
    # Ensure matrix and grab columns safely
    if (!is.matrix(ms)) ms <- cbind(ms, rep(NA_real_, length(ms)))
    mu[i:j] <- ms[, 1]
    # CatBoost returns SD (scale) in col 2 for RMSEWithUncertainty
    sd[i:j] <- ms[, 2]
    i <- j + 1
  }
  list(mean = mu, data_sd = sd)
}

# Chunked prediction for Virtual Ensembles (returns epistemic variance)
predict_ve_var_chunks <- function(model, df, features, ve_count = 16, chunk = 3000) {
  n <- nrow(df)
  kvar <- numeric(n)
  i <- 1
  while (i <= n) {
    j <- min(i + chunk - 1, n)
    pool <- catboost.load_pool(df[i:j, features, drop = FALSE])
    ve   <- catboost.predict(
      model, pool,
      prediction_type = "VirtEnsembles",
      virtual_ensembles_count = ve_count
    )
    # If ve is a vector (ve_count == 1), coerce to matrix
    if (is.vector(ve)) ve <- matrix(ve, ncol = 1)
    kvar[i:j] <- apply(ve, 1, var)
    i <- j + 1
  }
  pmax(kvar, 0)
}

# Compute hydrology stats equivalent to your Python block
compute_stats <- function(measured, mean_hat, lower, upper) {
  # replicate Python's strict inequalities
  p_vec   <- (upper > measured) & (lower < measured)
  r_vec   <- upper - lower
  
  pfactor <- mean(p_vec, na.rm = TRUE)
  nse <- 1 - sum((measured - mean_hat)^2, na.rm = TRUE) /
    sum((measured - mean(measured, na.rm = TRUE))^2, na.rm = TRUE)
  
  # PBIAS (positive => underestimation), common hydrology convention
  pbias <- 100 * sum(measured - mean_hat, na.rm = TRUE) / sum(measured, na.rm = TRUE)
  
  rmse <- sqrt(mean((measured - mean_hat)^2, na.rm = TRUE))
  rsr  <- rmse / stats::sd(measured, na.rm = TRUE)
  rfactor <- mean(r_vec, na.rm = TRUE) / stats::sd(measured, na.rm = TRUE)
  
  list(
    pfactor = pfactor,
    nse = nse,
    pbias = pbias,
    rfactor = rfactor,
    rsr = rsr
  )
}

print_stats_table <- function(stats_list) {
  stats_tbl <- tibble::tibble(
    parameter = c("NSE", "PBIAS", "RSR", "P factor", "R factor"),
    value     = c(stats_list$nse, stats_list$pbias, stats_list$rsr,
                  stats_list$pfactor, stats_list$rfactor)
  )
  stats_tbl %>% mutate(value = sprintf("%.4f", value)) %>% print(n = Inf)
}

# Objective for BayesianOptimization (per horizon) — uses RMSE on validation mean
catboost_cv_bayes <- function(iterations,
                              learning_rate,
                              depth,
                              l2_leaf_reg,
                              random_strength,
                              bagging_temperature,
                              leaf_estimation_iterations,
                              min_data_in_leaf,
                              horizon) {
  target_col <- paste0(target_base, "_lead", horizon)
  
  # Build NA-free frames for this horizon
  tr_idx <- !is.na(dat_train_final[[target_col]])
  va_idx <- !is.na(dat_val[[target_col]])
  train_df <- dat_train_final[tr_idx, , drop = FALSE]
  val_df   <- dat_val[va_idx, , drop = FALSE]
  
  # Pools
  train_pool <- catboost.load_pool(train_df[, features, drop = FALSE],
                                   label = train_df[[target_col]])
  val_pool   <- catboost.load_pool(val_df[, features, drop = FALSE],
                                   label = val_df[[target_col]])
  
  params <- list(
    loss_function = "RMSEWithUncertainty",
    iterations = round(iterations),
    learning_rate = learning_rate,
    depth = round(depth),
    l2_leaf_reg = l2_leaf_reg,
    random_strength = random_strength,
    bagging_temperature = bagging_temperature,
    leaf_estimation_iterations = round(leaf_estimation_iterations),
    min_data_in_leaf = round(min_data_in_leaf),
    random_seed = 42,
    od_type = "Iter",
    od_wait = 30,
    verbose = 0
  )
  
  model <- catboost.train(train_pool, val_pool, params)
  
  # ---- RMSE on validation (mean predictions ONLY) ----
  preds_mean <- safe_predict_mean(model, val_df, features)
  actuals    <- val_df[[target_col]]
  
  # extra guard
  if (length(preds_mean) != nrow(val_df)) {
    stop(sprintf("Length mismatch: preds=%d rows, val_df=%d rows",
                 length(preds_mean), nrow(val_df)))
  }
  
  rmse_val <- yardstick::rmse_vec(actuals, preds_mean, na_rm = TRUE)
  
  list(Score = -rmse_val, Pred = 0)
}

# ------------------ Main loop ------------------
for (h in horizons) {
  cat("\n--- Optimizing for horizon:", h, "hours ---\n")
  
  opt_result <- BayesianOptimization(
    FUN = function(...)
      catboost_cv_bayes(..., horizon = h),
    bounds = list(
      iterations = c(300L, 1500L),
      learning_rate = c(0.01, 0.3),
      depth = c(4L, 10L),
      l2_leaf_reg = c(0.1, 30),
      random_strength = c(0.1, 10),
      bagging_temperature = c(0.0, 1.0),
      leaf_estimation_iterations = c(1L, 20),
      min_data_in_leaf = c(1L, 100)
    ),
    init_points = 10,
    n_iter = 40,
    acq = "ucb",
    verbose = TRUE
  )
  
  best_params <- as.list(opt_result$Best_Par)
  
  final_params <- list(
    loss_function = "RMSEWithUncertainty",
    iterations = round(best_params$iterations),
    learning_rate = best_params$learning_rate,
    depth = round(best_params$depth),
    l2_leaf_reg = best_params$l2_leaf_reg,
    random_strength = best_params$random_strength,
    bagging_temperature = best_params$bagging_temperature,
    leaf_estimation_iterations = round(best_params$leaf_estimation_iterations),
    min_data_in_leaf = round(best_params$min_data_in_leaf),
    random_seed = 42,
    od_type = "Iter",
    od_wait = 30,
    verbose = 0
  )
  
  target_col <- paste0(target_base, "_lead", h)
  
  # NA-free frames for this horizon (again, for final model + stats)
  tr_idx <- !is.na(dat_train_final[[target_col]])
  va_idx <- !is.na(dat_val[[target_col]])
  te_idx <- !is.na(dat_test[[target_col]])
  train_df <- dat_train_final[tr_idx, , drop = FALSE]
  val_df   <- dat_val[va_idx, , drop = FALSE]
  test_df  <- dat_test[te_idx, , drop = FALSE]
  
  # Pools
  train_pool <- catboost.load_pool(train_df[, features, drop = FALSE],
                                   label = train_df[[target_col]])
  val_pool <- catboost.load_pool(val_df[, features, drop = FALSE],
                                 label = val_df[[target_col]])
  
  # Train final model
  model <- catboost.train(train_pool, val_pool, final_params)
  
  # Save model and parameters
  model_filename <- paste0("catboost_model_RMSEWithUncertainty_h", h, ".cbm")
  param_filename <- paste0("best_params_RMSEWithUncertainty_h", h, ".rds")
  catboost.save_model(model, model_filename)
  saveRDS(best_params, file = param_filename)
  
  # -------- Pick split for stats (VAL to mirror Python, or TEST) --------
  stats_df <- if (stats_split == "val") val_df else test_df
  
  # Mean + aleatoric SD (chunked)
  ms   <- predict_mu_sigma_chunks(model, stats_df, features, chunk = 5000)
  mu   <- ms$mean
  sd_a <- pmax(ms$data_sd, 0)       # aleatoric SD
  var_a <- sd_a^2
  
  # Epistemic variance via virtual ensembles (chunked)
  var_e <- predict_ve_var_chunks(model, stats_df, features, ve_count = ve_count, chunk = 3000)
  
  total_sd <- sqrt(pmax(var_a + var_e, 0))
  
  # Build 'result' equivalent to Python
  result <- tibble::tibble(
    measured  = stats_df[[target_col]],  # actuals at t+h (ground truth)
    mean      = mu,                      # predicted mean
    knowledge = var_e,                   # epistemic variance
    data      = var_a,                   # aleatoric variance
    total     = total_sd,
    lower     = mean - 1.96 * total,
    upper     = mean + 1.96 * total
  ) %>%
    mutate(
      p = as.integer((upper > measured) & (lower < measured)),  # strict, like Python
      r = upper - lower
    )
  
  # ---- Compute stats (NSE, PBIAS, RSR, P-factor, R-factor) ----
  stats <- compute_stats(
    measured = result$measured,
    mean_hat = result$mean,
    lower    = result$lower,
    upper    = result$upper
  )
  
  cat(sprintf("\n### Statistics (h = %d, split = %s)\n", h, stats_split))
  print_stats_table(stats)
  
  # Optional: save predictions/bands CSV
  if (save_preds) {
    out <- result %>%
      mutate(time = stats_df$time, .before = measured)
    readr::write_csv(out, paste0("predictions_", stats_split, "_h", h, ".csv"))
  }
}
