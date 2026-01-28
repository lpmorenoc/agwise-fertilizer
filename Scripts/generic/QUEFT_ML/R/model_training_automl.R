################################################################################
# R/model_training_automl.R
# H2O AutoML training for yield prediction
################################################################################

#' Initialize H2O with configured resources
#' @return logical TRUE on success
init_h2o <- function(nthreads, max_mem_size) {
  h2o::h2o.init(
    nthreads     = nthreads,
    max_mem_size = max_mem_size
  )
  invisible(TRUE)
}

#' Shutdown H2O safely
shutdown_h2o <- function() {
  try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE)
  invisible(TRUE)
}

#' Train yield model using H2O AutoML
#' @param ml_df Tibble with Y (numeric) and predictors
#' @param max_runtime_secs AutoML runtime budget
#' @param max_models Max models in AutoML
#' @param seed Seed for reproducibility
#' @param sort_metric Metric to select best model
#' @return list(model = H2O model, leaderboard = H2O leaderboard)
train_yield_model_automl <- function(
  ml_df,
  max_runtime_secs = 3600,
  max_models       = 30,
  seed             = 123,
  sort_metric      = "RMSE"
) {
  train_h2o <- h2o::as.h2o(ml_df)
  x_cols    <- setdiff(names(ml_df), "Y")

  aml <- h2o::h2o.automl(
    x                = x_cols,
    y                = "Y",
    training_frame   = train_h2o,
    max_runtime_secs = max_runtime_secs,
    max_models       = max_models,
    seed             = seed,
    sort_metric      = sort_metric,
    nfolds           = 5
  )

  leader <- aml@leader
  leaderboard <- aml@leaderboard

  list(model = leader, leaderboard = leaderboard)
}
################################################################################
