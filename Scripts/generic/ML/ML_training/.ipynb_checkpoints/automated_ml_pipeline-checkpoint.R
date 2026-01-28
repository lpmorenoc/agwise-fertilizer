

# ===============================================================
# AUTOMATED ML PIPELINE
# By Feben Assefa
# ===============================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(h2o, tidyverse, janitor, ggplot2, Metrics, vip, reshape2,shapviz)

#Read Data

read_data <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  
  if (ext == "csv") {
    data <- read.csv(file_path)
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Package 'readxl' is required to read Excel files.")
    data <- readxl::read_excel(file_path)
  } else if (ext %in% c("rds", "rda", "rdata", "RDS", "RData")) {
    # Case 1: .RDS (.rds)
    if (ext == "rds" || ext == "RDS") {
      data <- readRDS(file_path)
      
    } else {
      # Case 2: .RData (.rda, .RData)
      e <- new.env()
      loaded_objects <- load(file_path, envir = e)
      
      if (length(loaded_objects) == 1) {
        data <- e[[loaded_objects]]
      } else {
        warning(
          "Multiple objects found in .RData file. Returning a list.\nObjects: ",
          paste(loaded_objects, collapse = ", ")
        )
        data <- mget(loaded_objects, envir = e)
      }
    }
  } else {
    stop("Unsupported file type. Use .csv, .xlsx, .xls, .rds, or .RData/.Rdata/.rda")
  }
  # If returned object is a data frame → clean names
  if (is.data.frame(data)) {
    data <- janitor::clean_names(data)
  }
  return(data)
}

# Prepare Data for H2O
prepare_data <- function(data, target, split_ratio = 0.8) {
  h2o.init(max_mem_size = "6G", nthreads = -1)
  set.seed(42)
  split <- h2o.splitFrame(as.h2o(data), ratios = split_ratio, seed = 42)
  train <- split[[1]]
  test  <- split[[2]]
  list(train = train, test = test)
}

# Train initial H2O AutoML model

train_model <- function(train, target, pathModel,path_performance) {
  aml <- h2o.automl(y = target,
                    training_frame = train,
                    max_models = 10,
                    exclude_algos = c("StackedEnsemble"),
                    seed = 42)
  best_model <- aml@leader
  message("best model: ", best_model@algorithm)
  # Ensure output directory exists
  dir.create(pathModel, showWarnings = FALSE, recursive = TRUE)
  
  # Save model (correct order: object, path, force)
  saved_path <- h2o::h2o.saveModel(
    object = best_model,
    path   = pathModel,
    force  = TRUE
  )
  message("Model saved to: ", saved_path)
  
  # Return both the model and the saved file path
  return(list(model = best_model, saved_path = saved_path, aml = aml))
}

# 4. Evaluate model performance

evaluate_model <- function(model, train, test, target, path_performance = NULL) {
  
  # accept list(model=...), model_id, or H2OModel
  if (is.list(model) && !is.null(model$model)) model <- model$model
  if (is.character(model) && length(model) == 1) model <- h2o::h2o.getModel(model)
  
  stopifnot(inherits(model, "H2OModel"))
  stopifnot(inherits(train, "H2OFrame"))
  stopifnot(inherits(test,  "H2OFrame"))
  
  train_perf <- h2o::h2o.performance(model, train)
  test_perf  <- h2o::h2o.performance(model, test)
  
  metrics <- data.frame(
    Dataset = c("Train", "Test"),
    RMSE = c(h2o::h2o.rmse(train_perf), h2o::h2o.rmse(test_perf)),
    MAE  = c(h2o::h2o.mae(train_perf),  h2o::h2o.mae(test_perf)),
    R2   = c(h2o::h2o.r2(train_perf),   h2o::h2o.r2(test_perf))
  )
  
  saved_to <- NULL
  
  # ---- SAFE SAVE ----
  if (!is.null(path_performance) && is.character(path_performance) && nzchar(path_performance)) {
    
    path_performance <- path.expand(path_performance)
    
    # if directory OR ends with "/", write default filename
    if (dir.exists(path_performance) || grepl("/$", path_performance)) {
      path_performance <- file.path(path_performance, "performance_metrics.csv")
    }
    
    out_dir <- dirname(path_performance)
    if (!is.na(out_dir) && nzchar(out_dir)) {
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
      utils::write.csv(metrics, path_performance, row.names = FALSE)
      saved_to <- path_performance
    }
  }
  
  invisible(list(metrics = metrics, train_perf = train_perf, test_perf = test_perf, saved_to = saved_to))
}

## %. Tuning the best model from H2o
tune_best <- function(best_model, train_df, target, seed = 1234,
                      pathModel_tuned = NULL) {
  
  stopifnot(inherits(best_model, "H2OModel"))
  stopifnot(inherits(train_df, "H2OFrame"))
  
  
  algo <- tolower(best_model@algorithm)
  y <- target
  x <- setdiff(names(train_df), y)
  
  grid_id <- paste0("grid_", algo, "_", as.integer(Sys.time()))
  
  if (algo == "gbm") {
    
    hyper_params <- list(
      ntrees      = seq(100, 1000, 100),
      max_depth   = seq(2, 16, 2),
      learn_rate  = seq(0.01, 0.1, 0.02),
      sample_rate = seq(0.6, 1.0, 0.1),
      col_sample_rate = seq(0.6, 1.0, 0.1)
    )
    
    grid <- h2o::h2o.grid(
      algorithm = "gbm", grid_id = grid_id,
      x = x, y = y,
      training_frame = train_df,
      hyper_params = hyper_params,
      seed = seed
    )
    
  } else if (algo == "xgboost") {
    
    hyper_params <- list(
      ntrees      = seq(200, 800, 100),
      max_depth   = seq(3, 12, 2),
      learn_rate  = seq(0.01, 0.1, 0.03),
      sample_rate = seq(0.6, 1.0, 0.1),
      col_sample_rate = seq(0.6, 1.0, 0.1)
    )
    
    grid <- h2o::h2o.grid(
      algorithm = "xgboost", grid_id = grid_id,
      x = x, y = y,
      training_frame = train_df,
      hyper_params = hyper_params,
      seed = seed
    )
    
  } else if (algo == "drf") {
    
    hyper_params <- list(
      ntrees      = seq(200, 1000, 200),
      max_depth   = seq(5, 20, 5),
      sample_rate = seq(0.6, 1.0, 0.1),
      mtries      = seq(2, max(2, length(x)), 2)
    )
    
    grid <- h2o::h2o.grid(
      algorithm = "drf", grid_id = grid_id,
      x = x, y = y,
      training_frame = train_df,
      hyper_params = hyper_params,
      seed = seed
    )
    
  } else if (algo == "deeplearning") {
    
    hyper_params <- list(
      hidden      = list(c(50,50), c(100,100), c(200,100,50)),
      epochs      = c(50, 100, 200),
      activation  = c("Rectifier", "Tanh"),
      l1          = c(0, 1e-5, 1e-4),
      l2          = c(0, 1e-5, 1e-4)
    )
    
    grid <- h2o::h2o.grid(
      algorithm = "deeplearning", grid_id = grid_id,
      x = x, y = y,
      training_frame = train_df,
      hyper_params = hyper_params,
      seed = seed
    )
    
  } else if (algo == "glm") {
    
    hyper_params <- list(
      alpha  = seq(0, 1, 0.1),
      lambda = seq(1e-6, 1e-2, length.out = 10)
    )
    
    grid <- h2o::h2o.grid(
      algorithm = "glm", grid_id = grid_id,
      x = x, y = y,
      training_frame = train_df,
      family = "gaussian",
      hyper_params = hyper_params,
      seed = seed
    )
    
  } else if (algo == "knn") {
    
    hyper_params <- list(k = seq(5, 50, 5))
    
    grid <- h2o::h2o.grid(
      algorithm = "knn", grid_id = grid_id,
      x = x, y = y,
      training_frame = train_df,
      hyper_params = hyper_params,
      seed = seed
    )
    
  } else if (algo == "svm") {
    
    hyper_params <- list(
      C = c(0.1, 1, 10),
      gamma = c(0.01, 0.1, 1)
    )
    
    grid <- h2o::h2o.grid(
      algorithm = "svm", grid_id = grid_id,
      x = x, y = y,
      training_frame = train_df,
      hyper_params = hyper_params,
      seed = seed
    )
    
  } else {
    stop("Unsupported algorithm for tuning: ", algo)
  }
  
  # Select best model from grid
  grid_perf <- h2o::h2o.getGrid(grid_id, sort_by = "rmse", decreasing = FALSE)
  best_model_id <- grid_perf@model_ids[[1]]
  best_tuned <- h2o::h2o.getModel(best_model_id)
  
  message("✅ Tuning complete for ", algo, " | Best tuned model ID: ", best_model_id)
  
  # Save best tuned model (ONLY if path provided)
  saved_path <- NULL
  if (!is.null(pathModel_tuned)) {
    pathModel_tuned <- path.expand(pathModel_tuned)
    dir.create(pathModel_tuned, showWarnings = FALSE, recursive = TRUE)
    
    saved_path <- h2o::h2o.saveModel(
      object = best_tuned,
      path   = pathModel_tuned,
      force  = TRUE
    )
    message("✅ Tuned model saved to: ", saved_path)
  }
  
  return(list(model = best_tuned, saved_path = saved_path, grid_id = grid_id))
}


# 5. Retrain with subset of top predictors

retrain_h2o_with_params_subset <- function(train_df, target, best_model, predictors_subset, seed = 42) {
  # Ensure input validity
  stopifnot(inherits(best_model, "H2OModel"))
  
  algo <- tolower(best_model@algorithm)
  params <- best_model@allparameters
  x <- predictors_subset
  y <- target
  new_model_id <- paste0("retrained_subset_", best_model@model_id, "_", as.integer(Sys.time()))
  
  set.seed(seed)
  
  # Reuse tuned hyperparameters from best_model directly
  model <- switch(algo,
                  "gbm" = h2o.gbm(
                    x = x, y = y, training_frame = train_df, model_id = new_model_id,
                    ntrees = params$ntrees,
                    max_depth = params$max_depth,
                    learn_rate = params$learn_rate,
                    sample_rate = params$sample_rate,
                    col_sample_rate = params$col_sample_rate,
                    min_rows = params$min_rows %||% 1,
                    seed = seed
                  ),
                  
                  "xgboost" = h2o.xgboost(
                    x = x, y = y, training_frame = train_df, model_id = new_model_id,
                    ntrees = params$ntrees,
                    max_depth = params$max_depth,
                    learn_rate = params$learn_rate,
                    subsample = params$subsample,
                    colsample_bytree = params$colsample_bytree,
                    min_child_weight = params$min_child_weight,
                    seed = seed
                  ),
                  
                  "drf" = h2o.randomForest(
                    x = x, y = y, training_frame = train_df, model_id = new_model_id,
                    ntrees = params$ntrees,
                    max_depth = params$max_depth,
                    sample_rate = params$sample_rate,
                    mtries = params$mtries,
                    seed = seed
                  ),
                  "glm" = h2o.glm(
                    x = x, y = y, training_frame = train_df,
                    model_id = new_model_id,
                    family = "gaussian",
                    alpha = params$alpha,
                    lambda = params$lambda,
                    seed = seed
                  ),
                  
                  "knn" = h2o.knn(
                    x = x, y = y, training_frame = train_df,
                    model_id = new_model_id,
                    k = params$k,
                    seed = seed
                  ),
                  
                  "svm" = h2o.svm(
                    x = x, y = y, training_frame = train_df,
                    model_id = new_model_id,
                    C = params$C,
                    gamma = params$gamma,
                    seed = seed
                  ),
                  
                  "decision_tree" = h2o.gbm(
                    x = x, y = y, training_frame = train_df,
                    model_id = new_model_id,
                    ntrees = 1,
                    max_depth = params$max_depth,
                    min_rows = params$min_rows,
                    learn_rate = 1,
                    seed = seed
                  ),
                  "deeplearning" = h2o.deeplearning(
                    x = x, y = y, training_frame = train_df, model_id = new_model_id,
                    hidden = params$hidden,
                    epochs = params$epochs,
                    l1 = params$l1,
                    l2 = params$l2,
                    rate = params$rate %||% 0.01,
                    activation = params$activation %||% "Rectifier",
                    reproducible = TRUE,
                    seed = seed
                  ),
                  
                  stop("Algorithm not implemented for retraining subset")
  )
  
  message("Model retrained successfully using top variables and tuned hyperparameters.")
  return(model)
}

# 6. Variable importance selection + retrain

auto_varimp_selection_and_retrain <- function(baseline_model,
                                              train_df, test_df, target,
                                              try_ns = c(15, 20, 25, 30),
                                              seed = 42,
                                              verbose = TRUE,
                                              out_dir = NULL) {
  
  stopifnot(inherits(baseline_model, "H2OModel"))
  stopifnot(inherits(train_df, "H2OFrame"))
  stopifnot(inherits(test_df,  "H2OFrame"))

  # ---- output dirs (optional) ----
  if (!is.null(out_dir)) {
    out_dir <- path.expand(out_dir)
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(out_dir, "performance"), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(out_dir, "models"), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
  }
  
  # ---- baseline performance ----
  baseline_perf_path <- if (!is.null(out_dir)) file.path(out_dir, "performance", "baseline_metrics.csv") else NULL
  baseline_metrics <- evaluate_model(baseline_model, train_df, test_df, target,
                                     path_performance = baseline_perf_path)
  
  baseline_rmse_test <- baseline_metrics$metrics |>
    dplyr::filter(Dataset == "Test") |>
    dplyr::pull(RMSE) |>
    as.numeric()
  
  if (verbose) message("Baseline test RMSE: ", round(baseline_rmse_test, 6))
  
  # ---- variable importance ranking ----
  vi <- as.data.frame(h2o::h2o.varimp(baseline_model)) |>
    dplyr::arrange(dplyr::desc(relative_importance))
  
  vars_ranked <- vi$variable
  
  if (!is.null(out_dir)) {
    utils::write.csv(vi, file.path(out_dir, "tables", "baseline_varimp.csv"), row.names = FALSE)
  }
  
  # ---- candidate predictor sets ----
  candidate_sets <- list()
  for (n in try_ns) {
    candidate_sets[[paste0("top", n)]] <- vars_ranked[1:min(length(vars_ranked), n)]
  }
  candidate_sets$all <- vars_ranked
  
  compare_tbl <- tibble::tibble(candidate = character(),
                                n_vars = integer(),
                                rmse_test = numeric())
  
  best_model_sel <- NULL
  best_rmse_test <- Inf
  best_candidate <- NULL
  
  # ---- loop candidates ----
  for (nm in names(candidate_sets)) {
    preds <- candidate_sets[[nm]]
    if (verbose) message("Trying candidate: ", nm)
    
    subset_model <- retrain_h2o_with_params_subset(train_df, target, baseline_model, preds, seed)
    
    perf_path <- if (!is.null(out_dir)) file.path(out_dir, "performance", paste0("metrics_", nm, ".csv")) else NULL
    metrics <- evaluate_model(subset_model, train_df, test_df, target, path_performance = perf_path)
    
    rmse_test <- metrics$metrics |>
      dplyr::filter(Dataset == "Test") |>
      dplyr::pull(RMSE) |>
      as.numeric()
    
    compare_tbl <- dplyr::bind_rows(
      compare_tbl,
      tibble::tibble(candidate = nm, n_vars = length(preds), rmse_test = rmse_test)
    )
    
    # save model (optional)
    if (!is.null(out_dir)) {
      h2o::h2o.saveModel(subset_model, path = file.path(out_dir, "models"), force = TRUE)
    }
    
    if (is.finite(rmse_test) && rmse_test < best_rmse_test) {
      best_rmse_test <- rmse_test
      best_candidate <- nm
      best_model_sel <- subset_model
    }
  }
  
  if (!is.null(out_dir)) {
    utils::write.csv(compare_tbl, file.path(out_dir, "tables", "varimp_retrain_comparison.csv"), row.names = FALSE)
  }
  
  message("\n✅ Best retrained model: ", best_candidate, " | Test RMSE = ", round(best_rmse_test, 3))
  
  return(list(
    selected = best_candidate,
    final_model = best_model_sel,
    compare = compare_tbl,
    baseline_rmse_test = baseline_rmse_test
  ))
}

# ---------------------------
# 7. Model analytics
# ---------------------------

model_analytics <- function(model,
                            test_h2o,
                            target,
                            out_dir,
                            top_n_shap = 10,
                            top_n_pdp  = 10,
                            num_of_features = 20,
                            shap_row = 1,
                            seed = 42) {
  
  set.seed(seed)
  stopifnot(inherits(model, "H2OModel"))
  
  # --- Ensure H2OFrame ---
  if (!inherits(test_h2o, "H2OFrame")) test_h2o <- h2o::as.h2o(test_h2o)
  
  # --- Output folders ---
  out_dir <- path.expand(out_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  png_dir    <- file.path(out_dir, "plots_png")
  pdf_dir    <- file.path(out_dir, "report")
  tables_dir <- file.path(out_dir, "tables")
  dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
  
  pdf_file <- file.path(pdf_dir, "model_analytics_all_plots.pdf")
  
  # Open ONE multipage PDF for all plots
  grDevices::pdf(pdf_file, width = 9, height = 6)
  on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
  
  algo <- tolower(model@algorithm)
  y <- target
  x <- setdiff(colnames(test_h2o), y)
  
  # --- Packages for plotting ---
  has_gg <- requireNamespace("ggplot2", quietly = TRUE)
  has_shapviz <- requireNamespace("shapviz", quietly = TRUE)
  
  # --- Helper: save ggplot to PNG and also print into the open PDF ---
  save_gg_png_and_pdf <- function(p, png_name, width = 9, height = 6, dpi = 300) {
    if (!has_gg) return(invisible(NULL))
    ggplot2::ggsave(
      filename = file.path(png_dir, paste0(png_name, ".png")),
      plot = p,
      width = width, height = height, dpi = dpi
    )
    print(p) # goes to the open PDF device
    invisible(NULL)
  }
  
  # --- Helper: save base plots (H2O partialPlot, varimp_plot, shap_summary_plot) ---
  save_base_png_and_pdf <- function(png_name, expr_plot,
                                    width_in = 9, height_in = 6, dpi = 300) {
    grDevices::png(file.path(png_dir, paste0(png_name, ".png")),
                   width = width_in, height = height_in, units = "in", res = dpi)
    try(expr_plot, silent = TRUE)
    grDevices::dev.off()
    
    # also add to PDF (already open)
    try(expr_plot, silent = TRUE)
    invisible(NULL)
  }
  
  message("Generating model analytics (Perf, PDP/ICE, SHAP)...")
  message("Model algorithm: ", model@algorithm)
  
  # ============================================================
  # 1) Performance
  # ============================================================
  perf <- h2o::h2o.performance(model, test_h2o)
  rmse <- h2o::h2o.rmse(perf)
  mae  <- h2o::h2o.mae(perf)
  r2   <- h2o::h2o.r2(perf)
  
  perf_tbl <- data.frame(algorithm = algo, RMSE = rmse, MAE = mae, R2 = r2)
  utils::write.csv(perf_tbl, file.path(tables_dir, "performance_metrics.csv"), row.names = FALSE)
  
  # ============================================================
  # 2) Observed vs predicted + residuals (ggplot)
  # ============================================================
  pred <- h2o::h2o.predict(model, test_h2o)
  
  df_plot <- as.data.frame(h2o::h2o.cbind(test_h2o[, y], pred))
  colnames(df_plot) <- c("observed", "predicted")
  df_plot$residual <- df_plot$observed - df_plot$predicted
  utils::write.csv(df_plot, file.path(tables_dir, "observed_predicted_residuals.csv"), row.names = FALSE)
  
  if (has_gg) {
    p_scatter <- ggplot2::ggplot(df_plot, ggplot2::aes(observed, predicted)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_abline(slope = 1, intercept = 0) +
      ggplot2::labs(title = paste0("Observed vs Predicted (", algo, ")")) +
      ggplot2::theme_minimal()
    save_gg_png_and_pdf(p_scatter, "observed_vs_predicted")
  }
  
  # ============================================================
  # 3) H2O varimp plot (built-in)
  # ============================================================
  save_base_png_and_pdf(
    png_name = paste0("varimp_plot_top", num_of_features),
    expr_plot = {
      h2o::h2o.varimp_plot(model, num_of_features = num_of_features)
    }
  )
  
  # ============================================================
  # 4) PDP + ICE (built-in partialPlot) for top vars
  # ============================================================
  vi <- tryCatch(as.data.frame(h2o::h2o.varimp(model)), error = function(e) NULL)
  if (!is.null(vi) && nrow(vi) > 0) {
    pdp_vars <- head(vi$variable, min(top_n_pdp, nrow(vi)))
  } else {
    pdp_vars <- head(x, min(top_n_pdp, length(x)))
  }
  
  for (v in pdp_vars) {
    safe_v <- gsub("[^A-Za-z0-9_\\-]", "_", v)
    save_base_png_and_pdf(
      png_name = paste0("pdp_ice_", safe_v),
      expr_plot = { h2o::h2o.partialPlot(object = model, newdata = test_h2o, cols = v) }
    )
  }
  
  # ============================================================
  # 5) H2O SHAP summary plot (built-in)
  # ============================================================
  save_base_png_and_pdf(
    png_name = "h2o_shap_summary_plot",
    expr_plot = {
      h2o::h2o.shap_summary_plot(model, test_h2o, top_n_features = num_of_features)
    }
  )
  
  # ============================================================
  # 6) Optional: shapviz SHAP (only if supported)
  # ============================================================
  shap_contrib <- tryCatch(h2o::h2o.predict_contributions(model, test_h2o),
                           error = function(e) NULL)
  
  if (!is.null(shap_contrib) && has_shapviz && has_gg) {
    shap_df <- as.data.frame(shap_contrib)
    X_df <- as.data.frame(test_h2o[, x, drop = FALSE])
    
    bias_name <- intersect(c("BiasTerm", "biasTerm"), names(shap_df))
    
    baseline <- 0
    if (length(bias_name) == 1) {
      baseline <- mean(shap_df[[bias_name]], na.rm = TRUE)  # <-- scalar
    }
    
    shap_mat <- as.matrix(shap_df[, setdiff(names(shap_df), bias_name), drop = FALSE])
    
    sv <- shapviz::shapviz(shap_mat, X = X_df, baseline = baseline)
    
    
    p_bee <- shapviz::sv_importance(sv, kind = "bee", max_display = top_n_shap)
    save_gg_png_and_pdf(p_bee, "shap_beeswarm_shapviz")
    
    p_bar <- shapviz::sv_importance(sv, kind = "bar", max_display = top_n_shap)
    save_gg_png_and_pdf(p_bar, "shap_importance_bar_shapviz")
  }
  
  message("✅ Done. PNGs saved in: ", normalizePath(png_dir, winslash = "/", mustWork = FALSE))
  message("✅ Combined PDF saved at: ", normalizePath(pdf_file, winslash = "/", mustWork = FALSE))
  
  invisible(list(perf = perf, rmse = rmse, mae = mae, r2 = r2,
                 pdf_report = pdf_file, png_dir = png_dir))
}


# ===============================================================
# Let us try the pipeline
# ===============================================================
data <- read_data("~/Feben/RWA/Data/modelReady_bean.csv") |> select(-c("x_1","x","y","uid"))
target <- "grain_yield_kgpha"

data <- data[1:1000,]
# Prepare Data
data_h2o <- prepare_data(data, target)
train_h2o <- data_h2o$train
test_h2o  <- data_h2o$test

# Train Initial Model
best_h2o <- train_model(train_h2o, target,pathModel= "/home/jovyan/Feben/ML")##Give absolute path anytime you save h2o models

# Evaluate
evaluate_model(best_h2o$model, train_h2o, test_h2o, target,path_performance="~/Feben/ML")

###Tune model
tuned <- tune_best(best_h2o$model, train_h2o, target,
                   pathModel_tuned = "/home/jovyan/Feben/ML/tuned")

tuned_model <- tuned$model

# Retrain using Top Variables
varimp_h2o <- auto_varimp_selection_and_retrain(tuned_model, train_h2o, test_h2o, target, try_ns = c(15, 20, 25, 30),out_dir ="~/Feben/ML")
final_h2o <- varimp_h2o$final_model
##to be examined

# Analytics
model_analytics(
  model   = final_h2o,
  test_h2o = test_h2o,
  target  = "grain_yield_kgpha",
  out_dir = "~/Feben/ML/plots",
  top_n_shap = 5,
  shap_row = 5
)
