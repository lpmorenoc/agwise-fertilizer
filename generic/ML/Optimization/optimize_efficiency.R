###This script optimizes using Resource Use Efficiency(RUE)
#Which is Fertilizer Use Efficiency(FUE)
###Ity accepts three predicted values that are predicted in the below, above an normal climate
#Scenarios

calculate_efficiency_optimal <- function(dom_list, out_dir,
                                         col1 = "yield.46.10",
                                         col2 = "yield.166.75",
                                         n_cores = max(1, parallel::detectCores() - 1),
                                         NPratio_min = 1.5,
                                         nue_q = 0.70,
                                         pue_q = 0.70) {
  
  # --- helpers ---
  format_dur <- function(secs) {
    secs <- round(secs)
    h <- secs %/% 3600; m <- (secs %% 3600) %/% 60; s <- secs %% 60
    paste0(sprintf("%02d", h), "h:", sprintf("%02d", m), "m:", sprintf("%02d", s), "s")
  }
  stamp <- function(t) format(t, "%Y-%m-%d %H:%M:%S")
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Start timing
  t_start <- Sys.time()
  message("[", stamp(t_start), "] Optimization using RUE initiated...")
  
  # Load NPUE
  source("~/shared-data/Scripts/generic/nutrientResponse/NUE_PUE.R")
  
  # Cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  doParallel::registerDoParallel(cl)
  
  parallel::clusterEvalQ(cl, {
    library(terra); library(tidyverse); library(sf); library(tidyr)
    source("~/shared-data/Scripts/generic/nutrientResponse/NUE_PUE.R")
    NULL
  })
  parallel::clusterExport(cl, varlist = c("col1","col2"), envir = environment())
  
  # --- runner (no timing here) ---
  run_scenario <- function(df, scen_name) {
    locs <- unique(df$location)
    res <- foreach::foreach(
      i = seq_along(locs),
      .packages = c('terra','tidyverse','sf','tidyr'),
      .errorhandling = "pass"
    ) %dopar% {
      locData <- df[df$location == locs[i], ]
      calculate_NPUE(locData, col1, col2)
    }
    res_df <- dplyr::bind_rows(res)
    saveRDS(res_df, file.path(out_dir, paste0("NUE_PUE_", scen_name, ".rds")))
    message("RUE calculation finished for ", scen_name)
    res_df
    
  }
  
  # --- STEP 1 ---
  npue_below  <- run_scenario(dom_list$below,  "below")
  npue_normal <- run_scenario(dom_list$normal, "normal")
  npue_above  <- run_scenario(dom_list$above,  "above")
  
  # --- STEP 2 ---
  clean_npue <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (!"location" %in% colnames(df)) df$location <- paste(df$x, df$y, sep = "_")
    df <- df[df$nue > 0 & df$pue > 0, ]
    if (nrow(df) == 0) return(NULL)
    df$P2O5 <- (df$p * 141.94) / 61.94
    df$NPratio <- df$n / df$P2O5
    df <- df[df$NPratio >= NPratio_min, ]
    df <- stats::na.omit(df)
    if (nrow(df) == 0) return(NULL)
    df
  }
  
  npue_below  <- clean_npue(npue_below)
  npue_normal <- clean_npue(npue_normal)
  npue_above  <- clean_npue(npue_above)
  
  # --- STEP 3 ---
  pick_optimal <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df %>%
      dplyr::group_by(location) %>%
      dplyr::filter(nue > stats::quantile(nue, nue_q, na.rm = TRUE),
                    pue > stats::quantile(pue, pue_q, na.rm = TRUE)) %>%
      dplyr::filter(yield == max(yield)) %>%
      dplyr::slice_min(n, with_ties = TRUE) %>%
      dplyr::slice_min(p, with_ties = TRUE) %>%
      dplyr::ungroup()
  }
  
  opt_below  <- pick_optimal(npue_below)
  opt_normal <- pick_optimal(npue_normal)
  opt_above  <- pick_optimal(npue_above)
  
  # --- STEP 4 ---
  final_opt <- dplyr::bind_rows(
    dplyr::mutate(opt_below,  scenario="below"),
    dplyr::mutate(opt_normal, scenario="normal"),
    dplyr::mutate(opt_above,  scenario="above")
  )
  
  saveRDS(final_opt, file.path(out_dir, "final_optimal_rates.rds"))
  saveRDS(opt_below,  file.path(out_dir, "below_optimal_rates.rds"))
  saveRDS(opt_normal, file.path(out_dir, "normal_optimal_rates.rds"))
  saveRDS(opt_above,  file.path(out_dir, "above_optimal_rates.rds"))
  utils::write.csv(final_opt, file.path(out_dir, "final_optimal_rates.csv"), row.names = FALSE)
  
  # End timing
  t_end <- Sys.time()
  total_secs <- as.numeric(difftime(t_end, t_start, units = "secs"))
  
  # Log + file
  message("[", stamp(t_end), "] ✅ Optimization complete in ", format_dur(total_secs))
  writeLines(c(
    paste0("Started: ", stamp(t_start)),
    paste0("Ended:   ", stamp(t_end)),
    paste0("Total:   ", format_dur(total_secs), " (", round(total_secs), " s)")
  ), con = file.path(out_dir, "timings.txt"))
  
  list(
    final_opt = final_opt,
    timing = list(
      start = t_start,
      end   = t_end,
      total_seconds = total_secs,
      total_pretty  = format_dur(total_secs)
    )
  )
}


# opt <- calculate_efficiency_optimal(
#   dom_list,
#   out_dir = "~/Feben/opt2",
#   col1="yield.46.10",
#   col2="yield.166.75",
#   n_cores = 12,
#   NPratio_min = 1.5,
#   nue_q = 0.70,
#   pue_q = 0.70
# )








