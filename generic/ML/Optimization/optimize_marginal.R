############################################################
# Marginal Economic Optimizer (MR >= 1) for wide predicted grids
# - Input: dom_list = list(below=DF, normal=DF, above=DF)
#   Each DF contains columns: x, y, location, yield.N.P ... (wide)
# - No K; columns are "yield.N.P"
# - Selects per-location marginal-optimal rate using MR = dDMR/dDTC
#   where DMR = yield * price_yield, DTC = N*cost_N + P2O5*cost_P2O5
# - Saves stage1 (expanded) and final picks per scenario, and returns a list
############################################################

calculate_marginal_optimal <- function(dom_list, out_dir,
                                       col1 = "yield.46.10",
                                       col2 = "yield.166.75",
                                       price_yield = 150,
                                       cost_N,
                                       cost_P2O5,
                                       n_cores = max(1, parallel::detectCores() - 1)) {
  # ---- Packages ----
  req <- c("dplyr","tidyr","foreach","doParallel","purrr","stringr","readr")
  to_install <- req[!(req %in% rownames(installed.packages()))]
  if (length(to_install)) install.packages(to_install)
  lapply(req, require, character.only = TRUE)
  
  message("💹 Starting marginal economic optimization (MR ≥ 1 rule)")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- Helpers ----
  # Convert elemental P -> P2O5 for cost calc
  ###IF-ELSE condition to calculate P2o% or skip
  P_to_P2O5 <- function(P_elem) (P_elem * 141.94) / 61.94
  
  # Robust finite difference after ordering by cost (DTC).
  # Deduplicate identical DTC points (keep row with highest yield, tie → min N then min P)
  calc_marginal <- function(lodData) {
    # Identify the wide yield columns by positions (like your NUE/PUE code)
    i1 <- which(names(lodData) == col1)
    i2 <- which(names(lodData) == col2)
    if (length(i1) == 0 || length(i2) == 0 || i2 < i1) return(NULL)
    
    long <- lodData %>%
      tidyr::pivot_longer(cols = i1:i2, names_to = "rate", values_to = "yield") %>%
      mutate(
        n    = as.numeric(sub("^.*?\\.(\\d+)\\..*", "\\1", rate)),
        p    = as.numeric(sub(".*\\.(\\d+)$", "\\1", rate)),
        P2O5 = P_to_P2O5(p),
        DMR  = yield * price_yield,
        DTC  = (n * cost_N) + (P2O5 * cost_P2O5)
      ) %>%
      select(-rate)
    
    # Need at least two distinct cost points
    if (!is.finite(mean(long$DTC)) || dplyr::n_distinct(long$DTC) < 2) return(NULL)
    
    # Deduplicate by DTC: keep best yield; tie-break by min N then min P
    long2 <- long %>%
      arrange(DTC, desc(yield), n, p) %>%
      group_by(DTC) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(DTC)
    
    # Differences (guard against zero or negative deltas)
    long2 <- long2 %>%
      mutate(
        DMR_prev = dplyr::lag(DMR),
        DTC_prev = dplyr::lag(DTC),
        dDMR = DMR - DMR_prev,
        dDTC = DTC - DTC_prev,
        MR_ratio = dDMR / dDTC
      )
    
    # Replace invalid/zero-cost deltas with NA (no economic meaning)
    long2$MR_ratio[!is.finite(long2$dDTC) | long2$dDTC <= 0] <- NA_real_
    
    long2
  }
  
  select_optimal <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # The MR at row i is calculated from previous step -> first MR is NA
    # Choose the last index where MR >= 1; else the point closest to 1
    valid_ge1 <- which(df$MR_ratio >= 1)
    if (length(valid_ge1) > 0) {
      pick <- tail(valid_ge1, 1)
      rule <- "MR>=1"
    } else {
      valid <- which(is.finite(df$MR_ratio))
      if (length(valid) == 0) return(NULL)
      pick <- valid[which.min(abs(df$MR_ratio[valid] - 1))]
      rule <- "closest_to_1"
    }
    
    out <- df[pick, , drop = FALSE]
    out$choice_rule <- rule
    out
  }
  
  run_scenario <- function(df, scen_name) {
    if (is.null(df) || !nrow(df)) {
      message(sprintf("⚠️ %s: empty input, skipping.", scen_name))
      return(tibble())
    }
    
    locs <- unique(df$location)
    message(sprintf("▶️ %s: %s locations", scen_name, length(locs)))
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    
    # ✅ export helper functions to workers
    clusterExport(cl, c("calc_marginal", "select_optimal", "P_to_P2O5"),
                  envir = environment())
    
    # ✅ load packages on workers
    clusterEvalQ(cl, {
      library(dplyr); library(tidyr)
    })
    
    on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
    
    # column positions for yield range
    i1 <- which(names(df) == col1)
    i2 <- which(names(df) == col2)
    
    results <- foreach::foreach(
      i = seq_along(locs),
      .packages = c("dplyr", "tidyr")
    ) %dopar% {
      loc <- locs[i]
      lod <- df[df$location == loc, , drop = FALSE]
      
      if (length(i1)==0 || length(i2)==0 || i2 < i1) return(NULL)
      
      m <- calc_marginal(lod)
      if (is.null(m) || nrow(m) < 2 || all(!is.finite(m$MR_ratio))) return(NULL)
      
      m$location <- loc
      m
    }
    
    stage1 <- dplyr::bind_rows(results[!vapply(results, is.null, logical(1))])
    saveRDS(stage1, file.path(out_dir, paste0("marginal_stage1_", scen_name, ".rds")))
    #write.csv(stage1, file.path(out_dir, paste0("marginal_stage1_", scen_name, ".csv")))
    if (!nrow(stage1)) {
      saveRDS(tibble(), file.path(out_dir, paste0("marginal_opt_", scen_name, ".rds")))
      return(tibble())
    }
    
    stage2 <- stage1 %>%
      group_by(location) %>%
      group_split() %>%
      purrr::map(~ select_optimal(.x %>% arrange(DTC))) %>%
      dplyr::bind_rows()
    
    if (nrow(stage2)) {
      stage2 <- stage2 %>%
        mutate(
          DAP_kg  = round(P2O5 / 0.46, 0),
          Urea_kg = round(n / 0.46, 0),
          n       = round(n, 0),
          p       = round(p, 0),
          P2O5    = round(P2O5, 0),
          yield   = round(yield, 0),
          DMR     = round(DMR, 0),
          DTC     = round(DTC, 02),
          MR_ratio = round(MR_ratio, 0),
          scenario = scen_name
        )
    }
    
    saveRDS(stage2, file.path(out_dir, paste0("marginal_opt_", scen_name, ".rds")))
    stage2
  }
  
  
  # ---- Run for scenarios ----
  below  <- run_scenario(dom_list$below,  "below")
  normal <- run_scenario(dom_list$normal, "normal")
  above  <- run_scenario(dom_list$above,  "above")
  
  combined <- dplyr::bind_rows(below, normal, above)
  saveRDS(combined, file.path(out_dir, "marginal_opt_all.rds"))
  
  message("✅ Marginal optimization complete")
  
  list(
    combined = combined,
    below = below,
    normal = normal,
    above = above
  )
}

# ############################################################
# # EXAMPLE CALL (uncomment and edit paths/objects if needed)
# dom_list <- list(
#   below  = dom_below[1:10, ],
#   normal = dom_normal[1:10, ],
#   above  = dom_above[1:10, ]
# )
# marg <- calculate_marginal_optimal(
#   dom_list,
#   out_dir = "Feben/Marginal_Results/",
#   col1 = "yield.46.10",
#   col2 = "yield.166.75",
#   price_yield = 150,
#   cost_N = 389,
#   cost_P2O5 = 152,
#   n_cores = 12
# )
#marg$combined %>% dplyr::as_tibble() %>% dplyr::glimpse()
############################################################
