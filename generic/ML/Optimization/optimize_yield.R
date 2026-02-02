
# optimize_yield.R
########################################################
# Calculate Maximum Yield (wide table, yield.N.P)
# with tolerance = 0.02 (2%)
########################################################
###############################################################
# Run calculate_max_yield across all scenarios in dom_list
###############################################################

###############################################################
# Full function: run max yield optimization on dom_list
# dom_list = list(below=df, normal=df, above=df),
#out_dir is intended output file path
###############################################################

run_max_yield <- function(dom_list,
                          col1 = "yield.46.10",
                          col2 = "yield.166.75",
                          tolerance = 0.02, out_dir) {
  
  # --- Internal function: max yield per location ----
  calculate_max_yield <- function(lodData, col1, col2, tolerance = 0.02) {
    
    lodData_long <- lodData %>%
      tidyr::gather(rate, yield, col1:col2)
    
    lodData_long$n <- as.numeric(sub("^.*?\\.(\\d+)\\..*", "\\1", lodData_long$rate))
    lodData_long$p <- as.numeric(sub(".*\\.(\\d+)$", "\\1", lodData_long$rate))
    
    lodData_long <- lodData_long %>%
      dplyr::select(x, y, location, yield, n, p)
    
    max_yield_val <- max(lodData_long$yield, na.rm = TRUE)
    cutoff <- max_yield_val * (1 - tolerance)
    
    best <- lodData_long %>%
      dplyr::filter(yield >= cutoff) %>%
      dplyr::arrange(n, p) %>%
      dplyr::slice(1)
    
    best$selection <- "max_yield"
    return(best)
  }
  
  # --- Loop through scenarios and locations ----
  scenarios <- names(dom_list)
  
  results_all <- lapply(scenarios, function(scen) {
    
    df <- dom_list[[scen]]
    locs <- unique(df$location)
    
    message("▶️ Running Max Yield for: ", scen, " | Locations: ", length(locs))
    
    results_scen <- lapply(locs, function(loc) {
      lod <- df[df$location == loc, ]
      out <- calculate_max_yield(lod, col1, col2, tolerance)
      out$scenario <- scen
      return(out)
    })
    
    dplyr::bind_rows(results_scen)
  })
  
  final <- dplyr::bind_rows(results_all)
  saveRDS(final, file.path(out_dir, "Yield_max_opt.rds"))
  return(final)
}


#best_yield  <- run_max_yield(dom_list, "yield.46.10", "yield.166.75", tolerance = 0.02)
