###############################################################
# Full function: run max profit optimization on dom_list
# dom_list = list(below=df, normal=df, above=df)
###############################################################
run_max_profit <- function(dom_list,
                           col1 = "yield.46.10",
                           col2 = "yield.166.75",
                           price_yield = 150,
                           cost_N = 70,
                           cost_P2O5 = 120,
                           tolerance = 0.02,
                           out_dir = tempdir()) {
  
  stopifnot(is.list(dom_list))
  if (!is.numeric(tolerance) || tolerance < 0 || tolerance >= 1) {
    stop("tolerance must be in [0, 1).")
  }
  
  # make sure output dir exists
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # -----------------------------------------------
  # internal: calculate max profit for one location
  # -----------------------------------------------
  calculate_max_profit <- function(lodData, col1, col2,
                                   price_yield, cost_N, cost_P2O5,
                                   tolerance) {
    
    # pivot the yield columns long
    # assumes yield rate columns exist and are ordered so col1:col2 is valid
    yield_cols <- rlang::set_names(c(col1, col2))
    if (!all(c(col1, col2) %in% names(lodData)))
      stop("One or both of the columns (col1/col2) are missing in lodData.")
    
    lodData_long <- lodData |>
      tidyr::pivot_longer(cols = {{col1}}:{{col2}},
                          names_to = "rate", values_to = "yield")
    
    # parse N and P from names like "yield.46.10"
    lodData_long$n <- as.numeric(sub("^.*?\\.(\\d+)\\..*", "\\1", lodData_long$rate))
    lodData_long$p <- as.numeric(sub(".*\\.(\\d+)$", "\\1", lodData_long$rate))
    
    # convert P -> P2O5 (factor ≈ 2.292)
    lodData_long$P2O5 <- (lodData_long$p * 141.94) / 61.94
    
    lodData_long <- lodData_long |>
      dplyr::mutate(
        revenue = yield * price_yield,
        cost    = (n * cost_N) + (P2O5 * cost_P2O5),
        profit  = revenue - cost
      )
    
    # If x, y, location exist, keep them; else create minimal placeholders
    have_xyz <- all(c("x","y","location") %in% names(lodData_long))
    if (!have_xyz) {
      lodData_long <- lodData_long |>
        dplyr::mutate(
          x = dplyr::if_else(!"x" %in% names(lodData_long), NA_real_, x),
          y = dplyr::if_else(!"y" %in% names(lodData_long), NA_real_, y),
          location = dplyr::if_else(!"location" %in% names(lodData_long),
                                    paste0("loc_", dplyr::row_number()), location)
        )
    }
    
    lodData_long <- lodData_long |>
      dplyr::select(x, y, location, yield, n, p, P2O5, profit)
    
    # handle all-NA profit (or empty)
    if (!any(is.finite(lodData_long$profit))) {
      out <- utils::head(lodData_long, 1)
      out$selection <- "max_profit"
      return(out)
    }
    
    max_profit_val <- max(lodData_long$profit, na.rm = TRUE)
    cutoff <- max_profit_val * (1 - tolerance)
    
    best <- lodData_long |>
      dplyr::filter(is.finite(profit), profit >= cutoff) |>
      dplyr::arrange(n, p) |>
      dplyr::slice(1)
    
    best$selection <- "max_profit"
    best
  }
  
  # -----------------------------------------------
  # loop through each scenario and each location
  # -----------------------------------------------
  scenarios <- names(dom_list)
  
  results_all <- lapply(scenarios, function(scen) {
    df <- dom_list[[scen]]
    
    if (!"location" %in% names(df)) {
      # create a location id if missing (x + y preferred if present)
      if (all(c("x","y") %in% names(df))) {
        df$location <- paste(df$x, df$y, sep = "_")
      } else {
        df$location <- paste0("loc_", seq_len(nrow(df)))
      }
    }
    
    locs <- unique(df$location)
    message("Running Max Profit optimization for: ", scen, " | Locations: ", length(locs))
    
    results_scen <- lapply(locs, function(loc) {
      lod <- df[df$location == loc, ]
      out <- calculate_max_profit(lod, col1, col2,
                                  price_yield, cost_N, cost_P2O5, tolerance)
      out$scenario <- scen
      out
    })
    
    dplyr::bind_rows(results_scen)
  })
  
  final <- dplyr::bind_rows(results_all)
  saveRDS(final, file.path(out_dir, "profit_opt.rds"))
  final
}
# best_profit <- run_max_profit(
#   dom_list,
#   col1 = "yield.46.10",
#   col2 = "yield.166.75",
#   price_yield = 150,
#   cost_N = 389,
#   cost_P2O5 = 152,
#   tolerance = 0.02,
#   out_dir = "~/Feben/opt2/Outputs"
# )















###Prev
# run_max_profit <- function(dom_list,
#                            col1 = "yield.46.10",
#                            col2 = "yield.166.75",
#                            price_yield = 150,
#                            cost_N = 70,
#                            cost_P2O5 = 120,
#                            tolerance = 0.02,out_dir) {
#   
#   # ----------------------------------------------------------
#   # internal function: calculate max profit for one location
#   # ----------------------------------------------------------
#   calculate_max_profit <- function(lodData, col1, col2,
#                                    price_yield, cost_N, cost_P2O5,
#                                    tolerance) {
#     
#     lodData_long <- lodData %>%
#       tidyr::gather(rate, yield, col1:col2)
#     
#     lodData_long$n <- as.numeric(sub("^.*?\\.(\\d+)\\..*", "\\1", lodData_long$rate))
#     lodData_long$p <- as.numeric(sub(".*\\.(\\d+)$", "\\1", lodData_long$rate))
#     
#     lodData_long$P2O5 <- (lodData_long$p * 141.94) / 61.94
#     
#     lodData_long <- lodData_long %>%
#       mutate(
#         revenue = yield * price_yield,
#         cost    = (n * cost_N) + (P2O5 * cost_P2O5),
#         profit  = revenue - cost
#       ) %>%
#       dplyr::select(x, y, location, yield, n, p, P2O5, profit)
#     
#     # Max profit and tolerance
#     max_profit_val <- max(lodData_long$profit, na.rm = TRUE)
#     cutoff <- max_profit_val * (1 - tolerance)
#     
#     best <- lodData_long %>%
#       dplyr::filter(profit >= cutoff) %>%
#       dplyr::arrange(n, p) %>%
#       dplyr::slice(1)
#     
#     best$selection <- "max_profit"
#     return(best)
#   }
#   
#   # ----------------------------------------------------------
#   # loop through each scenario and each location
#   # ----------------------------------------------------------
#   scenarios <- names(dom_list)
#   
#   results_all <- lapply(scenarios, function(scen) {
#     
#     df <- dom_list[[scen]]
#     locs <- unique(df$location)
#     
#     message("💰 Running Max Profit for: ", scen, " | Locations: ", length(locs))
#     
#     results_scen <- lapply(locs, function(loc) {
#       lod <- df[df$location == loc, ]
#       out <- calculate_max_profit(lod, col1, col2,
#                                   price_yield, cost_N, cost_P2O5,
#                                   tolerance)
#       out$scenario <- scen
#       return(out)
#     })
#     
#     dplyr::bind_rows(results_scen)
#   })
#   
#   final <- dplyr::bind_rows(results_all)
#   saveRDS(final, paste(out_dir,sep="/", "profit_opt.rds"))
#   return(final)
# }
# 
# 
# best_profit <- run_max_profit(dom_list, "yield.46.10", "yield.166.75",
#                               price_yield = 150, cost_N = 389,
#                               cost_P2O5 = 152, tolerance = 0.02,out_dir="~/Feben/opt2/Outputs")