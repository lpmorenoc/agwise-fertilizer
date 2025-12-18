
###############################################################################
# This script prepares predicted yield response data for multiple
# climate scenarios (e.g., below/normal/above), then runs four optimization
# approaches to identify recommended fertilizer rates:
# (1) maximum yield, (2) maximum profit (given grain price and input costs),
# (3) marginal rate of return (MR ≥ 1 rule), and (4) nutrient use efficiency
# (NUE/PUE with optional NP ratio constraint). Results are saved to the
# specified output folders for downstream mapping and reporting.
# ===============================================================

if (!exists("progress_msg")) {
  progress_msg <- function(msg, linebreak=TRUE) {
    time <- format(Sys.time(), "%H:%M:%S")
    cat(paste0("[",time,"] ",msg))
    if (linebreak) cat("\n")
  }
}

load_packages <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
    library(p, character.only = TRUE)
  }
}

load_packages(c("terra","dplyr","tidyr","foreach","doParallel","parallel","rlang","tibble"))

setwd("Feben/opt2")

source("prepare_prediction_data.R")
source("optimize_efficiency.R")
source("optimize_yield.R")
source("optimize_profit.R")
source("optimize_marginal.R")

# Prepare data

dom_list <- prepare_predicted_data(
  base_dir = "~/shared-data/Data",
  crop_name = "Maize",
  sample_n = Inf   # or for the whole data use sample_n=Inf
)

### #####DO efficiency using maximum yield
best_yield  <- run_max_yield(dom_list, "yield.46.10", "yield.166.75", tolerance = 0.02)

#####DO efficiency using maximum profit
best_profit <- run_max_profit(dom_list, "yield.46.10", "yield.166.75",
                              price_yield = 150, cost_N = 389,
                              cost_P2O5 = 152, tolerance = 0.02)

####Do optimization using Marginal rate of return
marg <- calculate_marginal_optimal(
  dom_list,
  out_dir = "~/Feben/opt2/Outputs",
  col1 = "yield.46.10",
  col2 = "yield.166.75",
  price_yield = 150,
  cost_N = 389,
  cost_P2O5 = 152,
  n_cores = 12
)

###Do optimization using Nutrient Use Efficiency
eff <- calculate_efficiency_optimal(
  dom_list,
  out_dir = "~/Feben/opt2",
  col1="yield.46.10",
  col2="yield.166.75",
  n_cores = 12,
  NPratio_min = 1.5,
  nue_q = 0.70,
  pue_q = 0.70
)



