
###############################################################################
# Fertilizer Recommendation Optimization Framework
#
# This script provides a unified interface for selecting fertilizer 
# recommendation strategies based on predicted crop response surfaces.
#
# ✅ Supported Optimization Engines
# 1) Maximum Yield Optimization
#    - Objective: Select the fertilizer rate combination (N, P) that 
#      achieves the maximum predicted yield, allowing a small tolerance
#      to avoid inefficient input use.
#    - Method: run_max_yield()
#    - Key Idea: Choose the highest yield; if multiple solutions achieve
#      ≥ (1 - tolerance) * MaxYield, pick the one with minimum fertilizer inputs.
#
# 2) Maximum Profit Optimization
#    - Objective: Maximize net return (Revenue – Fertilizer Cost)
#    - Method: run_max_profit()
#    - Key Idea: Evaluate cost of N & P and select the combination that 
#      produces the highest profit within a tolerance of the maximum.
#
# 3) Nutrient Use Efficiency (NUE/PUE) Optimization
#    - Objective: Select fertilizer rates that maximize agronomic nutrient efficiency
#      while also considering yield and minimum input use.
#    - Method: optimize_efficiency()
#    - Key Filters:
#         * NUE ≥ NUE quantile threshold (e.g., 70th percentile)
#         * PUE ≥ PUE quantile threshold (e.g., 70th percentile)
#         * (Optional) N:P2O5 ratio > threshold (e.g., 1.5)
#         * Among candidates → choose highest yield → lowest N → lowest P
#    - Use when you want agronomic efficiency instead of purely economic or yield focus.
#
# 4) Marginal Rate of Return (MRR) Optimization
#    - Objective: Apply CIMMYT/FAO economic rule: choose the fertilizer rate 
#      where the Marginal Rate of Return (ΔReturn/ΔCost) ≥ 1 (or closest to 1)
#    - Method: calculate_marginal_optimal()
#    - Key Idea: Invest fertilizer only until the value of an additional 
#      birr spent equals or exceeds one birr in return (MRR >= 1).
#
# ------------------------------------------------------------------------------
# ✅ Optimizer Selector Interface
#
# optimizer_selector(method, dom_list, params = list())
#
# - method : One of "yield", "profit", "NUE", "MRR"
# - dom_list : List of data frames for below / normal / above climate outcomes
# - params : Named list of parameters to override defaults for each engine
#
# Example Usage:
#   optimizer_selector("NUE", dom_list,
#                      params = list(nue_quant = 0.8, pue_quant = 0.75))
#
# ------------------------------------------------------------------------------
# ✅ Parameters Required Per Optimization Engine
#
# ⬤ Yield Optimization (run_max_yield)
#      tolerance     = allowable yield loss fraction (default 0.02)
#      col1, col2    = prediction column names (e.g., yield.46.10 to yield.166.75)
#
# ⬤ Profit Optimization (run_max_profit)
#      price_yield   = market price per yield unit
#      cost_N        = cost per kg of nitrogen
#      cost_P2O5     = cost per kg of P2O5
#      tolerance     = allowable profit loss fraction
#      col1, col2
#
# ⬤ NUE/PUE Optimization (optimize_efficiency)
#      nue_quant     = NUE threshold percentile (0–1)
#      pue_quant     = PUE threshold percentile (0–1)
#      use_np_ratio  = TRUE/FALSE apply N:P2O5 rule
#      np_ratio_cut  = required N:P2O5 ratio threshold
#      n_cores       = parallel cores
#      col1, col2, out_dir
#
# ⬤ Marginal Rate of Return (calculate_marginal_optimal)
#      price_yield   = market price per yield unit
#      cost_N        = cost per kg N
#      cost_P2O5     = cost per kg P2O5
#      n_cores
#      col1, col2, out_dir
#
# ------------------------------------------------------------------------------
# ✅ Summary
#
# This framework provides a flexible modular system to:
#   • Predict crop yield under different fertilizer levels
#   • Apply agronomic, economic, and efficiency criteria
#   • Generate site-specific fertilizer recommendations
#
# The optimizer_selector() function routes the request to the correct engine
# and allows the user to override default parameters using a params list.
#
# Recommended use:
#   • Start with NE/PUE or MRR to balance agronomy & economics
#   • Compare against Yield & Profit selectors in post-analysis
#
###############################################################################

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
#source("optimizer_selecter.R")

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



#--------------------------------------------------------------------------------
##The yield distributions of the four yields
library(tidyverse)

# --- Load your four datasets ---
# Replace these with your actual data frames
df1 <- read_csv("eff_opt.csv")   # e.g., below
df2 <- read.csv("marginal_opt.csv")   # e.g., normal
df3 <- read.csv("best_yield.csv")   # e.g., above
df4 <- read.csv("best_profit.csv")   # e.g., observed or model2

# --- Assume each dataset has a column named `yield` ---
df1$type <- "NUE_opt"
df2$type <- "marginal_opt"
df3$type <- "max_yield_opt"
df4$type <- "max_profit_opt"

# --- Combine ---
df_all <- bind_rows(df1, df2, df3, df4)

# --- Calculate mean yield per dataset ---
mean_df <- df_all %>%
  group_by(type) %>%
  summarize(mean_yield = mean(yield, na.rm = TRUE))

# --- Plot ---
ggplot(df_all, aes(x = type, y = yield, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  geom_point(data = mean_df, aes(x = type, y = mean_yield),
             size = 3, color = "black") +
  geom_text(data = mean_df, aes(x = type, y = mean_yield, 
                                label = sprintf("%.2f", mean_yield)),
            vjust = -1.2, color = "black", size = 4) +
  labs(title = "Yield Distribution Across Scenarios",
       x = "Scenario",
       y = "Yield (kg/ha)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))



library(tidyverse)

# --- your datasets ---
df1$scenario <- "NUE_opt"
df2$scenario <- "marginal_opt"
df3$scenario <- "max_yield_opt"
df4$scenario <- "max_profit_opt"

df_all <- bind_rows(df1, df2, df3, df4)

# compute means
mean_df <- df_all %>%
  group_by(scenario) %>%
  summarise(mean_yield = mean(yield, na.rm = TRUE))

ggplot(df_all, aes(x = yield, fill = scenario)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.4) +
  geom_density(alpha = 0.7, linewidth = 1) +
  geom_vline(data = mean_df, aes(xintercept = mean_yield, color = scenario),
             linetype = "dashed", linewidth = 1) +
  geom_text(data = mean_df, aes(x = mean_yield, y = 0,
                                label = sprintf("Mean: %.1f", mean_yield)),
            vjust = -1.2, size = 3.8, fontface = "bold") +
  facet_wrap(~scenario, scales = "free_y") +
  labs(title = "Yield Distribution Across Scenarios",
       x = "Yield (kg/ha)",
       y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

# --- Combine all datasets ---
df_all <- bind_rows(df1, df2, df3, df4)

# --- Compute mean yield per scenario ---
mean_df <- df_all %>%
  group_by(scenario) %>%
  summarize(mean_yield = mean(yield, na.rm = TRUE))

# --- Plot all in one graph ---
ggplot(df_all, aes(x = yield, color = scenario)) +
  geom_density(alpha = 0.7, linewidth = 1.4) +
  geom_vline(data = mean_df, aes(xintercept = mean_yield, color = scenario),
             linetype = "dashed", linewidth = 1) +
  geom_point(data = mean_df, aes(x = mean_yield, y = 0), size = 3) +
  geom_text(data = mean_df, aes(x = mean_yield, y = 0,
                                label = paste0("Mean=", round(mean_yield,1))),
            vjust = -1.4, size = 4, fontface = "bold") +
  labs(
    title = "Yield Distribution Across Scenarios",
    x = "Yield (kg/ha)",
    y = "Density",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


std_result <- function(df, method_label){
  df %>%
    mutate(
      method = method_label,
      P2O5 = ifelse("P2O5" %in% names(df), P2O5, (p * 141.94) / 61.94),
      profit = ifelse("profit" %in% names(df), profit, NA_real_)
    ) %>%
    select(location, scenario, n, p, P2O5, yield, profit, method) %>%
    distinct()
}

results_all <- dplyr::bind_rows(
  std_result(df3, "MaxYield"),
  std_result(df4, "MaxProfit"),
  std_result(df1, "NUE_PUE"),
  std_result(df2, "MRR")
)


agreement <- results_all %>%
  count(location, n, P2O5) %>%
  group_by(location) %>%
  summarize(
    methods_agree = max(n),
    .groups="drop"
  )

agreement

f <- ggplot(results_all, aes(n, yield, color = method)) +
  geom_point(size=2) +
  labs(title="Yield response across optimizers")


ggplot(results_all, aes(method, n, fill=method)) + geom_boxplot() +
  labs(title="N recommendation by optimizer")

ggplot(results_all, aes(method, p, fill=method)) + geom_boxplot() +
  labs(title="P recommendation by optimizer")

ggplot(results_all, aes(n, yield, fill=method)) + geom_boxplot() +
  labs(title="N recommendation by optimizer")

ggplot(results_all, aes(p, yield, fill=method)) + geom_boxplot() +
  labs(title="P recommendation by optimizer")








# optimizer_selector <- function(method="yield"){
#   
#   if(method=="yield"){
#     #####DO efficiency using maximum yield
#     best_yield  <- run_max_yield(dom_list, "yield.46.10", "yield.166.75", tolerance = 0.02)
#     
#   }
#   else if(method=="profit"){
#     message("Optimizing using maximum profit")
#     best_profit <- run_max_profit(dom_list, "yield.46.10", "yield.166.75",
#                                   price_yield = 150, cost_N = 389,
#                                   cost_P2O5 = 152, tolerance = 0.02)
#   }
#   else if(method=="NUE"){
#     eff <- optimize_efficiency(
#       dom_list,
#       out_dir = "Outputs/Efficiency/",
#       nue_quant = 0.70,
#       pue_quant = 0.70,
#       use_np_ratio = TRUE,
#       np_ratio_cut = 1.5
#     )
#   }
#   else if(method=="MRR"){
#     marg <- calculate_marginal_optimal(
#       dom_list,
#       out_dir = "Feben/Marginal_Results/",
#       col1 = "yield.46.10",
#       col2 = "yield.166.75",
#       price_yield = 150,
#       cost_N = 389,
#       cost_P2O5 = 152,
#       n_cores = 12
#     )
#   }
#   else{
#     message("Invalid input")
#   }
# }
# 
# optimizer_selector(method="profit")


# eff <- optimizer_selector("NUE", dom_list,
#                           params = list(nue_quant = 0.8, pue_quant = 0.75, np_ratio_cut = 2.0)
# )
# 
# 
# m <- optimizer_selector(
#   methods = c("NUE"),dom_list,
#   params = list(
#     NUE   = list(nue_quant = 0.75),
#     YIELD = list(tolerance = 0.01)
#   )
# )

