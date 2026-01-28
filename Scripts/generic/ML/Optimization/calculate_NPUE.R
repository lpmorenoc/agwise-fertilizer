# calculate_NPUE.R
###Calculate the Fertilizer use efficiency particularly N and P

calculate_NPUE <- function(lodData, col1, col2){
  suppressPackageStartupMessages({library(dplyr); library(tidyr); library(rlang)})
  
  # Expect these columns to already exist in lodData
  need <- c("x","y","prob","location")
  miss <- setdiff(need, names(lodData))
  if (length(miss)) stop("calculate_NPUE(): missing columns: ", paste(miss, collapse=", "))
  
  long <- lodData %>%
    tidyr::gather(rate, yield, !!sym(col1):!!sym(col2)) %>%     # keep x,y,prob,location untouched
    mutate(
      n = as.numeric(sub("^.*?\\.(\\d+)\\..*", "\\1", rate)),
      p = as.numeric(sub(".*\\.(\\d+)$", "\\1", rate))
    ) %>%
    select(x, y, prob, location, yield, n, p)                   # explicit ordering
  
  # NUE by fixing P
  n_df <- bind_rows(lapply(unique(long$p), function(ps) {
    long %>% filter(p==ps) %>% arrange(n) %>%
      mutate(yld_diff = yield - first(yield),
             nue = yld_diff / (n - first(n))) %>%
      select(x, y, prob, location, yield, n, p, nue)
  }))
  
  # PUE by fixing N
  p_df <- bind_rows(lapply(unique(long$n), function(ns) {
    long %>% filter(n==ns) %>% arrange(p) %>%
      mutate(yld_diff = yield - first(yield),
             pue = yld_diff / (p - first(p))) %>%
      select(x, y, prob, location, yield, n, p, pue)
  }))
  
  out <- merge(n_df, p_df, by=c("x","y","prob","location","yield","n","p"))
  out <- out[out$n > 0 & out$p > 0, ]
  tibble::as_tibble(out)
}
