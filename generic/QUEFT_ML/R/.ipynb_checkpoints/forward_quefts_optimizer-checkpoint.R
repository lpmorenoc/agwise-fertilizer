################################################################################
# R/forward_quefts_optimizer.R
# Forward QUEFTS and NUE-based fertilizer optimization
################################################################################

#' Compute baseline yield (Y0) with zero fertilizer
#' @param INS_r SpatRaster of INS
#' @param IPS_r SpatRaster of IPS
#' @param IKS_r SpatRaster of IKS
#' @param Ya_r SpatRaster of attainable yield
#' @param crop crop name
#' @return SpatRaster of baseline yield
compute_baseline_yield <- function(INS_r, IPS_r, IKS_r, Ya_r, crop = "Maize") {

  message(">>> Computing baseline yield (Y0) with QUEFTS ...")

  f <- function(x) {
    if (any(is.na(x))) return(NA)
    s  <- x[1:3]
    Ya <- x[4]
    res <- try(
      run_quefts_single(
        nut_rates = data.frame(N=0,P=0,K=0),
        supply    = s,
        crop      = crop,
        Ya        = Ya
      ),
      silent = TRUE
    )
    if (inherits(res,"try-error")) return(NA)
    as.numeric(res)
  }

  Y0 <- terra::app(c(INS_r,IPS_r,IKS_r,Ya_r), fun = f)
  Y0
}

#' Evaluate NUE-based score for one fertilizer combo (per-pixel)
#' @param Nf,Pf,Kf numeric fertilizer rates
#' @param INS_r,IPS_r,IKS_r,Ya_r rasters
#' @param Y0 baseline yield raster
#' @param crop crop name
#' @param NUE_alpha exponent for NUE in objective
#' @return list(Y = SpatRaster, score = SpatRaster)
evaluate_combo_score <- function(
  Nf, Pf, Kf,
  INS_r, IPS_r, IKS_r, Ya_r,
  Y0,
  crop      = "Maize",
  NUE_alpha = 0.3
) {

  f <- function(x) {
    if (any(is.na(x))) return(c(NA,NA))
    s  <- x[1:3]
    Ya <- x[4]
    Y0_loc <- x[5]

    Y <- try(
      run_quefts_single(
        nut_rates = data.frame(N = Nf, P = Pf, K = Kf),
        supply    = s,
        crop      = crop,
        Ya        = Ya
      ),
      silent = TRUE
    )
    if (inherits(Y,"try-error") || is.na(Y)) return(c(NA,NA))

    dY   <- Y - Y0_loc
    AE_N <- if (Nf > 0) dY / Nf else 0
    AE_P <- if (Pf > 0) dY / Pf else 0
    AE_K <- if (Kf > 0) dY / Kf else 0
    NUE  <- (AE_N + AE_P + AE_K) / 3
    score <- Y * (NUE ^ NUE_alpha)

    c(Y, score)
  }

  ys  <- terra::app(c(INS_r,IPS_r,IKS_r,Ya_r,Y0), fun = f)
  list(Y = ys[[1]], score = ys[[2]])
}

#' Full forward QUEFTS optimization over NPK grid
#' @param INS_r,IPS_r,IKS_r SpatRasters of soil N,P,K supply
#' @param Ya_r SpatRaster of attainable yield
#' @param N_rates,P_rates,K_rates numeric vectors of rates
#' @param crop crop name
#' @param NUE_alpha NUE exponent
#' @param use_parallel logical, use future-based parallel over combos
#' @param show_progress logical, print progress messages
#' @return list(best_yield,bestN,bestP,bestK,Y0,yield_gap)
forward_quefts_optimize <- function(
  INS_r, IPS_r, IKS_r,
  Ya_r,
  N_rates, P_rates, K_rates,
  crop          = "Maize",
  NUE_alpha     = 0.3,
  use_parallel  = TRUE,
  show_progress = TRUE
) {

  Y0 <- compute_baseline_yield(INS_r, IPS_r, IKS_r, Ya_r, crop = crop)

  combos <- expand.grid(N = N_rates, P = P_rates, K = K_rates,
                        KEEP.OUT.ATTRS = FALSE)
  ncomb  <- nrow(combos)

  best_yield <- Y0 * NA
  best_score <- Y0 * NA
  best_N <- best_P <- best_K <- Y0 * NA

  # Function for one combo
  combo_fun <- function(i) {
    Nf <- combos$N[i]; Pf <- combos$P[i]; Kf <- combos$K[i]
    if (show_progress) {
      message(sprintf("  >> combo %d/%d: N=%g P=%g K=%g", i, ncomb, Nf, Pf, Kf))
    }
    evaluate_combo_score(
      Nf, Pf, Kf,
      INS_r, IPS_r, IKS_r, Ya_r,
      Y0,
      crop      = crop,
      NUE_alpha = NUE_alpha
    )
  }

  # Run combos (parallel or sequential)
  if (use_parallel && ncomb > 1) {
    res_list <- future.apply::future_lapply(seq_len(ncomb), combo_fun)
  } else {
    res_list <- lapply(seq_len(ncomb), combo_fun)
  }

  # Update "best" maps
  for (i in seq_len(ncomb)) {
    Nf <- combos$N[i]; Pf <- combos$P[i]; Kf <- combos$K[i]
    Y   <- res_list[[i]]$Y
    sc  <- res_list[[i]]$score

    imp <- (sc > best_score) | is.na(best_score)
    best_score[imp] <- sc[imp]
    best_yield[imp] <- Y[imp]
    best_N[imp]     <- Nf
    best_P[imp]     <- Pf
    best_K[imp]     <- Kf
  }

  yield_gap <- best_yield - Y0

  list(
    best_yield = best_yield,
    bestN      = best_N,
    bestP      = best_P,
    bestK      = best_K,
    Y0         = Y0,
    yield_gap  = yield_gap
  )
}
################################################################################
