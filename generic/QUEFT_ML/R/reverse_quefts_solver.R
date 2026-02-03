################################################################################
# R/reverse_quefts_solver.R
# Reverse QUEFTS to estimate INS, IPS, IKS from ML maps
################################################################################

#' Run QUEFTS for given nutrient rates and soil supply
#' @param nut_rates data.frame with columns N,P,K (one or more rows)
#' @param supply numeric vector length 3 (INS,IPS,IKS)
#' @param crop crop name ("Maize","Potato","Rice")
#' @param Ya attainable yield (storage organ) in kg/ha
#' @param SeasonLength season length in days
#' @return numeric vector of yields (kg/ha), length = nrow(nut_rates)
run_quefts_single <- function(nut_rates, supply, crop = "Maize",
                              Ya, SeasonLength = 120) {

  crop <- match.arg(crop, c("Maize","Potato","Rice"))
  qCrop <- Rquefts::quefts_crop(crop)
  qSoil <- Rquefts::quefts_soil()

  qSoil$N_base_supply <- supply[1]
  qSoil$P_base_supply <- supply[2]
  qSoil$K_base_supply <- supply[3]

  hi <- data.frame(
    crop       = c("Maize","Potato","Rice"),
    leaf_ratio = c(0.46, 0.17, 0.18),
    stem_ratio = c(0.56, 0.14, 0.67)
  )

  qYa <- list(
    leaf_att   = Ya * hi$leaf_ratio[hi$crop == crop],
    stem_att   = Ya * hi$stem_ratio[hi$crop == crop],
    store_att  = Ya,
    SeasonLength = SeasonLength
  )

  Yq <- numeric(nrow(nut_rates))
  for (i in seq_len(nrow(nut_rates))) {
    qF <- list(N = nut_rates$N[i],
               P = nut_rates$P[i],
               K = nut_rates$K[i])
    q  <- Rquefts::quefts(qSoil, qCrop, qF, qYa)
    Yq[i] <- as.numeric(Rquefts::run(q)["store_lim"])
  }
  Yq
}

#' Reverse QUEFTS using two ML "observations" (control + max)
#' @param Yc_ml ML control yield (N=P=K=0)
#' @param Ymax_ml ML max yield (N=Nmax,P=Pmax,K=Kmax)
#' @param Nmax numeric N rate for max scenario
#' @param Pmax numeric P rate for max scenario
#' @param Kmax numeric K rate for max scenario
#' @param crop crop name
#' @param Ya attainable yield (kg/ha)
#' @param lambda regularization parameter
#' @param s0 prior supply vector (length 3)
#' @return numeric length 3 vector INS,IPS,IKS
reverse_quefts_two <- function(Yc_ml, Ymax_ml, Nmax, Pmax, Kmax,
                               crop = "Maize", Ya,
                               lambda = 1e-4,
                               s0 = c(30,5,30)) {

  ds <- rbind(
    data.frame(N = 0,     P = 0,    K = 0,    Y = Yc_ml),
    data.frame(N = Nmax,  P = Pmax, K = Kmax, Y = Ymax_ml)
  )

  obj <- function(s) {
    if (any(s < 0) || any(!is.finite(s))) return(1e18)
    pred <- run_quefts_single(ds[,c("N","P","K")], supply = s,
                              crop = crop, Ya = Ya)
    SSE  <- sum((pred - ds$Y)^2)
    pen  <- lambda * sum((s - s0)^2)
    SSE + pen
  }

  res <- optim(
    par   = pmax(s0, 1e-6),
    fn    = obj,
    method= "L-BFGS-B",
    lower = c(0,0,0),
    upper = c(5000,5000,5000)
  )
  res$par
}

#' Reverse QUEFTS for rasters (per-pixel), optionally parallel with chunks
#' @param Yc_r SpatRaster: control ML yield
#' @param Ymax_r SpatRaster: max ML yield
#' @param Ya_r SpatRaster: attainable yield
#' @param Nmax numeric N rate for max scenario
#' @param Pmax numeric P rate for max scenario
#' @param Kmax numeric K rate for max scenario
#' @param crop crop name
#' @param chunk_size cells per chunk
#' @param use_parallel logical
#' @return list(INS = SpatRaster, IPS = SpatRaster, IKS = SpatRaster)
solve_reverse_quefts_rasters <- function(
  Yc_r, Ymax_r, Ya_r,
  Nmax, Pmax, Kmax,
  crop        = "Maize",
  chunk_size  = 50000,
  use_parallel = TRUE
) {

  stopifnot(
    terra::ncell(Yc_r) == terra::ncell(Ymax_r),
    terra::ncell(Yc_r) == terra::ncell(Ya_r)
  )

  mat <- cbind(
    Yc   = terra::values(Yc_r),
    Ymax = terra::values(Ymax_r),
    Ya   = terra::values(Ya_r)
  )

  n  <- nrow(mat)
  ch <- make_chunks(n, chunk_size = chunk_size)

  solve_chunk <- function(idx) {
    sub <- mat[idx, , drop = FALSE]
    out <- matrix(NA_real_, nrow = nrow(sub), ncol = 3)
    colnames(out) <- c("INS","IPS","IKS")

    for (i in seq_len(nrow(sub))) {
      v <- sub[i,]
      if (any(is.na(v)) || v[1] < 0 || v[2] <= 0 || v[3] <= 0) next
      Yc  <- v[1]; Ym <- v[2]; Ya <- v[3]
      if (Ym < Yc) Ym <- Yc * 1.05

      s <- try(
        reverse_quefts_two(Yc, Ym, Nmax, Pmax, Kmax,
                           crop = crop, Ya = Ya,
                           lambda = 1e-4, s0 = c(30,5,30)),
        silent = TRUE
      )
      if (!inherits(s, "try-error")) {
        out[i,] <- s
      }
    }
    out
  }

  if (use_parallel && length(ch) > 1) {
    res_list <- future.apply::future_lapply(ch, solve_chunk)
  } else {
    res_list <- lapply(ch, solve_chunk)
  }

  res_mat <- do.call(rbind, res_list)

  INS_r <- Yc_r; terra::values(INS_r) <- res_mat[,"INS"]; names(INS_r) <- "INS"
  IPS_r <- Yc_r; terra::values(IPS_r) <- res_mat[,"IPS"]; names(IPS_r) <- "IPS"
  IKS_r <- Yc_r; terra::values(IKS_r) <- res_mat[,"IKS"]; names(IKS_r) <- "IKS"

  list(INS = INS_r, IPS = IPS_r, IKS = IKS_r)
}
################################################################################
