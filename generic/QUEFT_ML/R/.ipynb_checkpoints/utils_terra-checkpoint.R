################################################################################
# R/utils_terra.R
# Terra utilities including chunking helpers
################################################################################

#' Create chunk indices for splitting large vectors
#' @param n total length
#' @param chunk_size desired chunk size
#' @return list of integer indices
make_chunks <- function(n, chunk_size = 50000) {

  if (n <= chunk_size) {
    return(list(seq_len(n)))
  }

  starts <- seq(1, n, by = chunk_size)
  ends   <- pmin(starts + chunk_size - 1, n)

  mapply(seq, starts, ends, SIMPLIFY = FALSE)
}
################################################################################
