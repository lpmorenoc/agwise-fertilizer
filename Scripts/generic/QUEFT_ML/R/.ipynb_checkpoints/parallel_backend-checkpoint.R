################################################################################
# R/parallel_backend.R
# Set up future-based parallel backend
################################################################################

#' Configure parallel backend using futures
#' @param use_parallel logical, whether to use parallel or not
#' @param workers integer, number of workers (if NULL, auto detect - 1)
#' @return invisibly TRUE
setup_parallel <- function(use_parallel = TRUE, workers = NULL) {

  if (!use_parallel) {
    future::plan(future::sequential)
    message(">>> Parallel disabled: using sequential plan.")
    return(invisible(TRUE))
  }

  if (is.null(workers)) {
    workers <- max(1L, parallel::detectCores() - 1L)
  }

  future::plan(future::multisession, workers = workers)
  message(">>> Parallel enabled with ", workers, " workers.")

  invisible(TRUE)
}
################################################################################
