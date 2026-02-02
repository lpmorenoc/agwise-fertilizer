################################################################################
# R/load_libraries.R
# Install and load all required packages
################################################################################

load_required_packages <- function() {

  required_pkgs <- c(
    "terra","sf","dplyr","readr","tibble","purrr","tidyr",
    "geodata","ggplot2","viridis","pbapply","parallel",
    "Rquefts","limSolve","h2o","rnaturalearth",
    "future","future.apply"
  )

  to_install <- setdiff(required_pkgs, rownames(installed.packages()))
  if (length(to_install) > 0) {
    install.packages(to_install, dependencies = TRUE)
  }

  suppressPackageStartupMessages({
    lapply(required_pkgs, require, character.only = TRUE)
  })

  invisible(TRUE)
}
################################################################################
