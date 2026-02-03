################################################################################
# R/plotting.R
# Quick-look plotting utilities (simple ggplot maps)
################################################################################

#' Plot a SpatRaster as a quick ggplot map
#' @param r SpatRaster (single layer)
#' @param title map title
#' @return ggplot object
plot_raster_quick <- function(r, title = "") {

  df <- as.data.frame(r, xy = TRUE)
  nm <- names(r)

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = .data[[nm]])) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c(na.value = NA) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, fill = nm) +
    ggplot2::theme_minimal()
}
################################################################################
