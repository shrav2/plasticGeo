#' Plot Temperature and Plastic Collected
#'
#' Creates a hexbin plot comparing mean annual temperature and total plastic collected.
#'
#' @param data A data frame with AnnualMeanTemp, grand_total, and Country.
#'
#' @import ggplot2
#' @import dplyr
#' @return A ggplot object.
#'
#' @export
#'

plot_temp_plastic_hex <- function(data) {
  clean_data <- data |>
    filter(!is.na(AnnualMeanTemp), !is.na(grand_total), grand_total > 0)

  top_point <- clean_data |>
    slice_max(grand_total, n = 1)

  ggplot(clean_data, aes(x = AnnualMeanTemp, y = log(grand_total))) +
    geom_hex(bins = 25) +
    scale_fill_gradient(
      low = "lightblue",
      high = "royalblue4"
    ) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      color = "blue"
    ) +
    geom_point(
      data = top_point,
      color = "black",
      size = 3
    ) +
    geom_text(
      data = top_point,
      aes(label = Country),
      vjust = -0.8,
      size = 4
    ) +
    labs(
      x = "Mean Annual Temperature",
      y = "Log Total Plastic Collected",
      fill = "Number of Countries"
    )
}
