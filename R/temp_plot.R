#' Plot Temperature and Plastic Collected
#'
#' Shows the relationship between mean annual temperature and total
#' plastic collected using a hexbin plot of plastic counts, with a fitted linear
#' trend line and label of the country with the highest collection total.
#'
#' @importFrom dplyr filter slice_max
#' @importFrom ggplot2 ggplot aes geom_hex scale_fill_gradient
#' @importFrom ggplot2 geom_smooth geom_point geom_text labs
#'
#' @return A ggplot object.
#'
#' @export


plot_temp_plastic_hex <- function() {
  clean_data <- load_plastic_data() |>
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
