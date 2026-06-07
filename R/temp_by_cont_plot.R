#' Plot Temperature and Plastic Collected, by Continent
#'
#' Creates a scatterplot showing the relationship between mean annual
#' temperature and total plastic collected across countries, with points
#' colored by continent, a log-scaled y-axis, and a fitted linear trend line.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth
#' @importFrom ggplot2 scale_y_log10 labs
#' @importFrom ggplot2 theme_classic theme element_text
#'
#' @return A ggplot object.
#'
#' @export

temp_by_cont_plot <- function() {

  ggplot(load_plastic_data(), aes(x = AnnualMeanTemp, y = grand_total, color = continent)) +
    geom_point(alpha = 0.7, size = 3) +
    scale_y_log10(labels = scales::comma) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(
      title = "Temperature vs Plastic Collection (Log Scale)",
      x = "Mean Annual Temperature (°C)",
      y = "Total Plastic Collected (Log Scale)",
      color = "Continent"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold"))

}
