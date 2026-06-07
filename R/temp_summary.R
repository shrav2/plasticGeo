#' Create total plastic summary tables
#'
#' Summarizes total plastic collected by either continent or annual temperature
#' bins, calculating mean, median, and standard deviation values and formatting
#' the results in gt table with colored summary columns.
#'
#' @param group Choose "continent" or "temp_bin".
#'
#' @return A gt table.
#'
#' @importFrom gt gt fmt_number cols_label tab_header tab_spanner data_color
#' @importFrom gt tab_style cell_text cells_column_labels cells_column_spanners cells_title
#' @importFrom scales col_numeric
#'
#' @export

temp_summary <- function(group = c("continent", "temp_bin")) {
  group <- match.arg(group)

  plastic_data <- load_plastic_data()

  if (group == "continent") {
    summary_data <- plastic_data |>
      dplyr::group_by(continent) |>
      dplyr::summarise(
        annual_mean_temp = mean(AnnualMeanTemp, na.rm = TRUE),
        mean = mean(grand_total, na.rm = TRUE),
        median = median(grand_total, na.rm = TRUE),
        sd = sd(grand_total, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(annual_mean_temp)

    summary_data |>
      gt() |>
      fmt_number(columns = c(annual_mean_temp, mean, sd), decimals = 2) |>
      fmt_number(columns = median, decimals = 0) |>
      cols_label(
        continent = "Continent",
        annual_mean_temp = "Annual Mean Temp (°C)",
        mean = "Mean",
        median = "Median",
        sd = "SD"
      ) |>
      tab_header(
        title = "Total Plastic Collected Summary by Continent"
      ) |>
      tab_spanner(
        label = "Total Plastic Collected",
        columns = c(mean, median, sd)
      ) |>
      data_color(
        columns = annual_mean_temp,
        fn = col_numeric(
          palette = c("#CFECEC", "#D8BFD8"),
          domain = NULL
        )
      ) |>
      color_total_cols()

  } else {
    summary_data <- plastic_data |>
      dplyr::mutate(
        temp_bin = cut(
          AnnualMeanTemp,
          breaks = c(-Inf, 10, 15, 20, 25, Inf),
          labels = c("<10°C", "10–15°C", "15–20°C", "20–25°C", "25°C+"),
          right = FALSE
        )
      ) |>
      dplyr::group_by(temp_bin) |>
      dplyr::summarise(
        mean = mean(grand_total, na.rm = TRUE),
        median = median(grand_total, na.rm = TRUE),
        sd = sd(grand_total, na.rm = TRUE),
        .groups = "drop"
      )

    summary_data |>
      gt() |>
      fmt_number(columns = c(mean, sd), decimals = 2) |>
      fmt_number(columns = median, decimals = 0) |>
      cols_label(
        temp_bin = "Annual Mean Temp (°C)",
        mean = "Mean",
        median = "Median",
        sd = "SD"
      ) |>
      tab_header(
        title = "Total Plastic Collected Summary Across Temperature Bins"
      ) |>
      tab_spanner(
        label = "Total Plastic Collected",
        columns = c(mean, median, sd)
      ) |>
      data_color(
        columns = temp_bin,
        palette = c("#ADD8E6", "#D8BFD8")
      ) |>
      color_total_cols()
  }
}

color_total_cols <- function(table) {
  table |>
    data_color(
      columns = mean,
      fn = col_numeric(
        palette = c("#C3F5F0", "#7393B3"),
        domain = NULL
      )
    ) |>
    data_color(
      columns = median,
      fn = col_numeric(
        palette = c("#C3F5F0", "#7393B3"),
        domain = NULL
      )
    ) |>
    data_color(
      columns = sd,
      fn = col_numeric(
        palette = c("#C3F5F0", "#7393B3"),
        domain = NULL
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(cells_column_labels(), cells_column_spanners())
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title(groups = "title")
    )
}
