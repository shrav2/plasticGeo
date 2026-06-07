#' Linear regression of Total Plastic Collected on Mean Annual Temperature
#'
#' Fits a simple linear regression model using mean annual temperature to
#' predict total plastic collected (with an optional a log transformation)
#' and formats the coefficient estimates and model fit statistics as a gt table.
#'
#' @return A gt table with simple linear regression coefficient estimates,
#' standard errors, t-tests, p-values, and model fit statistics.
#'
#' @importFrom stats lm
#' @importFrom broom tidy glance
#' @importFrom dplyr mutate recode
#' @importFrom gt gt tab_header cols_label fmt_number
#' @importFrom gt tab_source_note tab_style
#' @importFrom gt cell_text cell_fill
#' @importFrom gt cells_title cells_column_labels cells_body
#'
#' @export

analyze_plastic <- function(log_transform = TRUE) {

  data <- load_plastic_data()

  if (log_transform) {
    plastic.fit <- lm(log(grand_total) ~ AnnualMeanTemp, data = data)
    subtitle_text <- "log(Grand Total Plastic Collected) vs. Mean Annual Temperature"
  } else {
    plastic.fit <- lm(grand_total ~ AnnualMeanTemp, data = data)
    subtitle_text <- "Grand Total Plastic Collected vs. Mean Annual Temperature"
  }

  coef_df <- tidy(plastic.fit) |>
    mutate(
      term = recode(
        term,
        "(Intercept)" = "Intercept",
        "AnnualMeanTemp" = "Mean Annual Temperature"
      )
    )

  stats <- glance(plastic.fit)

  coef_df |>
    gt() |>
    tab_header(
      title = "Linear Regression Results",
      subtitle = subtitle_text
    ) |>
    cols_label(
      term = "Variable",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t value",
      p.value = "p-value"
    ) |>
    fmt_number(
      columns = c(estimate, std.error, statistic),
      decimals = 2
    ) |>
    fmt_number(
      columns = p.value,
      decimals = 3
    ) |>
    tab_source_note(
      source_note = paste0(
        "R² = ", round(stats$r.squared, 3),
        " | Adjusted R² = ", round(stats$adj.r.squared, 3)
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_title(groups = c("title", "subtitle")),
        cells_column_labels()
      )
    ) |>
    tab_style(
      style = cell_fill(color = "lightblue"),
      locations = cells_body(
        columns = p.value,
        rows = term == "Mean Annual Temperature"
      )
    )
}
