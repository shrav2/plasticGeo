#' Get the top n plastic types & counts for each type in the neighborhood specified in Nigeria.
#'
#' @param neighborhood A character string of a neighborhood in Nigeria in the data.
#' @param n A numeric whole value less than or equal to 10
#'
#' @import stringr
#' @import dplyr
#' @import tidyr
#'
#' @return A tibble of top plastic types with counts.
#'
#' @export
top_plastic_items <- function(neighborhood, n){

  validate_neighborhood(neighborhood)

  if (!is.numeric(n) || length(n) != 1 || n %% 1 != 0 || n < 1 || n > 10) {
    stop("`n` must be a whole number between 1 and 10.")
  }

  clean_nigeria() |>
    filter(Neighborhood == neighborhood) |>
    group_by(plastic_type) |>
    summarise(
      Count = sum(Count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(Count)) |>
    slice_head(n = n) |>
    select(
      `Plastic Type` = plastic_type,
      Count
    )
}

