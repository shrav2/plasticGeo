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
  neighborhoods <- load_nigeria_data() |>
    mutate(
      Neighborhood = stringr::str_extract(Location, "^[^,]+"),
      Neighborhood = stringr::str_squish(Neighborhood)
    ) |>
    distinct(Neighborhood) |>
    pull(Neighborhood)

  validate_neighborhood(neighborhood)

  if (!is.numeric(n) || length(n) != 1 || n %% 1 != 0 || n < 1 || n > 10) {
    stop("`n` must be a whole number between 1 and 10.")
  }

  load_nigeria_data() |>
    mutate(
      Neighborhood = stringr::str_extract(Location, "^[^,]+"),
      Neighborhood = stringr::str_squish(Neighborhood)
    ) |>
    filter(Neighborhood == neighborhood) |>
    select(
      starts_with("SUM_"),
      Fishing_Net
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "plastic_type",
      values_to = "Count"
    ) |>
    group_by(plastic_type) |>
    summarise(
      Count = sum(Count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(Count)) |>
    slice_head(n = n) |>
    mutate(
      plastic_type = stringr::str_remove(plastic_type, "SUM_"),
      plastic_type = dplyr::recode(
        plastic_type,
        "Soft_WrapperOrLabel" = "Wrappers & Labels",
        "Soft_OtherPlastic" = "Other Soft Plastics",
        "Soft_Bag" = "Plastic Bags",
        "Hard_PlasticBeverageBottle" = "Plastic Bottles",
        "HardOrSoft_PlasticBottleCap" = "Bottle Caps",
        "PlasticOrFoamFoodContainer" = "Food Containers",
        "PlasticOrFoamPlatesBowlsCup" = "Plates, Bowls & Cups",
        "OtherPlasticDebris" = "Plastic Debris",
        "Foam_OtherPlasticDebris" = "Foam Debris",
        "Hard_OtherPlasticBottle" = "Other Plastic Bottles",
        "Hard_BucketOrCrate" = "Buckets & Crates",
        "Soft_Straw" = "Straws",
        "Hard_Lighter" = "Lighters",
        "Fishing_Net" = "Fishing Nets",
        .default = stringr::str_to_title(
          stringr::str_replace_all(plastic_type, "_", " ")
        )
      )
    ) |>
    select(
      `Plastic Type` = plastic_type,
      Count
    )
}

