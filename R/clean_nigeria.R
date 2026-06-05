#' Clean the Nigeria plastic pollution dataset
#'
#' @return A tibble with neighborhood, plastic type, and count cleaned.
#'
#' @importFrom dplyr recode mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract str_squish str_remove str_to_title str_replace_all
#'
#' @export
clean_nigeria <- function() {
  load_nigeria_data() |>
    mutate(
      Neighborhood = stringr::str_extract(Location, "^[^,]+"),
      Neighborhood = stringr::str_squish(Neighborhood)
    ) |>
    pivot_longer(
      cols = c(starts_with("SUM_"), Fishing_Net),
      names_to = "plastic_type",
      values_to = "Count"
    ) |>
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
    )
}
