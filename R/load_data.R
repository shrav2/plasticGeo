#' Load Plastic Waste Dataset
#'
#' Loads a cleaned plastic waste dataset containing plastic types, collection
#' counts, and mean annual temperatures for different countries used to analyze
#' relationships between plastic collection totals and climate conditions.
#'
#' @return A tibble containing plastic waste collection data from the Break Free
#' From Plastic movement.
#'
#' @importFrom readr read_csv
#'
#' @export

load_plastic_data <- function() {
  path <- system.file(
    "extdata",
    "plastic.parquet",
    package = "plasticGeo"
  )

  arrow::read_parquet(path)
}

#' Load Nigeria Dataset
#'
#' Loads beach cleanup data collected in Nigeria through the Earth Challenge
#' 2020 initiative, including neighborhood information and plastic collection
#' counts for different types of plastic.
#'
#' @return A tibble containing plastic waste collection data from the Break Free
#' From Plastic movement.
#'
#' @importFrom readr read_csv
#'
#' @export

load_nigeria_data <- function() {
  file_path <- system.file(
    "extdata",
    "Nigeria Data.csv",
    package = "plasticGeo"
  )

  read_csv(file_path, show_col_types = FALSE)
}
