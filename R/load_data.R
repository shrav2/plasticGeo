#' Load Plastic Waste Dataset
#'
#' Downloads the environmental plastic waste dataset
#' used for temperature and cleanup analysis from the Break Free From Plastic movement.
#'
#' @return A tibble containing plastic waste collection data from the Break Free From Plastic movement.
#'
#' @importFrom readr read_csv
#'
#' @export
load_plastic_data <- function(){
   plastic_data <- readr::read_csv("https://raw.githubusercontent.com/shrav2/Environmental-Plastic-Waste-Analysis/main/stat431-meta.csv")
}

#' Load Nigeria Dataset
#'
#' Downloads outside data from the Earth Challenge 2020 for beach-cleanups in Nigeria.
#'
#' @return A tibble containing plastic waste collection data from the Break Free From Plastic movement.
#'
#' @importFrom readr read_csv
#'
#' @export
load_nigeria_data <- function(){
   nigeria_data <- readr::read_csv("https://raw.githubusercontent.com/shrav2/Environmental-Plastic-Waste-Analysis/main/Nigeria%20Data.csv")

}
