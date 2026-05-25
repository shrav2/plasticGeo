#' Load Plastic Waste Dataset & Nigeria Dataset
#'
#' Downloads the environmental plastic waste dataset
#' used for temperature and cleanup analysis from the Break Free From Plastic movement.
#' Also downloads outside data from the Earth Challenge 2020 for beach-cleanups in Nigeria.
#'
#' @return A tibble containing plastic waste and
#' temperature data & Nigeria beach-cleanup data
#'
#' @importFrom readr read_csv
#'
#' @export
load_data <- function(){
  readr::read_csv('https://raw.githubusercontent.com/shrav2/Environmental-Plastic-Waste-Analysis/refs/heads/main/stat431-meta.csv')
  readr::read_csv('https://raw.githubusercontent.com/shrav2/Environmental-Plastic-Waste-Analysis/refs/heads/main/Nigeria%20Data.csv')

}
