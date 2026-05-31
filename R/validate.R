# Internal helper functions -------------------------------------------------

#' Validate Nigeria neighborhood name
#'
#' Internal helper that checks whether Nigerian neighborhood name is valid
#'
#' @param neighborhood A character string of a neighborhood in Nigeria
#'
#' @return Returns `TRUE` when the input is valid
validate_neighborhood <- function(neighborhood) {
  valid_neighborhoods <- c("Agege", "Amuwo Odofin", "Apapa", "Eti Osa",
                           "Ibeju Lekki", "Port-Harcourt", "Surulere")

  if (!is.character(neighborhood) || length(neighborhood) != 1) {
    stop("`neighborhood` must be one character string.")
  }

  if (!neighborhood %in% valid_neighborhoods) {
    stop("`neighborhood` must be a neighborhood in the Nigeria data.")
  }

  TRUE

}
