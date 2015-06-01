#' Get the scheme name for this project
#' 
#' Gives a warning and returns NULL if scheme has not been set
#' 
#' @param warn logical specifying whether a warning should be given if the
#'   scheme is not set
#' @return character name of the scheme
#' @export
get_scheme <- function(warn=TRUE) {
  if(exists("scheme", envir=pkg.env)) {
    pkg.env$scheme
  } else {
    if(warn) warning("scheme has not been set")
    NULL
  }
}