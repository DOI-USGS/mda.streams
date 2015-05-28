#' Set the scheme name of the project
#' 
#' @param scheme character name of the ScienceBase project
#' @export
set_scheme <- function(scheme) {
  pkg.env$scheme <- scheme
}