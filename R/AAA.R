#' Define a package environment for storing data specific to a project during an
#' R session
#' 
#' \code{scheme} - string identifying the ScienceBase scheme where this SB project
#'   resides, accessed by \code{\link{set_scheme}} and \code{\link{get_scheme}}.
#' @return the package environment
define_pkg_env <- function() {
  pkg.env <- new.env()
  pkg.env$scheme <- "mda_streams"
  pkg.env$ts_prefix <- "ts_"
  pkg.env$ts_extension <- "tsv"
  pkg.env$ts_delim <- "\t"
  return(pkg.env)
}
pkg.env <- define_pkg_env()
