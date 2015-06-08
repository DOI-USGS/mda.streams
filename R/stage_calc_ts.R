#' @title stage calculated or modeled data into a time series file
#' @description accept calculated/modeled data as a data.frame and return a file handle
#' 
#' @param sites a character vector of valid NWIS site IDs
#' @param var short name of variable as in \code{get_var_codes(out='var')}
#' @param folder a folder to place the file outputs in (defaults to temp directory)
#' @param verbose provide verbose output (currently not implemented)
#' @param ... additional arguments passed to \code{\link[geoknife]{geoknife}} and \code{\link[unitted]{write_unitted}}
#' @return a file handle for time series file created 
#' @importFrom unitted u write_unitted
#' 
#' @examples
#' \dontrun{
#' files <- stage_calc_ts(sites = c("nwis_06893820","nwis_01484680"), variable = "baro", 
#'                  times = c('2014-01-01 00:00','2014-01-01 05:00'))
#' }
#' @export
stage_calc_ts <- function(sites, var, folder = tempdir(), verbose = FALSE, ...){
  warning("function under construction")
  NULL
}