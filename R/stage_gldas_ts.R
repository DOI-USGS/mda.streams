#' @title stage gldas data into a time series file
#' @description get data from nldas and return a file handle
#'   
#' @param sites a character vector of valid NWIS site IDs
#' @param var short name of var as in
#'   \code{unique(get_var_src_codes(out='var'))}
#' @param times a length 2 vector of POSIXct dates
#' @param folder a folder to place the file outputs in (defaults to temp 
#'   directory)
#' @param version character string indicating whether you want to stage the 
#'   \code{ts} as a .tsv or .rds
#' @param verbose logical. provide verbose output?
#' @param url for web dataset. If missing, uses geoknife's "gldas" url
#' @param ... additional arguments passed to \code{\link[geoknife]{geoknife}}
#' @return a file handle for time series file created
#' @importFrom geoknife webdata
#'   
#' @examples
#' \dontrun{
#' files <- stage_gldas_ts(sites = c("nwis_06893820","nwis_01484680"), var = "baro", 
#'   times = c('2014-01-01 00:00','2014-01-01 05:00'), verbose = TRUE)
#' read_ts(files[1])
#' }
#' @export
stage_gldas_ts <- function(sites, var, times, folder = tempdir(), version=c('rds','tsv'), verbose = FALSE, url, ...){
  version <- match.arg(version)
  src <- 'nldas'
  if(length(var) > 1) stop("one var at a time, please")
  verify_var_src(var, 'gldas', on_fail=warning)
  vars <- var # need a renamed version for get_var_src_codes filter on var
  p_code <- '.dplyr_var'
  p_code <- get_var_src_codes(src=='gldas',var%in%vars,!is.na(p_code),out="p_code")
  expected_units <- get_var_src_codes(var==vars, src=='gldas', out='units')
  if (missing(url)){
    url <- url(webdata('gldas'))
  }
  stage_ldas_ts(sites, var, p_code, times, folder, version, verbose, url, expected_units, ...)
}