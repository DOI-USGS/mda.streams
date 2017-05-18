#' verify timeseries data.frame format and contents
#' 
#' Checks to see a given data.frame meets project criteria
#' 
#' @param data a \code{\link[unitted]{unitted}} data.frame with date and value
#' @param var optional name for timeseries. If supplied, will check variable
#'   column name
#' @param checks tests to run on the data ('cols', 'tz', 'units', 'variable')
#' @param on_fail function to call when a test fails. good options are warning 
#'   and stop. if the function does not halt evaluation, FALSE is returned after
#'   the function is first called.
#' @return TRUE if is valid, FALSE if not
#' @importFrom unitted is.unitted get_units deunitted u v
#' @importClassesFrom unitted unitted unitted_data.frame
#' @examples 
#' \dontrun{
#' files <- stage_nwis_ts(sites = c("nwis_06893820","nwis_01484680"), var = "doobs",
#'                  times = c('2014-01-01','2014-02-01'))
#' files <- download_ts("doobs_nwis", "nwis_01073389", on_local_exists="skip")
#' ts <- read_ts(files[1]) 
#' verify_ts(ts, var='doobs', checks=c('ncol','units','names')) # TRUE
#' verify_ts(ts, var='wtr', checks=c('ncol','units','names')) # warning + FALSE
#' }
#' @export
verify_ts <- function(
  data, var, checks=c('ncol','nrow','unitted','tz','units','names','NA_values','NA_dates','timesteps'), on_fail=warning){
  
  tests <- list(
    'ncol' = function(x,v) {
      if(v %in% c("gpp","er","K600"))
        ncol(x) %in% c(2,4)
      else
        ncol(x) == 2
    },
    'nrow' = function(x,...) {
      nrow(x) > 0
    },
    'unitted' = function(x,...) {
      is.unitted(x)
    },
    'tz' = function(x,...) {
      ((get_units(x[[1]]) == '') && attr(x[[1]],'tzone') == 'UTC') || 
        (is.na(x[[1,1]]) && nrow(x) == 1) # allow for 1-row const tses
    },
    'units' = function(x,v) {
      (!is.na(get_units(x[[2]]))) && 
        (get_units(x[[2]]) == get_units(u(1, get_var_src_codes(var==v, out='units')[1]))) &&
        (if(ncol(x)==4) length(unique(get_units(x[,2:4]))) == 1 else TRUE)
    },
    'names' = function(x,v) {
      names(x)[2] == v
    },
    'NA_values' = function(x,v) {
      if(v %in% c("gpp","er","K600"))
        TRUE # metabolism predictions can be whatever they want; NAs are informative
      else
        all(!is.na(v(x[,2:ncol(x)]))) # values of input data should not be NA; missing is better
    },
    'NA_dates' = function(x,v) {
      all(!is.na(v(x[[1]]))) || # in general, DateTimes should not be NA
        (is.na(x[[1,1]]) && nrow(x) == 1) # allow for 1-row const tses
    },
    'timesteps' = function(x,v) {
      all(as.numeric(diff(v(x[[1]])), units='mins') > 0) # require positive timesteps
    })
  
  if(class(data) == 'unitted_data.frame' && !is.data.frame(data)) {
    warning("unitted_data.frame got loaded imperfectly. This happens when mda.streams is called from the command line using Rscript; consider R CMD BATCH instead.")
    data <- deunitted(data)
    checks <- checks[!(checks %in% c('unitted','tz','units'))]
  }
  failures <- c()
  for (check in checks){
    if (!tests[[check]](x = data, v = var)) {
      failures <- c(failures, check)
    }
  }
  if(length(failures) > 0) {
    on_fail("verify_ts for ", var, " failed on test",if(length(failures)>1) "s" else "",
            " for ", paste0(failures, collapse=", "), call.=FALSE)
    return(FALSE)
  } else {
    return(TRUE)
  }
  
}
