#' verify timeseries data.frame format and contents
#'
#' Checks to see a given data.frame meets project criteria
#' 
#' @param data a \code{\link[unitted]{unitted}} data.frame with date and value
#' @param variable optional name for timeseries. If supplied, will check variable column name
#' @param checks tests to run on the data ('cols', 'tz', 'units', 'variable')
#' @return TRUE if is valid, FALSE if not
#' 
#' @examples 
#' files <- stage_nwis_ts(sites = c("nwis_06893820","nwis_01484680"), variable = "doobs",
#'                  times = c('2014-01-01','2014-02-01'))
#' ts <- read_ts(files[1]) 
#' verify_ts(ts)     
#' verify_ts(ts, variable = 'doobs', checks = c('cols', 'units', 'variable'))            
#' 
#' @keywords internal
#' @export
verify_ts <- function(data, variable, checks = c('cols', 'tz', 'units', 'variable')){
  
  tests <- list('cols' = function(x,...) ncol(x) == 2,
                'tz' = function(x,...) get_units(x[,1]) == 'UTC',
                'units' = function(x,v,...) get_units(x[,2]) == get_var_codes(v, 'units'),
                'variable' = function(x,v,...) names(x)[2] == v)
  
  
  for (check in checks){
    if (!tests[[check]](x = data, v = variable))
      return(FALSE)
  }
  return(TRUE)
  
}