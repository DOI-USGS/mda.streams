#' Get a vector of timeseries dataset names
#' 
#' list_datasets(site) returns the data available to a specific site. In
#' contrast, get_var_codes() returns a list of all possible variables and
#' get_var_codes(type="ts") returns all possible timeseries variables.
#' 
#' @param site_names a list of site names such as those returned from 
#'   make_site_name()
#' @param type character. one or more dataset types to return
#' @param ... additional arguments passed to
#'   \code{\link[sbtools]{session_check_reauth}}, for example \code{username}
#'   
#' @return an alphabetically sorted character vector of unique timeseries 
#'   variable names for given sites
#' @examples
#' 
#' \dontrun{
#' list_datasets(site_name = 'nwis_295826095082200')
#' }
#' @import sbtools stringr
#' @export
list_datasets = function(site_name, type=c("ts","watershed"), ...){
  
  ts_pattern = pkg.env$ts_prefix
  
  if (missing(site_name)){
    stop("site_name required. looking for a list of possible dataset variables? try ?get_var_codes.")
  } else {
    session_check_reauth(...)
    site_items <- query_item_identifier(scheme = get_scheme(), key = site_name, limit = 10000)
    
    if (nrow(site_items) == 0){ 
      stop(site_name, ' does not exist')
    }
    var_names <- site_items$title
    is_ts <- str_detect(var_names, pattern = ts_pattern)
    
    ts_variables <- NULL
    for (i in which(is_ts)){
      ts_variables = c(ts_variables, str_split_fixed(string = var_names[i], pattern = ts_pattern, n = 2)[2])
    }
    ts_variables = unique(ts_variables)
    
    if (any(!ts_variables %in% get_var_codes(out="var_src"))){
      warning('some variables from ', site_name, ' not found in all-site catalog. ')
    }
  }
  
  ts_variables <- sort(ts_variables)
  return(ts_variables)
}
