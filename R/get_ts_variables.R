#' Get a vector of timeseries variable names
#' 
#' get_ts_variables() returns a list of all possible timeseries variables, while
#' get_ts_variables(site) returns the data available to a specific site.
#' 
#' @param site a valid mda.streams site (see \link{get_sites}) or NULL for all 
#'   sites
#' @param ... additional arguments passed to \code{\link[sbtools]{session_check_reauth}}, 
#'   for example \code{username}
#'
#' @return an alphabetically sorted character vector of unique timeseries 
#'   variable names for given sites
#' @examples
#' get_ts_variables() #list all timeseries variable names
#' \dontrun{
#' get_ts_variables(site = 'nwis_01018035')
#' }
#' @import sbtools stringr
#' @export
get_ts_variables = function(site = NULL, ...){
  
  ts_pattern = pkg.env$ts_prefix
  
  ts_variables <- NULL
  
  session_check_reauth(...)
  
  if (is.null(site)){
    ts_variables <- ts_variables_superset()
  } else {
    site_items <- query_item_identifier(scheme = get_scheme(), key = site, limit = 10000)
    
    if (nrow(site_items) == 0){ 
      stop(site, ' does not exist')
    }
    var_names <- site_items$title
    is_ts <- str_detect(var_names, pattern = ts_pattern)
    
    for (i in which(is_ts)){
      ts_variables = c(ts_variables, str_split_fixed(string = var_names[i], pattern = ts_pattern, n = 2)[2])
    }
    ts_variables = unique(ts_variables)
    
    if (any(!ts_variables %in% ts_variables_superset())){
      warning('some variables from ', site, ' not found in all-site catalog. ')
    }
  }
  
  ts_variables <- sort(ts_variables)
  return(ts_variables)
}

ts_variables_superset <- function(){
  ts_variables <- rownames(get_var_codes())
  return(ts_variables)
}