#' Get a vector of timeseries variable names
#' 
#' list_datasets() returns a list of all possible timeseries variables, while
#' list_datasets(site) returns the data available to a specific site.
#' 
#' @param site a valid mda.streams site (see \link{get_sites}) or NULL for all 
#'   sites
#' @param type character. one or more dataset types to return
#' @param ... additional arguments passed to \code{\link[sbtools]{session_check_reauth}}, 
#'   for example \code{username}
#'
#' @return an alphabetically sorted character vector of unique timeseries 
#'   variable names for given sites
#' @examples
#' 
#' \dontrun{
#' list_datasets() #list all timeseries variable names
#' list_datasets(site = 'nwis_01018035')
#' }
#' @import sbtools stringr
#' @export
list_datasets = function(site, type=c("ts","watershed"), ...){
  
  ts_pattern = pkg.env$ts_prefix
  
  if (missing(site)){
    stop("looking for a list of possible datasets? try get_var_codes.")
  } else {
    session_check_reauth(...)
    site_items <- query_item_identifier(scheme = get_scheme(), key = site, limit = 10000)
    
    if (nrow(site_items) == 0){ 
      stop(site, ' does not exist')
    }
    var_names <- site_items$title
    is_ts <- str_detect(var_names, pattern = ts_pattern)
    
    ts_variables <- NULL
    for (i in which(is_ts)){
      ts_variables = c(ts_variables, str_split_fixed(string = var_names[i], pattern = ts_pattern, n = 2)[2])
    }
    ts_variables = unique(ts_variables)
    
    if (any(!ts_variables %in% get_var_codes(out="shortname"))){
      warning('some variables from ', site, ' not found in all-site catalog. ')
    }
  }
  
  ts_variables <- sort(ts_variables)
  return(ts_variables)
}
