#'@title get a vector of timeseries variable name from a site or sites
#'@param site a valid mda.streams site (see \link{get_sites}) or NULL for all sites
#'@param session a valid sciencebase session (see \code{\link[sbtools]{authenticate_sb}}). 
#'Set \code{session = NULL} (default) for sites on sciencebase that are public.
#'@return an alphabetically sorted character vector of unique timeseries variable names for given sites
#'@examples
#'get_ts_variables() #list all timeseries variable names
#'\dontrun{
#'get_ts_variables(site = 'nwis_01018035')
#'}
#'@import sbtools stringr
#'@export
get_ts_variables = function(site = NULL, session = NULL){
  
  ts_pattern = 'ts_'
  
  ts_variables <- NULL
  
  
  if (is.null(site)){
    ts_variables <- ts_variables_superset()
  } else {
    site_items <- query_item_identifier(scheme = 'mda_streams', key = site, session, limit = 10000)
    
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
  ts_variables <- c('doobs','wtr','disch','stage') # put this in sysdata.rda?
  return(ts_variables)
}