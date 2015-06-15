#' Get a vector of timeseries dataset names
#' 
#' list_datasets(site) returns the data available to a specific site. In
#' contrast, get_var_codes() returns a list of all possible variables and
#' get_var_codes(type="ts") returns all possible timeseries variables.
#' 
#' @param site_name a character vector of length one with a site name such 
#'   as those returned from make_site_name()
#' @param type character. one or more dataset types to return
#' @param ... additional arguments passed to
#'   \code{\link[sbtools]{query_item_identifier}}, for example \code{limit}
#'   
#' @return an alphabetically sorted character vector of unique timeseries 
#'   variable names for given sites
#' @examples
#' \dontrun{
#' list_datasets(site_name = 'nwis_01021050')
#' }
#' @import sbtools stringr
#' @export
list_datasets = function(site_name, type=c("ts","watershed"), ...){
  
  type <- match.arg(type, several.ok = TRUE)
  str_match_patterns <- c('ts' = pkg.env$ts_prefix, 'watershed' = 'watershed')[type] %>%
    as.character()
  
  if (missing(site_name)){
    stop("site_name required. looking for a list of possible dataset variables? try ?get_var_codes.")
  } else {
    site_items <- query_item_identifier(scheme = get_scheme(), key = site_name, limit = 10000)
    
    if (nrow(site_items) == 0){ 
      stop(site_name, ' does not exist')
    }
    var_names <- site_items$title
    is_dataset <- sapply(str_match_patterns, function (x) str_detect(var_names, pattern = x)) %>%
      rowSums() > 0
    
    
   datasets <- site_items$title[is_dataset] %>%
     unique() %>%
     sort()
   return(datasets)
  
  }
}
