#' A helper to find_sites. Gets site IDS from sciencebase w/ simple data query
#' 
#' Collects from sciencebase all site names ("site_root" titles, e.g., 
#' "nwis_02948375") or the names of sites containing a particular var_src 
#' dataset, and returns as a character vector of those site names
#' 
#' @param with_var_src limit sites to those with children matching the specified
#'   var_src key
#' @param limit numeric. Max number of sites to return. Default 10000
#' @param ... Additional parameters supplied to 
#'   \code{\link[sbtools]{session_check_reauth}}
#' @return a character vector of "site_root" titles (keys)
#' @import sbtools
#' @examples
#' \dontrun{
#' get_sites()
#' get_sites(limit = 10)
#' # get those sites that have water temperature
#' get_sites(with_var_src = 'ts_wtr')
#' }
#' @import jsonlite
#' @import httr
#' @import sbtools
#' @export
get_sites <- function(with_var_src = NULL, limit = 10000, ...){
  
  # log in to ScienceBase
  session_check_reauth(...)

  if (is.null(with_var_src)){
    # get the superset of sites. this query is used in both if{} blocks but with
    # different limits.
    sites <- query_item_identifier(scheme=get_scheme(), type='site_root', limit=limit)$title
  } else {
    
    # find all the time series items for the specified var_src
    if(length(with_var_src) != 1) stop("with_var_src must have length 1")
    # create the query
    filter_items = list('scheme'=get_scheme(), 'type'=with_var_src)
    filter = paste0('itemIdentifier=', toJSON(filter_items, auto_unbox=TRUE))
    # run the query & pull out the content (timeseries item IDs)
    query = list('filter' = filter, 'max' = limit, 'format' = 'json', 'fields' = 'parentId')
    child_ids <- query_items(query) 
    response <- content(child_ids, 'parsed')
    
    # convert from parents of these items to site names
    parents <- sapply(response$items, function(item) item$parentId ) # get the parents (site items) of the timeseries items
    parents <- parents[seq_len(limit)] # limit the number of responses
    identifiers <- query_item_identifier(scheme=get_scheme(), type='site_root', limit=10000) #DON'T limit the number of id->sitename translation entries
    sites <- identifiers$title[match(parents, identifiers$id)] # translate IDs to site names

    # this code would be slower because it involves many SB queries:
    # sites <- sapply(response$items, function(item) item_get(item$parentId)$title )
  }

  return(sites)
}