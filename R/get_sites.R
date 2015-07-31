#' A helper to list_sites. Gets site IDS from sciencebase w/ simple data query
#' 
#' Collects from sciencebase all site names ("site_root" titles, e.g., 
#' "nwis_02948375") or the names of sites containing a particular var_src 
#' dataset, and returns as a character vector of those site names
#' 
#' @param with_dataset_name limit sites to those with children matching the
#'   specified ts or other dataset name (e.g., "ts_doobs_nwis")
#' @param limit numeric. Max number of sites to return
#' @param ... Additional parameters supplied to 
#'   \code{\link[sbtools]{session_check_reauth}}
#' @return a character vector of "site_root" titles (keys)
#' @import sbtools
#' @keywords internal
#' @examples
#' \dontrun{
#' get_sites()
#' get_sites(limit = 10)
#' # get those sites that have water temperature
#' get_sites(with_dataset_name = 'ts_wtr_nwis')
#' }
#' @import jsonlite
#' @import httr
#' @import sbtools
#' @export
get_sites <- function(with_dataset_name = NULL, limit = 10000, ...){

  if (is.null(with_dataset_name)){
    # get the superset of sites. this query is used in both if{} blocks but with
    # different limits.
    sites <- query_item_identifier(scheme=get_scheme(), type='site_root', limit=limit)$title
  } else {
    
    # find all the time series items for the specified var_src
    if(length(with_dataset_name) != 1) stop("with_dataset_name must have length 1")
    # create the query
    filter_items = list('scheme'=get_scheme(), 'type'=with_dataset_name)
    filter = paste0('itemIdentifier=', toJSON(filter_items, auto_unbox=TRUE))
    # run the query & pull out the content (timeseries item IDs)
    query = list('filter' = filter, 'max' = limit, 'format' = 'json', 'fields' = 'parentId')
    child_ids <- query_items(query) 
    response <- content(child_ids, 'parsed')
    
    # convert from parents of these items to site names
    parents <- sapply(response$items, function(item) item$parentId ) # get the parents (site items) of the timeseries items
    if (length(parents)==0){
      return( vector('character'))
    }
      
    # -- ignore specified limiter on query: 
    
    identifiers <- query_item_identifier(scheme=get_scheme(), type='site_root', limit=10000)
    sites <- identifiers$title[match(parents, identifiers$id)] # translate IDs to site names

    # this code would be slower because it involves many SB queries:
    # sites <- sapply(response$items, function(item) item_get(item$parentId)$title )
  }

  return(sites)
}