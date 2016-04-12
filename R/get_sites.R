#' A helper to list_sites. Gets site IDS from sciencebase w/ simple data query
#' 
#' Collects from sciencebase all site names ("site_root" titles, e.g., 
#' "nwis_02948375") or the names of sites containing a particular var_src 
#' dataset, and returns as a character vector of those site names
#' 
#' @param with_dataset_name limit sites to those with children matching the
#'   specified ts or other dataset name (e.g., "ts_doobs_nwis")
#' @param limit numeric. Max number of sites to return
#' @return a character vector of "site_root" titles (keys)
#' @import sbtools
#' @keywords internal
#' @examples
#' \dontrun{
#' mda.streams:::get_sites()
#' mda.streams:::get_sites(limit = 10)
#' # get those sites that have water temperature
#' mda.streams:::get_sites(with_dataset_name = 'ts_wtr_nwis')
#' }
#' @import jsonlite
#' @import httr
#' @import sbtools
get_sites <- function(with_dataset_name=NULL, limit=10000){

  if (is.null(with_dataset_name)){
    # get the superset of sites. this query is used in both if{} blocks but with
    # different limits.
    site_items <- query_item_identifier(scheme=get_scheme(), type='site_root', limit=limit)
    sites <- sapply(site_items, function(item) item$title)
  } else {

    # find all the time series items for the specified var_src
    if(length(with_dataset_name) != 1) stop("with_dataset_name must have length 1")
    ts_items <- query_item_identifier(scheme=get_scheme(), type=with_dataset_name, limit=limit)
    
    # convert from parents of these items to site names
    site_ids <- sapply(ts_items, function(ts) ts$parentId)
    if(length(site_ids)==0) {
      sites <- vector('character')
    } else {
      # get all sites IDs and titles, then filter to sites whose IDs match ours.
      # override limit arg because this is a superset of the final output
      all_site_items <- query_item_identifier(scheme=get_scheme(), type='site_root', limit=10000)
      all_site_info <- bind_rows(lapply(all_site_items, function(site) as_data_frame(site[c('title','id')])))
      sites <- all_site_info$title[match(site_ids, all_site_info$id)] # translate IDs to site names
      # this code would be slower because it involves many SB queries:
      # sites <- sapply(site_ids, function(id) as.sbitem(id)$title)
    }
  }

  return(sites)
}