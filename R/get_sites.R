#'@title get site IDS from sciencebase
#'@description collects all "site_root" titles from sciencebase, returns as a character vector
#'
#'@param with_child_key limit sites to those with children matching the specified key
#'@param ... Additional parameters supplied to \code{\link[sbtools]{session_check_reauth}}
#'@param limit numeric. Max number of sites to return. Default 10000
#'@return a character vector of "site_root" titles (keys)
#'@import sbtools
#'@examples
#'\donttest{
#'get_sites()
#'}
#'\dontrun{
#'# get sites if they are private
#'
#'get_sites(limit = 10)
#'# get sites with water temperature
#'get_sites(with_child_key = 'ts_wtr')
#'}
#'@import jsonlite 
#'@import httr
#'@import sbtools
#'@export
get_sites <- function(with_child_key = NULL, limit = 10000, ...){
  
  scheme <- get_scheme()
  type <- 'site_root'
  
  session_check_reauth(...)
  session = current_session()

  #get the superset of sites, which is used for both options
  identifiers <- query_item_identifier(scheme = scheme, type = type, session = session, limit = limit)
  
  if (is.null(with_child_key)){
    sites <- identifiers$title
  } else {
    
    filter_items = list('scheme'=scheme, 'type'= with_child_key)
    filter = paste0('itemIdentifier=', toJSON(filter_items, auto_unbox=TRUE))
    
    query = list('filter' = filter, 'max' = limit, 'format' = 'json', 'fields' = 'parentId')
    child_ids <- query_items(query, session) 
    response <- content(child_ids, 'parsed')

    sites = vector(mode = 'character', length = length(response$items))
    for(i in seq_len(length(response$items))){
      parent_id <- response$items[[i]]$parentId
      # match to the superset
      sites[i] <- identifiers$title[identifiers$id == parent_id]
    }
  }

  return(sites)
}