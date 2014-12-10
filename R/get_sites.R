#'@title get site IDS from sciencebase
#'@description collects all "site_root" titles from sciencebase, returns as a character vector
#'
#'@param session arguments passed to sbtools \code{query_item_identifier} for authentication. 
#'Not needed if parent of sites is public
#'@param limit numeric. Max number of sites to return. Default 10000
#'@return a character vector of "site_root" titles (keys)
#'@import sbtools
#'@examples
#'\donttest{
#'get_sites()
#'}
#'\dontrun{
#'# get sites if they are private
#'session <- authenticate_sb(username, password)
#'get_sites(session = session, limit = 10)
#'}
#'@export
get_sites <- function(session = NULL, limit = 10000){
  
  # session not needed since items are public
  identifiers <- query_item_identifier(scheme = 'mda_streams', type = 'site_root', session = session, limit = limit)
  
  sites <- identifiers$title
  return(sites)
}