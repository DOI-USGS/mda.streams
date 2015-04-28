#'@title get WFS from site
#'@description get a WFS url from a site that has a 'watershed'
#'
#'@param site a site identifier
#'@param session arguments passed to sbtools \code{query_item_identifier} for authentication. 
#'Not needed if parent of sites is public 
#'@return a character string for WFS url or NULL if watershed doesn't exist for site, or site doesn't exist.
#'@seealso \code{\link{get_watershed_WMS}}
#'@examples
#'site <- "nwis_04165500"
#'get_watershed_WFS(site)
#'
#'\dontrun{
#'# will fail
#'get_watershed_WFS('fake_site'))
#'}
#'
#'@import sbtools 
#'@export
get_watershed_WFS = function(site, session = NULL){
  
  watershed_item <- get_watershed_item(site, session)
 
  WFS_url <- match_url_distro(watershed_item, "ScienceBase WFS Service")
  return(WFS_url)
}

get_watershed_item = function(site, session){
  # session not needed since items are public
  identifier <- query_item_identifier(scheme = 'mda_streams', type = 'watershed', key = site,  session = session)
  
  watershed_id <- identifier$id
  
  watershed_item <- item_get(id = watershed_id, session = session)
  
  return(watershed_item)
}
match_url_distro = function(item, title_to_match){
  
  num_links <- length(item[["distributionLinks"]])
  
  url = NULL
  for (i in seq_len(num_links)){
    if (item[["distributionLinks"]][[i]][['title']] == title_to_match){
      url <- item[["distributionLinks"]][[i]][['uri']]
      break
    }
  }
  
  return(url)
  
}