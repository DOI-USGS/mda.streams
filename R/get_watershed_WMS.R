#'@title get WMS from site
#'@description get a WMS url from a site that has a 'watershed'
#'
#'@param site a site identifier
#'@param session arguments passed to sbtools \code{query_item_identifier} for authentication. 
#'Not needed if parent of sites is public 
#'@return a character string for WMS url or NULL if watershed doesn't exist for site, or site doesn't exist.
#'@seealso \code{\link{get_watershed_WFS}}
#'@examples
#'site <- "nwis_04165500"
#'get_watershed_WMS(site)
#'
#'\dontrun{
#'# will fail
#'get_watershed_WMS('fake_site'))
#'}
#'
#'@import sbtools
#'@export
get_watershed_WMS = function(site, session = NULL){
  
  watershed_item <- get_watershed_item(site, session)
  
  WMS_url <- match_url_distro(watershed_item, "ScienceBase WMS Service")
  return(WMS_url)
}

