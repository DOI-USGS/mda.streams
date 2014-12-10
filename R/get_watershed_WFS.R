#'@title get WFS from site
#'@description get a WFS url from a site that has a 'watershed'
#'
#'@param site a site identifier
#'@param session arguments passed to sbtools \code{query_item_identifier} for authentication. 
#'Not needed if parent of sites is public 
#'@return a character string for WFS url or NULL if watershed doesn't exist for site, or site doesn't exist.
#'
#'@examples
#'site <- "nwis_01018035"
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
  
  # session not needed since items are public
  identifier <- query_item_identifier(scheme = 'mda_streams', type = 'watershed', key = site,  session = session)
  
  watershed_id <- identifier$id
  
  watershed_item <- item_get(id = watershed_id, session = session)
  watershed_item[["distributionLinks"]]
  
  num_links <- length(watershed_item[["distributionLinks"]])
  
  WFS_url = NULL
  for (i in seq_len(num_links)){
    if (watershed_item[["distributionLinks"]][[i]][['title']] == "ScienceBase WFS Service"){
      WFS_url <- watershed_item[["distributionLinks"]][[i]][['uri']]
      break
    }
  }
  
  return(WFS_url)
}