#'@title Create new site in SB
#'
#'
#'
#'@param site Local site ID (e.g. NWIS_09238475)
#'
#'
#'
#'
#'
#'@import sbtools
#'@export
create_site <- function(site, session){
  
  #check that it doesn't already exits
  item = query_item_identifier(scheme='mda_streams',type="site_root", 
                               key=site, session=session)
  
  if(nrow(item) > 0){
    stop('This site already exists')
  }	
  
  #create site in SB 5487139fe4b02acb4f0c8110
  ts_item = item_create(parent_id="5487139fe4b02acb4f0c8110", 
                        title=site, session=session)
  
  #tag item with our special identifier
  item_update_identifier(ts_item, scheme="mda_stream", type="site_root",
                         key=site, session=session)
  
  
  return(ts_item)  
  
}