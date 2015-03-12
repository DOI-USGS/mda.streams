#'@title Create new site in SB
#'
#'
#'
#'@param site Local site ID (e.g. nwis_09238475)
#'@param session Session object from \code{\link[sbtools]{authenticate_sb}}
#'@param skip_exists boolean for skip creation if item already exists. 
#'if TRUE, site will not be created and an empty list will be returned
#'@return and item list
#'@author Corinna Gries
#'
#'
#'@import sbtools
#'@export
create_site <- function(site, session, replace_existing = FALSE){
  
  #check that it doesn't already exits
  item = query_item_identifier(scheme='mda_streams',type="site_root", 
                               key=site, session=session)
  
  if(nrow(item) > 0){
    if (replace_existing){
      item_rm(id, session)
    } else {
      return(list())
    }
  }	
  
  project_id <- get_project_id()
  item = item_create(parent_id = project_id, 
                        title=site, session=session)
  
  #tag item with our special identifier
  item_update_identifier(item, scheme="mda_streams", type="site_root",
                         key=site, session=session)
  
  
  return(item)  
  
}