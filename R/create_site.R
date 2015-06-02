#' Create new site in SB
#' 
#' @param site Local site ID (e.g. nwis_09238475)
#' @param ... Additional parameters supplied to \code{\link[sbtools]{session_check_reauth}}
#' @param replace_existing logical. Should an item that already exists be
#'   replaced?
#' @return an item list
#' @author Corinna Gries
#' @import sbtools
#' @export
create_site <- function(site, replace_existing = FALSE, ...){
  
	session_check_reauth(...)
	session = current_session()
	
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