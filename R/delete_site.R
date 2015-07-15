#' Delete a site and all its files.
#' 
#' @param sites one or more site IDs such as "nwis_08437710" to look up and 
#'   delete
#' @param children_only logical. If TRUE, only the child items in the site 
#'   folder will be deleted, leaving an empty site folder
#' @param verbose logical. Should status messages be given?
#' @return anything passed back from the final item_rm if children_only==FALSE,
#'   or a pointer to the remaining item if children_only==TRUE
#' @keywords internal
#' @examples
#' \dontrun{
#' sbtools::authenticate_sb()
#' set_scheme("mda_streams_dev")
#' 
#' sites <- c("nwis_00000000", "nwis_00000001", "nwis_00000002")
#' post_site(sites)
#' mda.streams:::delete_site(sites)
#' 
#' set_scheme("mda_streams")
#' }
delete_site <- function(sites, children_only=FALSE, verbose=TRUE) {
  
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before deleting")
  
  deletion_msgs <- lapply(sites, function(site) {
    # find the item id by hook or by crook (aka tag or dir)
    site_id <- locate_site(site, by="either")
    if(is.na(site_id)) {
      if(isTRUE(verbose)) message("skipping deletion of missing site ", site)
      return(NA) # do nothing if it's already not there
    }
    
    # delete the children and their files
    children <- item_list_children(site_id, limit=100)
    if(nrow(children) > 0) {
      for(child in children$id) {
        item_rm_files(child) # delete any data files from the child
      }
      # use 2 loops to reduce waiting time for multiple children
      for(child in children$id) {
        for(wait in 1:100) {
          Sys.sleep(0.2) # sleep to give time for full deletion
          if(nrow(item_list_files(child)) == 0) break
        }
        item_rm(child) # delete the child itself
      }
      # sleep for a bit so it can finish deleting the children
      for(wait in 1:100) {
        Sys.sleep(0.2)
        if(nrow(item_list_children(site_id, limit=2)) == 0) break
        if(wait==100) stop("failed to delete children of site ", site)
      }
    }
    
    # delete the site folder or return its ID
    if(children_only) {
      return(site_id)
    } else {
      # delete the folder
      if(isTRUE(verbose)) message("deleting site ", site)
      out <- item_rm(site_id)
      # sleep again to finish deleting the folder
      for(wait in 1:100) {
        Sys.sleep(0.2)
        if(is.na(locate_site(site))) break
        if(wait==100) stop("failed to delete site ", site)
      }
      # if it worked, return the output
      return(out)
    }
  })
  
  invisible(deletion_msgs)
}