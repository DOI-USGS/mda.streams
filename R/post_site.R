#' Create new site in SB
#' 
#' @param sites one or more local site IDs (e.g. nwis_09238475)
#' @param replace_existing logical. Should an item that already exists be 
#'   replaced?
#' @param verbose logical. Should status messages be given?
#' @return an item list
#' @author Corinna Gries
#' @import sbtools
#' @export
#' @examples 
#' \dontrun{
#' sbtools::authenticate_sb()
#' set_scheme("mda_streams_dev")
#' 
#' sites <- c("nwis_00000000", "nwis_00000001", "nwis_00000002")
#' is.null(post_site()) # returns NULL (empty list) if sites is missing or NULL
#' (post_site(sites)) # adds sites, returns item IDs
#' (post_site(sites, on_exists="skip")) # leaves existing items untouched, returns item IDs
#' (post_site(sites, on_exists="clear")) # empties existing sites of child items, returns item IDs
#' mda.streams:::delete_site(sites)
#' 
#' set_scheme("mda_streams")
#' }
post_site <- function(sites, on_exists=c("stop", "skip", "clear", "replace"), verbose = TRUE){
  
  # check inputs & session
  if(missing(sites) || is.null(sites)) return(invisible(NULL))
  on_exists <- match.arg(on_exists)
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before posting")
  
  posted_items <- sapply(setNames(sites, sites), function(site) {
    
    # look for an existing ts and respond appropriately
    site_id <- locate_site(site, by="either")
    if(!is.na(site_id)) {
      if(verbose) message('the folder for site ', site, ' already exists')
      switch(
        on_exists,
        "stop"={ 
          stop('site already exists and on_exists="stop"') },
        "skip"={ 
          if(isTRUE(verbose)) message("skipping site") 
          return(site_id)
        },
        "clear"={
          if(isTRUE(verbose)) message("deleting children from the site")
          return(delete_site(site, children_only=TRUE, verbose=verbose))
        },
        "replace"={
          if(isTRUE(verbose)) message("deleting the entire site")
          delete_site(site, verbose=verbose)
        }
      )
    }
     
    # create the item and add title, scheme, type, and key
    site_id <- item_create(parent_id=locate_folder("sites"), title=site)
    item_update_identifier(site_id, scheme=get_scheme(), type="site_root", key=site)
    
    return(site_id)  
  })
  
  invisible(posted_items)
}

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
    if(isTRUE(verbose)) message("deleting site ", site)
    
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
    }
    
    # sleep for a bit so it can finish deleting the children
    for(wait in 1:100) {
      Sys.sleep(0.2)
      if(nrow(item_list_children(site_id, limit=2)) == 0) break
      if(wait==100) stop("failed to delete children of site ", site)
    }
    
    # delete the site folder or return its ID
    if(children_only) {
      return(site_id)
    } else {
      # delete the folder
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