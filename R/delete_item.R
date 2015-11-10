#' Delete a ScienceBase item
#' 
#' Deletes one or more generic items, including their child items and files as
#' applicable & specified.
#' 
#' @param item_ids character. SB id[s]
#' @param item_names character. The name[s] by which to refer to the item[s] in 
#'   messages
#' @param delete_files logical. Should files be sought and deleted from within 
#'   this item? If FALSE, files will be ignored.
#' @param delete_children logical. Should child items be sought and recursively 
#'   deleted from within this item? If FALSE, children will be ignored.
#' @param delete_item logical. Should the item itself be deleted after files 
#'   and/or children are deleted?
#' @param verbose logical. Should status messages be given?
#' @keywords internal
delete_item <- function(item_ids, item_names, delete_files=FALSE, delete_children=FALSE, delete_item=TRUE, verbose=TRUE) {
  
  if(is.null(current_session()) || !session_validate()) stop("need ScienceBase access; call login_sb() first")
  if(length(item_ids) != length(item_names)) stop("expecting item_ids and item_names to have the same length")
  
  # delete in a loop over the assumed-to-be parallel vectors item_ids and item_names
  deletion_msgs <- lapply(1:length(item_ids), function(q) {
    
    item_id <- item_ids[q]
    item_name <- item_names[q]
    
    # do nothing & return if it's already gone
    if(is.na(item_id)) {
      if(isTRUE(verbose)) message("skipping deletion of missing item ", item_name)
      return(NA)
    }
    
    # declare intent
    deletion_msg <- list()
      
    # if requested, delete all files from the item
    if(isTRUE(delete_files)) {
      if(verbose) message("deleting files from item ", item_name, " (", item_id, ")")
      # do the deletion
      item_status <- item_rm_files(item_id)
      # sleep to give time for full deletion
      for(wait in 1:50) {
        Sys.sleep(0.2)
        if(nrow(item_list_files(item_id)) == 0) break
        if(wait==50) stop("failed to delete files & therefore item (", item_id, ")")
      }
      deletion_msg <- c(deletion_msg, list("deleted all files")) # this will be overwritten if delete_item=TRUE
    }
    
    # if requested, delete all children from the item. this is recursive - be careful!
    if(isTRUE(delete_children)) {
      if(verbose) message("deleting children from item ", item_name, " (", item_id, ")")
      # identify child items to delete
      children <- item_list_children(item_id, limit=1000)
      # delete them
      if(nrow(children) > 0) {
        for(child in children$id) {
          delete_item(item_id=child, item_name=child, delete_files=FALSE, delete_children=TRUE, delete_item=TRUE, verbose=verbose)
        }
      }
      # sleep to give time for full deletion
      for(wait in 1:50) {
        Sys.sleep(0.2)
        if(nrow(item_list_children(item_id, limit=1)) == 0) break
        if(wait==50) stop("failed to delete children & therefore item (", item_id, ")")
      }
      deletion_msg <- c(deletion_msg, list("deleted all children")) # this will be overwritten if delete_item=TRUE
    }
    
    # if requested, delete the item itself
    if(isTRUE(delete_item)) {
      if(verbose) message("deleting item ", item_name, " (", item_id, ")")
      # do the deletion
      rm_msg <- item_rm(item_id) 
      deletion_msg <- c(deletion_msg, list(rm_msg))
      # sleep to give time for full deletion
      for(wait in 1:50) {
        Sys.sleep(0.2)
        item_gone <- tryCatch({item_get(item_id); FALSE}, error=function(e) TRUE) # error=success here
        if(item_gone) break
        if(wait==50) stop("failed to delete item (", item_id, ")")
      }
    }
    
    # if it worked, return the output
    deletion_msg
  })
  
  invisible(deletion_msgs)  
}
