#' Delete a metadata item-file combination
#' 
#' Deletes a metadata object
#' 
#' @param types character. One or more types of meta file
#' @param files_only logical. Should only the files, not the item, be delted?
#' @param verbose logical. Should status messages be given?
#' @keywords internal
delete_meta <- function(types, files_only=FALSE, verbose=TRUE) {
  if(is.null(current_session())) stop("need ScienceBase access; call login_sb() first")
  
  deletion_msgs <- lapply(types, function(type) {
    # find the item id by hook or by crook (aka tag or dir)
    meta_id <- locate_meta(type, by="either")
    if(is.na(meta_id)) {
      if(isTRUE(verbose)) message("skipping deletion of missing meta_", type)
      return(NA) # do nothing if it's already not there
    } else {
      if(verbose) message("deleting ", type, " metadata (", meta_id, ")")
      
      # delete any data files from the metadata item
      item_status <- item_rm_files(meta_id)
      if(!is.list(item_status)) stop("couldn't delete item because couldn't find files")
      # sleep to give time for full deletion
      for(wait in 1:50) {
        Sys.sleep(0.2)
        if(nrow(item_list_files(meta_id)) == 0) break
        if(wait==50) stop("failed to delete files & therefore item")
      }
      
      # delete the metadata item itself or return its ID
      if(files_only) {
        return(meta_id) 
      } else {
        # delete the item
        out <- item_rm(meta_id) 
        # sleep (again!) to give time for full deletion
        for(wait in 1:50) {
          Sys.sleep(0.2)
          if(is.na(locate_meta(type=type, by="either"))) break
          if(wait==50) stop("failed to delete item (", meta_id, ")")
        }
        # if it worked, return the output
        return(out)
      }
    }
    
  })
  
  invisible(deletion_msgs)  
}
