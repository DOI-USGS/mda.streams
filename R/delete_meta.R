#' Delete a metadata item-file combination
#' 
#' Deletes a metadata object
#' 
#' @param types character. One or more types of meta file
#' @param files_only logical. Should only the files, not the item, be delted?
#' @param verbose logical. Should status messages be given?
#' @keywords internal
delete_meta <- function(types, files_only=FALSE, verbose=TRUE) {
  meta_ids <- locate_meta(types)
  delete_item(item_ids=meta_ids, item_names=types, 
              delete_files=files_only, delete_children=FALSE, delete_item=!files_only, verbose=verbose)
}
