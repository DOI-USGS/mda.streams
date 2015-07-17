#' Delete a metab_run item from SB
#' 
#' Delete the run item, and/or its files, from SB
#' 
#' @param title the metab_run title
#' @param files_only logical. Only delete the files, leaving an empty run item?
#' @param verbose logical. Give status messages?
#' @examples
#' \dontrun{
#' set_scheme('mda_streams_dev')
#' authenticate_sb()
#' 
#' set_scheme('mda_streams')
#' }
delete_metab_run <- function(title, files_only=FALSE, verbose=TRUE) {
  item_ids <- locate_metab_run(title=title, by="either")
  delete_item(item_ids, item_names=title, delete_files=TRUE, delete_children=FALSE, delete_item=!files_only, verbose=verbose)
}