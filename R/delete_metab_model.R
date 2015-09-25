#' Delete a metab_model item from SB
#' 
#' Delete the metab_model item and its files from SB
#' 
#' @param model_name the model name, e.g., "nwis_05515500-35-150729 0.1.2 compare_par_srces"
#' @param files_only logical. Only delete the file[s], leaving an empty item?
#' @param verbose logical. Give status messages?
#' @import sbtools
#' @examples
#' \dontrun{
#' set_scheme('mda_streams_dev')
#' login_sb()
#' 
#' set_scheme('mda_streams')
#' }
delete_metab_model <- function(model_name, files_only=FALSE, verbose=TRUE) {
  if(is.null(current_session())) stop("need ScienceBase access; call login_sb() first")
  
  item_ids <- locate_metab_model(model_name=model_name, by="either")
  delete_item(item_ids, item_names=model_name, delete_files=TRUE, delete_children=FALSE, delete_item=!files_only, verbose=verbose)
}