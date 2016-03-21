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
#' mm <- streamMetabolizer::metab_mle(info=list(config=data.frame(site="dummy")))
#' staged <- stage_metab_model("150415 0.0.0 dummy_model", mm)
#' mname <- parse_metab_model_path(staged)$model_name
#' post_metab_model(staged)
#' list_metab_models()
#' sbtools::item_list_files(locate_metab_model(list_metab_models()))$fname
#' mda.streams:::delete_metab_model(mname, files_only=TRUE)
#' sbtools::item_list_files(locate_metab_model(list_metab_models()))$fname
#' mda.streams:::delete_metab_model(mname)
#' list_metab_models()
#' set_scheme('mda_streams')
#' }
delete_metab_model <- function(model_name, files_only=FALSE, verbose=TRUE) {
  item_ids <- locate_metab_model(model_name=model_name, by="either")
  delete_item(item_ids, item_names=model_name, 
              delete_files=files_only, delete_children=FALSE, delete_item=!files_only, verbose=verbose)
}