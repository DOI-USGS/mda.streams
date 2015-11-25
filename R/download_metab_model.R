#' Download metabolism model .RData to local file
#' 
#' WARNING: older metab_models may not be compatible with contemporary 
#' streamMetabolizer or mda.streams functions. To investigate older models, 
#' either apply modernize_metab_model to your downloaded files or use 
#' get_metab_model, which does this for you.
#' 
#' @param model_name the name of the metab_model file
#' @param version character indicating whether you want the original metab_model
#'   or a modernized one that works with the current streamMetabolizer version
#' @inheritParams download_item_files
#' @return file handle (character path) for the downloaded file, or NA if the 
#'   timeseries is unavailable on ScienceBase
#'   
#' @author Alison P Appling
#' @examples
#' \dontrun{
#' download_metab_model("nwis_04087088-200-150730 0.0.7 MLE_for_PRK_wHarvey_and_sw")
#' }
#' @import sbtools
#' @export
download_metab_model <- function(model_name, folder = tempdir(), version=c('modern','original'),
                               on_remote_missing=c("stop","return_NA"), 
                               on_local_exists=c("stop","skip","replace")) {
  
  if(is.null(current_session()) || !session_validate()) stop("need ScienceBase access; call login_sb() first")
  
  version <- match.arg(version)
  on_remote_missing <- match.arg(on_remote_missing)
  on_local_exists <- match.arg(on_local_exists)
  
  ids <- locate_metab_model(model_name)
  
  download_item_files(
    item_ids=ids, item_names=model_name, files=make_metab_model_path(model_name, version=version), folder=folder, 
    on_remote_missing=on_remote_missing, on_local_exists=on_local_exists)
    
}