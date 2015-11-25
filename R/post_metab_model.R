#' Post individual metab_model object[s] to SB
#' 
#' @param files character one or more files to post to SB
#' @param on_exists character. what should be done when an item already exists?
#' @param verbose logical. Should status messages be given?
#' @return an item list
#' @author Alison Appling
#' @import sbtools
#' @importFrom stats setNames
#' @export
#' @examples 
#' \dontrun{
#' login_sb()
#' set_scheme("mda_streams_dev")
#' # stage_metab_model...
#' # post_metab_model...
#' set_scheme("mda_streams")
#' }
post_metab_model <- function(files, on_exists=c("stop", "skip", "replace_file"), verbose=TRUE) {
  # handle inputs
  on_exists <- match.arg(on_exists)
  if(is.null(current_session()) || !session_validate()) stop("need ScienceBase access; call login_sb() first")
  
  model_ids <- sapply(setNames(files, files), function(modelfile) {
    
    # look for an existing ts and respond appropriately
    modelpath <- parse_metab_model_path(modelfile)
    model_id <- locate_metab_model(modelpath$model_name, by="either")
    if(!is.na(model_id)) {
      if(verbose) message('the metab_model item ', modelpath$model_name, ' already exists on SB')
      switch(
        on_exists,
        "stop"={ 
          stop('the metab_model item already exists and on_exists="stop"') },
        "skip"={ 
          if(isTRUE(verbose)) message("skipping posting of the metab_model item") 
          return(NA) # na is signal that doesn't need new tags
        },
        "replace_file"={
          stop("whoops - this isn't yet possible")
        })
    } else {
      # create the item
      model_id <- item_create(locate_folder("metab_models"), title=modelpath$model_name)$id
    }
    
    # attach data file to ts item. SB quirk: must be done before tagging with 
    # identifiers, or identifiers will be lost
    if(verbose) message("posting metab_model file ", modelpath$file_name)
    item_append_files(model_id, files = modelfile)
    
    return(model_id)  
  })
  
  # separate loop to increase probability of success in re-tagging files when length(files) >> 1
  posted_items <- sapply(1:length(model_ids), function(i) {
    
    # if we skipped it once, skip it again
    if(is.na(model_ids[i])) 
      return(NA)
    else
      model_id <- model_ids[i]
    
    # parse (again) the file name to determine where to post the file
    modelpath <- parse_metab_model_path(files[i])
    
    # tag item with our special identifiers. if the item already existed,
    # identifiers should be wiped out by a known SB quirk, so sleep to give time
    # for the files to be added and the identifiers to disappear so we can replace them
    for(wait in 1:100) {
      Sys.sleep(0.2)
      if(nrow(sbtools::item_list_files(model_id)) > 0 && is.null(item_get(model_id)$identifiers)) break
      if(wait==100) stop("identifiers didn't disappear and so can't be replaced")
    }
    if(verbose) message("adding/replacing identifiers for item ", model_id, ": ",
                        "scheme=", get_scheme(), ", type=", "metab_model", ", key=", modelpath$model_name)
    repair_metab_model(modelpath$model_name, limit=5000)
    
    model_id
  })
  
  invisible(posted_items)
}
