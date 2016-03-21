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
  sb_require_login("stop")
  
  # initial loop to post the file
  model_ids <- sapply(setNames(files, files), function(modelfile) {
    
    # look for an existing item
    modelpath <- parse_metab_model_path(modelfile)
    model_id <- locate_metab_model(modelpath$model_name, by="either")

    # create the item if it didn't already exist; check for file conflicts
    if(is.na(model_id)) {
      model_id <- item_create(locate_folder("metab_models"), title=modelpath$model_name)$id
      repair_metab_model(modelpath$model_name, verbose=verbose)
      file_exists <- FALSE
    } else {
      file_exists <- basename(modelfile) %in% sbtools::item_list_files(model_id)$fname
    }
    
    # attach data file to item
    if(!file_exists) {
      # if file didn't exist, just add it. identifier loss after file upload
      # used to be a much flakier process, but see 
      # https://github.com/USGS-R/sbtools/issues/74 - we think it's solved now.
      if(verbose) message('posting new metab_model file "', modelpath$file_name, '"')
      item_append_files(model_id, files = modelfile)
    } else {
      # handle file conflicts
      if(verbose) message('the metab_model file "', modelpath$file_name, '" already exists on SB')
      switch(
        on_exists,
        "stop"={ 
          stop('the metab_model item already exists and on_exists="stop"')
        },
        "skip"={ 
          if(isTRUE(verbose)) message("skipping posting of the metab_model item") 
          return(NA)
        },
        "replace_file"={
          if(isTRUE(verbose)) message("replacing the metab_model file") 
          model_id <- item_replace_files(model_id, files=modelfile, all=FALSE)$id
          # identifier loss after file upload used to be a much flakier process, but see
          # https://github.com/USGS-R/sbtools/issues/74 - we think it's solved now.
        })
    }
    
    return(model_id)  
  })
  
  invisible(model_ids)
}
