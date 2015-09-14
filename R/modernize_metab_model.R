#' Update a metabolism model from SB to meet current formatting requirements
#' 
#' Known changes since first models include:
#' (1) config was the entire info slot, now is an element named config in a list that is the info slot.
#' (2) config df has more columns now; old columns have not changed names or contents.
#' (3) the column named 'date' in early fits is now named 'local.date'.
#' (4) the args list may be expanded from before but retains the same 
#' 
#' @param metab_model a model or list of models
#' @import dplyr
#' @export
modernize_metab_model <- function(metab_model) {
  # if metab_model isn't already a list, make it one
  if(!is.list(metab_model)) metab_model <- list(metab_model)
  
  # redefine the contents of each model
  new_mms <- lapply(metab_model, function(old_mm) {
    
    # model_class: keep the same class
    new_model_class <- class(old_mm)
    
    # info: if the info is a config df, move that df into a list element named 
    # config. if the config isn't complete, add the needed columns as NAs.
    old_info <- get_info(old_mm)
    old_config <- if(is.data.frame(old_info)) old_info else old_info$config
    empty_config <- suppressWarnings(stage_metab_config(tag='0.0.0', strategy='updating SB metab model', site=NA, file=NULL))
    new_config <- bind_rows(empty_config, old_config) %>% as.data.frame
    new_info <- list(config=new_config)
 
    # fit: rename 'date' to 'local.date'
    new_fit <- get_fit(old_mm)
    if('date' %in% names(new_fit)) names(new_fit)[which(names(new_fit) == 'date')] <- 'local.date'

    # args: args list may have changed, but until this is a problem for another 
    # function, leave it untouched
    new_args <- get_args(old_mm)
    
    # data: leave untouched
    new_data <- get_data(old_mm)
    
    # data_daily: may be missing from older sites, so seek it robustly.
    # Otherwise, don't change it.
    new_data_daily <-  tryCatch(
      suppressWarnings(get_data_daily(old_mm)),
      error=function(e) NULL)
    
    # pkg_version: keep unchanged
    new_pkg_version <- old_mm@pkg_version
    
    # create a new model and copy the contents over - this creates any
    # additional slots that were missing in the old model, e.g., data_daily
    new_mm <- 
      metab_model(
        model_class=new_model_class,
        info=new_info,
        fit=new_fit,
        args=new_args,
        data=new_data,
        data_daily=new_data_daily,
        pkg_version=new_pkg_version)
    
    # return
    new_mm
  })
  
  if(length(new_mms) == 1) {
    new_mms[[1]]
  } else {
    new_mms
  }
}