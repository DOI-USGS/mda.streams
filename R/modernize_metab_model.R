#' Update a metabolism model from SB to meet current formatting requirements
#' 
#' Known changes since first models include:
#' 
#' (1) config was the entire info slot, now is an element named config in a list
#' that is the info slot.
#' 
#' (2) config df has more columns now; old columns have not changed names or
#' contents.
#' 
#' (3) the column named 'date' in early fits and 'local.date' in the next round
#' is now named 'solar.date'.
#' 
#' (4) the args list may be expanded from before but retains the same core
#' elements.
#' 
#' @param metab_model a model or list of models
#' @import dplyr
#' @import streamMetabolizer
#' @importFrom unitted u v
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
    if('config.row' %in% names(old_config)) old_config$config.row <- as.numeric(old_config$config.row)
    empty_config <- stage_metab_config(tag='0.0.0', strategy='updating SB metab model', site=NA, filename=NULL)
    new_config <- bind_rows(empty_config, old_config) %>% as.data.frame(stringsAsFactors=FALSE)
    if(!('config.row' %in% names(old_config))) {
      new_config$config.row <- rownames(old_config)
    }
    new_info <- if(is.list(old_info)) old_info else list()
    new_info$config <- new_config
 
    # fit: rename 'date' and 'local.date' to 'solar.date' and add row.first, row.last to metab_night models
    new_fit <- get_fit(old_mm)
    if(any(c('date','local.date') %in% names(new_fit))) 
      names(new_fit)[which(names(new_fit) %in% c('date','local.date'))] <- 'solar.date'
    if(class(old_mm)=='metab_night' && !('row.first' %in% names(get_data(old_mm)))) {
      new_fit_rows <- streamMetabolizer:::mm_model_by_ply(
        function(data_ply, data_daily_ply, day_start, day_end, local_date, tests, model_specs) {
          which_night <- which(data_ply$light < 0.1) #v(u(0.1, "umol m^-2 s^-1")))
          has_night <- length(which_night) > 0
          data.frame(
            row.first = if(has_night) which_night[1] else NA,
            row.last = if(has_night) which_night[length(which_night)] else NA)
        },
        data=v(get_data(old_mm)), data_daily=NULL,
        day_start=get_args(old_mm)$day_start, day_end=get_args(old_mm)$day_end,
        tests=c(), model_specs=c()
      )
      new_fit <- left_join(new_fit, new_fit_rows, by='solar.date')
    }
    
    if(class(old_mm) == 'metab_bayes') {
      new_mcmc <- tryCatch(get_mcmc(old_mm), error=function(e) NULL)
    }
    
    # fitting_time: add dummy if it wasn't there before
    new_fitting_time <- tryCatch(
      suppressWarnings(get_fitting_time(old_mm)),
      error=function(e) system.time({}))

    # args: args list may have changed, but until this is a problem for another 
    # function, leave it untouched
    new_args <- get_args(old_mm)
    
    # data: rename 'local.time' to 'solar.time' (and also see below, where we'll
    # add preds to the data after putting it into a new metab_model)
    new_data <- get_data(old_mm)
    if('local.time' %in% names(new_data))
      names(new_data)[which(names(new_data) == 'local.time')] <- 'solar.time'
    
    # data_daily: may be missing from older sites, so seek it robustly.
    # Otherwise, don't change it.
    new_data_daily <-  tryCatch(
      suppressWarnings(get_data_daily(old_mm)),
      error=function(e) NULL)
    
    # pkg_version: mark with our new
    new_pkg_version <- paste0(packageVersion("streamMetabolizer"), " (was ", old_mm@pkg_version, ")")
    
    # create a new model and copy the contents over - this creates any
    # additional slots that were missing in the old model, e.g., data_daily
    new_mm <- 
      metab_model(
        model_class=new_model_class,
        info=new_info,
        fit=new_fit,
        fitting_time=new_fitting_time,
        args=new_args,
        data=new_data,
        data_daily=new_data_daily,
        pkg_version=new_pkg_version)
    if(class(new_mm)=='metab_bayes') new_mm@mcmc <- new_mcmc
    
    # data: add predictions if missing & available
    if(!('DO.mod' %in% names(new_data)) && new_model_class != 'metab_Kmodel') {
      tryCatch({
        new_mm@data <- predict_DO(new_mm)
      }, error=function(e) {
        warning(e)
      })
    }

    # return
    new_mm
  })
  
  if(length(new_mms) == 1) {
    new_mms[[1]]
  } else {
    new_mms
  }
}