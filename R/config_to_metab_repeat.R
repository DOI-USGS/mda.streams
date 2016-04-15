#' Acquire data and fit a model repeatedly
#' 
#' This function really only makes sense in the context of models that are 
#' fitted from simulated doobs data, which in turn must be specified as preds 
#' from a metab_sim model.
#' 
#' @return A metab_model object with an augmented \code{info} slot (access with 
#'   \code{get_info}) that includes, most importantly, a list element named 
#'   \code{fit_reps}: \code{fit_reps} is a data.frame containing information on 
#'   each repetition of the config_to_metab procedure, one row per repetition. 
#'   Data columns include the metabolism estimates, data prep time, model
#'   fitting time, doobs simulation model, and an abbreviated version of the
#'   metab estimation config specifications .
#'   
#' @param config a config table
#' @param row a single integer specifying hte row of config to use
#' @param times integer giving the number of times to fit the one config row
#' @param verbose logical. give status messages?
#' @export
config_to_metab_repeat <- function(config, row, times=5, verbose=FALSE) {
  if(length(row) != 1) stop("this function is for 1 row at a time")
  if(verbose) message("repeatedly fitting models for config row ", row)
  
  # collect info about the simulation model being used in this config row
  . <- src <- error_strategy <- strategy <- model_args <- '.dplyr.var'
  sm <- get_metab_model(config$doobs.src[row])
  sim <- data.frame(
      model_name = config$doobs.src[row],
      params = get_specs(sm)$calc_DO_args[c('err.obs.sigma','err.proc.sigma','err.proc.phi','ODE_method')],
      data_daily = get_data_daily(sm),
      stringsAsFactors=FALSE)
  
  # now run the model many times and collect info to return. run one model at a 
  # time so we're not storing a large number of models in session memory all at 
  # once. save just the first one in its entirety
  tag <- model <- site <- '.dplyr.var'
  rep_config <- config[rep(row, times=times), ]
  mtb_to_save <- NULL
  fits_to_save <- list()
  mtbout <- lapply(1:times, function(reprow) {
    mm <- config_to_metab(rep_config, rows=reprow, verbose=verbose)[[1]]
    if(is(mm, 'metab_model')) {
      if(length(mtb_to_save) == 0 || reprow == times) mtb_to_save <<- mm
      fits_to_save[[reprow]] <<- get_fit(mm)
      tryCatch(
        data.frame(
          rep=reprow,
          predict_metab(mm),
          prep_time=as.data.frame(as.list(get_info(mm)$prep_time)),
          fitting_time=as.data.frame(as.list(get_fitting_time(mm))),
          stringsAsFactors=FALSE),
        error=function(e) 
          NULL
      )
    } else {
      NULL
    }
  })
  if(length(mtbout) > 0) mtbout <- bind_rows(mtbout)

  # add info to a single model object
  if(is(mtb_to_save, 'metab_model')) {
    mtb_to_save@info <- c(get_info(mtb_to_save), list(
      fit_reps=tryCatch(left_join(mtbout, data.frame(sim=sim), by=c('date'='sim.data_daily.date')), error=function(e) e),
      fit_raws=fits_to_save,
      sim_model=sm
    ))
  } else {
    warning("no call to config_to_metab returned a metab_model")
  }
  mtb_to_save
}
