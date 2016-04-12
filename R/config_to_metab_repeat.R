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
  
  # run the model many times
  rep_config <- config[rep(row, times=times), ]
  mtb <- config_to_metab(rep_config, verbose=verbose)
  
  # collect info to return
  . <- src <- error_strategy <- strategy <- model_args <- tag <- model <- site <- '.dplyr.var'
  sim_model <- get_metab_model(config$doobs.src[row])
  sim_error <- get_info(sim_model)$config %>%
    mutate(
      src = config$doobs.src[row],
      error_strategy=sapply(strsplit(strategy, " "), `[`, 3)) %>%
    .[rep(1,times),] %>%
    select(src, error_strategy, strategy, model_args)
  estimates <- bind_rows(lapply(mtb, function(mm) predict_metab(mm)))
  prep_time <- bind_rows(lapply(mtb, function(mm) as.data.frame(as.list(get_info(mm)$prep_time))))
  fitting_time <- bind_rows(lapply(mtb, function(mm) as.data.frame(as.list(get_fitting_time(mm)))))

  # add info to a single model object
  mtb_to_save <- mtb[[1]]
  mtb_to_save@info <- c(mtb_to_save@info, list(
    fit_reps=data.frame(
      estimates,
      prep_time=prep_time,
      fitting_time=fitting_time,
      sim=sim_error,
      config=select(mtb_to_save@info$config, tag, strategy, date, model, model_args, site),
      stringsAsFactors=FALSE),
    sim_model=sim_model
  ))
  mtb_to_save
}
