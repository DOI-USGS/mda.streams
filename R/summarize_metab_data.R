#' Summarize a timeseries
#' 
#' @param config a config file/data.frame from which datasets should be
#'   constructed for summarization.
#' @param rows missing, integer, or vector of integers. The row number[s] of the
#'   config data.frame to use for this particular data summary.
#' @param out a list of one or more outputs to include in the summary dataframe
#' @param verbose logical. supply status messages?
#' @import dplyr
#' @export
summarize_metab_inputs <- function(config, rows, out=c("site", "model", "tag", "strategy", "date", "start_date","end_date","num_dates","num_rows","num_complete","modal_timestep","num_modal_timesteps"), verbose=TRUE) {
  
  # check inputs & session
  out <- match.arg(out, several.ok=TRUE)
  
  # Check the input
  if(!is.data.frame(config) && is.character(config)) {
    config <- read.table(config, sep="\t", header=TRUE, colClasses="character")
  }
  rows <- if(missing(rows)) 1:nrow(config) else rows
  
  # Check config. If it fails verification, return right now
  err_strs <- warn_strs <- character()
  verification <- withCallingHandlers(
    tryCatch({
      verify_config(config[rows,], on_fail=stop)
    },
    error=function(e) { 
      err_strs <<- paste0("verify config: ",e$message)
      FALSE
    }),
    warning=function(w) { 
      warn_strs <<- c(warn_strs, paste0("verify config: ",w$message))
      invokeRestart("muffleWarning")
    })
  if(!isTRUE(verification)) {
    out <- "config could not be verified"
    if(length(warn_strs) > 0) attr(out, "warnings") <- warn_strs
    if(length(err_strs) > 0) attr(out, "errors") <- err_strs
    return(list(out)) # return a list because we're outside the lapply
  }
  
  # define the units to be assigned after the lapply loop
  summary_units <- c(
    start_date="", end_date="", num_dates="dates", 
    num_rows="rows", num_complete="rows", 
    modal_timestep="mins", num_modal_timesteps="steps")
  summary_units <- summary_units[out[out %in% names(summary_units)]]
  config_units <- setNames(rep(NA, length(out[out %in% names(config)])), out[out %in% names(config)])

    # Summarize the metabolism data for each selected config row
  full_sumry <- lapply(rows, function(row) {
    
    if(verbose) message("row ", row, ": summarizing metab data for this config row")
    
    # Locate the model function. Look first in streamMetabolizer, then in the
    # current and inherited environments.
    metab_fun <- NA
    err_strs <- character()
    tryCatch(metab_fun <- get(config[[row,'model']], envir = environment(streamMetabolizer::metab_model)), 
             error=function(e) err_strs <<- append(err_strs, e$message) )
    tryCatch(metab_fun <- get(config[[row,'model']]), 
             error=function(e) err_strs <<- append(err_strs, e$message) )
    if(!is.function(metab_fun)) {
      out <- "error in locating metab_fun"
      attr(out, "errors") <- err_strs
      return(out)
    }
    
    # Prepare the model arguments
    metab_args <- NA
    try(metab_args <- eval(parse(text=config[[row,'model_args']])), silent=TRUE)
    if(isTRUE(is.na(metab_args)) || !is.list(metab_args)) {
      out <- "error in evaluating metab_args"
      attr(out, "errors") <- "could not parse or evaluate args"
      return(out)
    }
    
    # Prepare the data, passing along any errors from config_to_data
    if(verbose) message("row ", row, ": preparing metab_data...")
    metab_data <- config_to_data(config[row,], row, metab_fun, metab_args, on_error='quiet')
    metab_data_ok <- is.null(attr(metab_data, "errors"))
    data_errors <- if(metab_data_ok) NA else paste0(attr(metab_data, "errors"), collapse="; ")
    data_warnings <- if(is.null(attr(metab_data, "warnings"))) NA else paste0(attr(metab_data, "warnings"), collapse="; ")
    
    # Summarize the data
    
    # define the functions that might be applied to each metab_data, then
    # select only those that have been requested
    summary_funs <- list(
      start_date = function(ts) min(ts$local.time),
      end_date = function(ts) max(ts$local.time),
      num_dates = function(ts) length(unique(as.Date(ts$local.time))),
      num_rows = function(ts) nrow(ts),
      num_complete = function(ts) length(which(complete.cases(ts))),
      modal_timestep = function(ts) {
        table_tsteps <- suppressWarnings(table(as.numeric(diff(v(ts$local.time)), units="mins")))
        as.numeric(names(which.max(table_tsteps)))
      },
      num_modal_timesteps = function(ts) {
        table_tsteps <- suppressWarnings(table(diff(v(ts$local.time))))
        table_tsteps[[which.max(table_tsteps)]]
      }
    )
    summary_funs <- summary_funs[out[out %in% names(summary_funs)]]
    
    config_outs <- config[row, out[out %in% names(config)]]
    
    # create a data.frame of summary metrics, one ts per row. in each row only 
    # compute the selected summary metrics, and only compute them if the file is
    # available
    if(!metab_data_ok) {
      sumry <- as.data.frame(c(
        as.list(config_outs),
        list(config_row=row),
        lapply(summary_funs, function(fun) { NA }),
        list(data_errors=data_errors, data_warnings=data_warnings)
      ), stringsAsFactors=FALSE)
    } else {
      metab_data <- as.data.frame(v(metab_data))
      #metab_data[complete.cases(metab_data),] # we'll count both ways
      sumry <- as.data.frame(c(
        as.list(config_outs),
        list(config_row=row),
        lapply(summary_funs, function(fun) { fun(metab_data) }),
        list(data_errors=data_errors, data_warnings=data_warnings)
      ), stringsAsFactors=FALSE)
    }
    
    # If we've made it this far, the summary ought to have worked
    return(sumry)
  }) %>% bind_rows() %>% 
    as.data.frame() %>% 
    u(c(config_units, config_row=NA, summary_units, data_errors=NA, data_warnings=NA))
  
  # return
  full_sumry
}