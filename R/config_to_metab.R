#' Actually run the model as specified by the configuration arguments
#' 
#' @param config data.frame, or file/filename to read a data.frame from. As an 
#'   alternative to all preceding arguments, this single config argument may be 
#'   passed in. The data.frame columns must correspond precisely to the list of 
#'   preceding arguments.
#' @param rows missing, integer, or vector of integers. The row number[s] of the
#'   config data.frame to use for this particular model run.
#' @param verbose logical. Should status messages be given?
#' @param prep_only logical. If TRUE, data will be produced and returned without
#'   ever fitting a metabolism model. (return value is a list with 'data', 
#'   'data_daily', and/or 'args' elements)
#' @import streamMetabolizer
#' @import dplyr
#' @importFrom utils read.table
#' @importFrom stats complete.cases
#' @export
#' @examples
#' \dontrun{
#' config_to_metab(config=stage_metab_config(
#'   tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_04087142", start_date="2013-06-14", end_date="2013-06-19", 
#'   filename=NULL))
#' }
config_to_metab <- function(config, rows, verbose=TRUE, prep_only=FALSE) {

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
  
  # Run metabolism for each selected config row
  fits <- lapply(rows, function(row) {
    
    prep_time <- system.time({
      
      if(verbose) message("row ", row, ": initiating metab modeling for this config row")
      
      # Locate the model function. Look first in streamMetabolizer, then in the
      # current and inherited environments.
      metab_fun <- NA
      err_strs <- character()
      tryCatch(metab_fun <- get(config[[row,'model']], envir = environment(streamMetabolizer::metab_model)), 
               error=function(e) err_strs <<- append(err_strs, e$message) )
      if(!is.function(metab_fun)) 
        tryCatch(metab_fun <- get(config[[row,'model']]), 
                 error=function(e) err_strs <<- append(err_strs, e$message) )
      if(!is.function(metab_fun)) {
        out <- "error in locating metab_fun"
        attr(out, "errors") <- err_strs
        return(out)
      }
      
      # Prepare the model arguments
      metab_args <- tryCatch(
        eval(parse(text=config[[row,'model_args']])), 
        error=function(e) e$message )
      if(!is.list(metab_args)) {
        out <- "error in evaluating metab_args"
        attr(out, "errors") <- if(is.character(metab_args)) metab_args else "could not parse or evaluate args"
        return(out)
      }
      # add the config row to the info arg
      if('info' %in% names(metab_args)) {
        metab_args[['info']] <- list(userinfo=metab_args[['info']], config=config[row,])
      } else {
        metab_args <- c(metab_args, list(info=list(config=config[row,])))
      }
      
      # Prepare the data, passing along any errors from config_to_data
      if(verbose) message("row ", row, ": preparing metab_data...")
      metab_data_list <- config_to_data(config[row,], row, metab_fun, on_error='quiet')
      metab_data_ok <- is.null(attr(metab_data_list, "errors"))
      if(!metab_data_ok) {
        out <- "error in data prep"
        attr(out, "errors") <- attr(metab_data_list, "errors")
        attr(out, "warnings") <- attr(metab_data_list, "warnings")
        return(out)
      } else {
        # if the data are valid, also remove units. eventually want to be able
        # to pass units to metab_fun.
        . <- '.dplyr.var'
        metab_data <- metab_data_list[['data']] # do this way for exact matching (to not confuse with data_daily)
        metab_data <- u(metab_data, get_units(metab_data) %>% { replace(., which(.=="mg L^-1"), "mgO2 L^-1") })
        metab_data <- v(metab_data)
        metab_data_daily <- metab_data_list$data_daily # OK b/c partial matching won't confuse w/ anything else
        if(!is.null(metab_data_daily)) {
          metab_data_daily <- v(metab_data_daily)
        }
      }
      message('adjusted units of metab_data')
    })
    
    # Add info on the time we took to prepare the args & data
    metab_args$info <- c(metab_args$info, list(prep_time=prep_time))

    # Short-circuit if prep_only
    if(isTRUE(prep_only)) {
      return(c(list(data=metab_data, data_daily=metab_data_daily), metab_args))
    }
    
    # Run the model
    if(verbose) message("row ", row, ": running metab_fun")
    fit <- tryCatch({
      metab_fun(specs=metab_args$specs, data=metab_data, data_daily=metab_data_daily, info=metab_args$info)
    },
    error=function(e) {
      out <- "error in model run"
      attr(out, "errors") <- as.character(e$message)
      attr(out, "error_object") <- e
      attr(out, "inputs") <- list(specs=metab_args$specs, data=metab_data, data_daily=metab_data_daily, info=metab_args$info)
      out
    })
    
    # If we've made it this far, the modeling ought to have worked
    return(fit)
  })    
    
  # Return metabolism predictions
  fits
}

