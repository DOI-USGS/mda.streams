#' Actually run the model as specified by the configuration arguments
#' 
#' @param config data.frame, or file/filename to read a data.frame from. As an 
#'   alternative to all preceding arguments, this single config argument may be 
#'   passed in. The data.frame columns must correspond precisely to the list of 
#'   preceding arguments.
#' @param rows missing, integer, or vector of integers. The row number[s] of the
#'   config data.frame to use for this particular model run.
#' @param verbose logical. Should status messages be given?
#' @import streamMetabolizer
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' config_to_metab(config=stage_metab_config(
#'   tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_04087142", filename=NULL))
#' }
config_to_metab <- function(config, rows, verbose=TRUE) {

  # Check the input
  if(!is.data.frame(config) && is.character(config)) {
    config <- read.table(config, sep="\t", header=TRUE, colClasses="character")
  }
  rows <- if(missing(rows)) 1:nrow(config) else rows
  verify_config(config[rows,], on_fail="stop")
  
  fits <- lapply(rows, function(row) {
    
    if(verbose) {
      message("modeling metab for config row ", row, ":")
      print(config[row,])
    }
    
    # Locate the model function
    metab_fun <- 
      tryCatch(
        # Look first in streamMetabolizer
        get(config[row,'model'], envir = environment(streamMetabolizer::metab_model)),
        error=function(e) {
          # Look second in the current and inherited environments. If it's not
          # there, either, we really do want to throw an error.
          get(config[row,'model'])
        }
      )
    # Prepare the model arguments
    metab_args <- eval(parse(text=config[row,'model_args']))
    
    # Prepare the data
    metab_data <- 
      tryCatch({
        metab_data <- config_to_data(config[row,], row, metab_fun, metab_args)
        metab_data <- v(metab_data) # eventually want to hot have to do this
        metab_data[complete.cases(metab_data),]
      },
      error=function(e) {
        message("failed to prepare data in row ", row, ":")
        message(as.character(e))
        NA
      })
    
    # Run the model
    fit <- 
      tryCatch({
        do.call(metab_fun, c(list(data=metab_data), metab_args))
      },
      error=function(e) {
        message("the metabolism model failed in row ", row, ":")
        message(as.character(e))
        NA
      })
    
    return(fit)
  })    
    
  # Return metabolism predictions
  fits
}

