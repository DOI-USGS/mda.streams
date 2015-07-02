#' Actually run the model as specified by the configuration arguments
#' 
#' @param config data.frame, or file/filename to read a data.frame from. As an 
#'   alternative to all preceding arguments, this single config argument may be 
#'   passed in. The data.frame columns must correspond precisely to the list of 
#'   preceding arguments.
#' @param rows missing, integer, or vector of integers. The row number[s] of the
#'   config data.frame to use for this particular model run.
#' @import streamMetabolizer
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' config_to_metab(config=stage_metab_config(
#'   tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_04087142", filename=NULL))
#' }
config_to_metab <- function(config, rows) {

  # Check the input
  rows <- if(missing(rows)) 1:nrow(config) else rows
  verify_config(config[rows,], on_fail="stop")
  
  fits <- lapply(rows, function(row) {
    
    # Locate the model function
    tryCatch(
      # Look first in streamMetabolizer
      metab_fun <- get(config[row,'model'], envir = environment(streamMetabolizer::metab_model)),
      error=function(e) {
        # Look second in the current and inherited environments. If it's not
        # there, either, we really do want to throw an error.
        metab_fun <- get(config[row,'model'])
      }
    )
    
    # Prepare the model arguments
    metab_args <- eval(parse(text=config[row,'model_args']))
    
    # Prepare the data
    metab_data <- config_to_data(config[row,], row, metab_fun, metab_args)
    
    # Run the model
    #metab_fun()
  })    
    
  # Return metabolism predictions
  fits
}

