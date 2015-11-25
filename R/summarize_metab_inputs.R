#' Summarize a timeseries
#' 
#' @param config a config file/data.frame from which datasets should be
#'   constructed for summarization.
#' @param rows missing, integer, or vector of integers. The row number[s] of the
#'   config data.frame to use for this particular data summary.
#' @param out a list of one or more outputs to include in the summary dataframe
#' @param verbose logical. supply status messages?
#' @import dplyr
#' @importFrom utils read.table
#' @importFrom stats setNames complete.cases
#' @export
summarize_metab_inputs <- function(
  config, rows, 
  out=c("site", "model", "tag", "strategy", "date", "start_date","end_date","true_start","true_end","num_dates","num_rows","num_complete","modal_timestep","num_modal_timesteps"), 
  verbose=TRUE) {
  
  # check inputs & session
  out <- match.arg(out, several.ok=TRUE)
  
  # pull data and args according to the config file
  prep_list <- config_to_metab(config=config, rows=rows, verbose=verbose, prep_only=TRUE)
  
  # define the units to be assigned after the lapply loop
  summary_units <- c(
    start_date="", end_date="", num_dates="dates", 
    num_rows="rows", num_complete="rows", 
    modal_timestep="mins", num_modal_timesteps="steps")
  summary_units <- summary_units[out[out %in% names(summary_units)]]
  config_units <- setNames(rep(NA, length(out[out %in% names(config)])), out[out %in% names(config)])
 
  # define the functions that might be applied to each metab_data, then
  # select only those that have been requested
  summary_funs <- list(
    true_start = function(ts) min(ts$local.time),
    true_end = function(ts) max(ts$local.time),
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
    
  # Summarize the metabolism data for each selected config row
  full_sumry <- lapply(rows, function(row) {
    
    if(verbose) message("row ", row, ": summarizing metab data for this config row")
    
    # Collect the data for this one row
    row_data <- prep_list[[row]]
    config_outs <- config[row, out[out %in% names(config)]]
    
    # Summarize the data. Create a data.frame row of summary metrics, one ts per
    # row. in each row only compute the selected summary metrics, and only 
    # compute them if the file is available
    metab_data_ok <- is.null(attr(row_data, "errors"))
    if(!metab_data_ok) {
      sumry <- as.data.frame(c(
        as.list(config_outs),
        list(config_row=row),
        lapply(summary_funs, function(fun) { NA }),
        list(data_errors=attr(row_data, "errors"), data_warnings=attr(row_data, "warnings"))
      ), stringsAsFactors=FALSE)
    } else {
      sumry <- as.data.frame(c(
        as.list(config_outs),
        list(config_row=row),
        lapply(summary_funs, function(fun) { fun(row_data$data) }),
        list(data_errors=NA, data_warnings=NA)
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