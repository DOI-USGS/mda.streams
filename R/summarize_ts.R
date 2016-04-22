#' Summarize a timeseries
#' 
#' @param var_src character vector of dataset types to summarize, or named list 
#'   of ts data.frames() with only the columns DateTime and the variable (where 
#'   list element names are var_srces).
#' @param site_name character vector of the same length as var_src specifying 
#'   the site or sites (e.g., "nwis_24309857") where each timeseries dataset 
#'   should be sought.
#' @param out a list of one or more outputs to include in the summary dataframe,
#'   in addition to the var_src and site_name and id columns.
#' @inheritParams download_ts
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' summarize_ts(rep(c("doobs_nwis", "wtr_nwis"), each=4), 
#'   rep(c("nwis_01021050","nwis_01036390","nwis_01073389","nwis_notasite"), times=2))
#' }
summarize_ts <- function(
  var_src, site_name, version=c('rds','tsv'),
  out=c("date_updated","start_date","end_date","num_dates","num_rows","num_complete","modal_timestep","num_modal_timesteps"),
  on_local_exists=c("skip","stop","replace")) {
  
  # check inputs & session
  version <- match.arg(version)
  out <- match.arg(out, several.ok=TRUE)
  on_local_exists <- match.arg(on_local_exists)
  var_src_char <- if(is.list(var_src)) names(var_src) else var_src
  
  # collect the vector inputs. as a side benefit, this will throw an error if
  # var_src and site_name have incompatible dimensions
  ts_summary <- data.frame(site=unname(site_name), var_src=unname(var_src_char), stringsAsFactors=FALSE)
  
  # define the functions that might be applied to each ts item, then select only
  # those that have been requested
  item_funs <- list(
    date_updated = function(item) item$provenance$lastUpdated
  )
  item_funs <- item_funs[out[out %in% names(item_funs)]]
  item_units <- c(date_updated=NA)[out[out %in% names(item_funs)]]
  
  # define the functions that might be applied to each ts data.frame, then
  # select only those that have been requested
  summary_funs <- list(
    start_date = function(ts) min(ts$DateTime),
    end_date = function(ts) max(ts$DateTime),
    num_dates = function(ts) length(unique(as.Date(ts$DateTime))),
    num_rows = function(ts) nrow(ts),
    num_complete = function(ts) length(which(!is.na(ts[,2]))),
    modal_timestep = function(ts) {
      if(nrow(ts)==1) {
        NA 
      } else {
        table_tsteps <- suppressWarnings(table(as.numeric(diff(v(ts$DateTime)), units="mins")))
        as.numeric(names(which.max(table_tsteps)))
      }
    },
    num_modal_timesteps = function(ts) {
      if(nrow(ts)==1) {
        NA 
      } else {
        table_tsteps <- suppressWarnings(table(diff(v(ts$DateTime))))
        table_tsteps[[which.max(table_tsteps)]]
      }
    }
  )
  summary_funs <- summary_funs[out[out %in% names(summary_funs)]]
  summary_units <- c(
    start_date=NA, end_date=NA, num_dates="dates", 
    num_rows="rows", num_complete="rows", 
    modal_timestep="mins", num_modal_timesteps="steps")[out[out %in% names(summary_funs)]]
  
  # locate the items if needed, or make a dummy list otherwise
  ts_items <- 
    if(length(item_funs) > 0) {
      locate_ts(var_src_char, site_name) %>%
        lapply(function(id) if(is.na(id)) NA else item_get(id))
    } else {
      rep(NA, nrow(ts_summary))
    }
  
  # locate the tses if needed, or make a dummy list otherwise
  tses <- 
    if(length(summary_funs) > 0) {
      if(is.list(var_src)) {
        var_src
      } else {
        # download the files all at once, slightly reducing the number of requests to SB. then read iteratively
        download_ts(var_src, site_name, version=version, on_remote_missing="return_NA", on_local_exists=on_local_exists) %>%
          lapply(function(ts_file) if(is.na(ts_file)) NA else read_ts(ts_file) )
      }
    } else {
      # in this case all we need to summarize is the SB item properties (not the
      # ts itself), so here create a dummy list that's NA if the item doesn't
      # exist and TRUE if the item exists so it'll pass the is.na test in the
      # following lapply loop
      rep(NA, nrow(ts_summary))
    }
    
  # create a data.frame of summary metrics, one ts per row. in each row only 
  # compute the selected summary metrics, and only compute them if the file is
  # available
  ts_summary <- bind_rows(lapply(seq_len(nrow(ts_summary)), function(site_row) {
    ts <- tses[[site_row]]
    item <- ts_items[[site_row]]
    as.data.frame(c(
      as.list(ts_summary[site_row,]),
      lapply(item_funs, function(fun) { if(is(item, 'sbitem')) fun(item) else NA }),
      lapply(summary_funs, function(fun) { if(is.data.frame(ts)) fun(ts) else NA })
    ), stringsAsFactors=FALSE)
  })) %>% as.data.frame() %>%
    u(c(site=NA, var_src=NA, item_units, summary_units))
  
  # return
  ts_summary
}