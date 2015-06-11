#' Summarize a timeseries
#' 
#' @param var_src character vector of dataset types to summarize.
#' @param site_name character vector of the same length as var_src specifying
#'   the site or sites (e.g., "nwis_24309857") where each timeseries dataset
#'   should be sought.
#' @param out a list of one or more outputs to include in the summary dataframe,
#'   in addition to the var_src and site_name and id columns.
#' @param ... other args passed to \code{\link[sbtools]{session_check_reauth}}
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' summarize_ts(rep(c("doobs_nwis", "wtr_nwis"), each=4), 
#'   rep(c("nwis_01021050","nwis_01036390","nwis_01073389","nwis_notasite"), times=2))
#' }
summarize_ts <- function(var_src, site_name, out=c("start_date","end_date","num_rows","num_complete"), ...) {
  
  # check inputs & session
  out <- match.arg(out, several.ok=TRUE)
  session_check_reauth(...)
  
  # collect the vector inputs. as a side benefit, this will throw an error if
  # var_src and site_name have incompatible dimensions
  ts_summary <- data.frame(var_src=var_src, site=site_name, stringsAsFactors=FALSE)
  
  # download the files all at once, slightly reducing the number of requests to SB
  ts_files <- download_ts(var_src, site_name, on_remote_missing="return_NA", on_local_exists="replace")
  
  # define the functions that might be applied to each ts data.frame, then
  # select only those that have been requested
  summary_funs <- list(
    start_date = function(ts) min(ts$DateTime),
    end_date = function(ts) max(ts$DateTime),
    num_rows = function(ts) nrow(ts),
    num_complete = function(ts) length(which(!is.na(ts[,2])))
  )[out]
  
  # create a data.frame of summary metrics, one ts per row. in each row only 
  # compute the selected summary metrics, and only compute them if the file is
  # available
  ts_summary <- bind_rows(lapply(seq_len(nrow(ts_summary)), function(site_row) {
    ts_file <- ts_files[site_row]
    if(!is.na(ts_file)) {
      ts <- read_ts(ts_file)
      as.data.frame(c(
        as.list(ts_summary[site_row,]),
        lapply(summary_funs, function(fun) { fun(ts) })
      ), stringsAsFactors=FALSE)
    } else {
      as.data.frame(c(
        as.list(ts_summary[site_row,]),
        lapply(summary_funs, function(fun) { NA })
      ), stringsAsFactors=FALSE)
    }
  }))
  
  # return
  ts_summary
}