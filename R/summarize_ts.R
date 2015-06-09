#' Summarize a timeseries
#' 
#' @param var_src character vector of dataset types to summarize.
#' @param site_name character vector of the same length as var_src specifying the 
#'   site or sites (e.g., "nwis_24309857") where each timeseries dataset should 
#'   be sought.
#' @param ... other args passed to \code{\link[sbtools]{session_check_reauth}}
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' summarize_ts("doobs_nwis", "nwis_01021050")
#' }
summarize_ts <- function(var_src, site_name, ...) {
  # collect the inputs and determine whether each row describes an existing ts
  ts_summary <- 
    data.frame(var_src=var_src, site=site_name, stringsAsFactors=FALSE) %>%
    mutate(id=locate_ts(var_src=var_src, site_name=site_name))
  
  # create a data.frame of summary metrics, one per row
  ts_summary <- bind_rows(lapply(seq_len(nrow(ts_summary)), function(site_row) {
    one_ts <- ts_summary[site_row,]
    if(!is.na(one_ts$id)) {
      ts <- read_ts(download_ts(one_ts$site, one_ts$var_src, on_exists="replace"))
      data.frame(
        one_ts,
        start_date = min(ts$DateTime),
        end_date = max(ts$DateTime),
        num_rows = nrow(ts),
        num_complete = length(which(!is.na(ts[,2])))
      )
    } else {
      data.frame(
        one_ts,
        start_date = NA,
        end_date = NA,
        num_rows = NA,
        num_complete = NA
      )
    }
  }))
  
  # return
  ts_summary
}