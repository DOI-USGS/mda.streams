#' Delete a time series item and its data
#' 
#' Deletes timeseries objects specified by all combinations of variable and site
#' 
#' @inheritParams locate_ts
#' @param files_only logical. If TRUE, only the files will be deleted, leaving
#'   an empty ts item
#' @param verbose logical. Should status messages be given?
#' @keywords internal
#' @examples 
#' \dontrun{
#' login_sb()
#' set_scheme("mda_streams_dev")
#' 
#' sites <- c("nwis_05406479", "nwis_05435950", "nwis_04087119")
#' files <- stage_nwis_ts(sites = sites, var = "doobs", 
#'   times = c('2014-01-01 00:00','2014-01-01 05:00'))
#' post_ts(files, on_exists='skip')
#' locate_ts("doobs_nwis", sites) # find the newly posted data online
#' sapply(locate_ts("doobs_nwis", sites), function(tsid) sbtools::item_list_files(tsid)$fname)
#' 
#' # items should all still be there, but with no files
#' mda.streams:::delete_ts("doobs_nwis", sites, files_only=TRUE)
#' locate_ts("doobs_nwis", sites, by="either") 
#' sapply(locate_ts("doobs_nwis", sites), function(tsid) sbtools::item_list_files(tsid)$fname)
#' 
#' # everything should disappear
#' post_ts(files, on_exists='replace')
#' mda.streams:::delete_ts("doobs_nwis", sites)
#' locate_ts("doobs_nwis", sites, by="either")
#'  
#' set_scheme("mda_streams")
#' }
delete_ts <- function(var_src, site_name, files_only=FALSE, verbose=TRUE) {
  
  # combine var_src and site_name data.frame style, then find item ids
  query_args <- data.frame(
    var_src=var_src, site_name=site_name, ts_name=paste(var_src, site_name, sep="-"),
    ts_id=locate_ts(var_src=var_src, site_name=site_name, by="either"), 
    stringsAsFactors=FALSE)
  
  delete_item(item_ids=query_args$ts_id, item_names=query_args$ts_name, 
              delete_files=files_only, delete_children=FALSE, delete_item=!files_only, verbose=verbose)
}
