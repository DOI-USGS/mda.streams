#' List the timeseries data available for a particular site
#' 
#' @inheritParams list_datasets
#' @inheritParams ts_has_file
#' @return a character vector of timeseries names, or NULL if none exists
#' @seealso \code{\link{get_ts}} \code{\link{locate_ts}} \code{\link{list_sites}}
#' @examples
#' list_tses(site = "nwis_50231500")
#' @export
list_tses = function(site_name, with_ts_version='rds', with_ts_archived=FALSE, limit=10000) {
  list_datasets(site_name=site_name, data_type='ts', with_ts_version=with_ts_version, with_ts_archived=with_ts_archived)
}