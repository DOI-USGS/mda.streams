#' List the timeseries data available for a particular site
#' 
#' @param site_name a valid powstreams site name (see \link{list_sites})
#' @return a character vector of timeseries names, or NULL if none exists. 
#' @seealso \code{\link{download_ts}}
#' @examples
#' list_tses(site = "nwis_50231500")
#' @export
list_tses = function(site_name){
  list_datasets(site_name=site_name, data_type='ts')
}