#' Higher-level site querying function
#' 
#' @param with_timeseries character vector of timeseries variables (i.e., 1+ of 
#'   those listed in get_ts_variables())
#' @param logic how to join the constraints in with_timeseries, ...: is any of 
#'   the listed parameters sufficient, or do you need all of them to be 
#'   available for a site to qualify?
#' @return a character vector of site IDs
#' @export
#' @examples 
#' \dontrun{
#'   find_sites()
#'   find_sites(with_timeseries=c("wtr","doobs"), logic="any")
#' }
find_sites <- function(with_timeseries = NULL, logic=c("all","any"), ...) {
  
  logic <- match.arg(logic)
  
  if(is.null(with_timeseries)){
    sites <- get_sites()
  } else {
    types <- make_ts_name(variable = with_timeseries)
    sites <- vector('character')
    for (k in 1:length(types)){
      sites <- append(sites, get_sites(with_child_key = types[k]))
    }
    tbl_sites <- data.frame(table(sites))
    
    sites <- as.character(tbl_sites$sites[
      switch(
        logic,
        "all" = tbl_sites$Freq == length(types), # if any, sites must be repeated as many times as the number of types used
        "any" = TRUE # just need them to show up once
      )])
  }
  
  return(sites)
}