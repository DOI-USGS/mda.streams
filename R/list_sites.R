#' Get a list of the sites on ScienceBase
#' 
#' The with_var_src argument optionally limits the list to those sites that
#' contain specific timeseries variables.
#'  
#' @param with_var_src character vector of data variables (i.e., 1+ of those
#'   listed in get_var_src_codes(out='var_src'))
#' @param logic how to join the constraints in with_var_src, ...: is any of the
#'   listed parameters sufficient, or do you need all of them to be available
#'   for a site to qualify?
#' @inheritParams get_sites
#' @return a character vector of site IDs
#' @export
#' @examples 
#' \dontrun{
#' list_sites()
#' list_sites(with_var_src=c("wtr_nwis","doobs_nwis"), logic="any")
#' list_sites(with_var_src=c("wtr_nwis","doobs_nwis"), logic="any",
#'   with_ts_version=c('tsv'), with_ts_archived=TRUE, limit=10000)
#' list_sites(list("wtr_nwis",any=c("doobs_nwis","doobs_simCopy")), logic="all")
#' list_sites(list("wtr_nwis",any=c("doobs_nwis","doobs_simCopy"),
#'   any=list("disch_nwis", all=c("depth_calcDisch","stage_nwis"))), logic="all")
#' }
list_sites <- function(
  with_var_src = NULL, logic=c("all","any"), 
  with_ts_version='rds', with_ts_archived=FALSE, limit=10000) {
  
  # process args
  logic <- match.arg(logic)
  
  # 3 options: full list, recursive listing & combination, or base-case list
  # combination. options 2 & 3 use the specified logic for top-level combination
  if(is.null(with_var_src)){
    # if no criteria are specified, return all sites
    sites <- get_sites()
    return(sort(sites))
    
  } else if(is.list(with_var_src)) {
    if(length(with_var_src) == 0) stop("empty list not allowed in with_var_src")
    
    # call list_sites recursively on the list elements
    sites <- vector('character')
    for (k in 1:length(with_var_src)) {
      newlogic <- names(with_var_src)[k]
      if(newlogic=="") newlogic <- "all" # if unspecified, use the default
      sites <- append(sites, list_sites(
        with_var_src[[k]], logic=newlogic, 
        with_ts_version=with_ts_version, with_ts_archived=with_ts_archived, limit=limit))
    }
    
  } else {
    # check for valid var_src values; the next lines will break if with_var_src isn't valid
    verify_var_src(with_var_src, on_fail=stop)
    
    # convert with_var_src to a vector of dataset names
    data_codes <- get_var_src_codes(out = c('data_type','var_src'))
    data_types <- data_codes[match(with_var_src, data_codes$var_src),'data_type']
    data_names <- ifelse(data_types=="ts", make_ts_name(with_var_src), with_var_src)
    
    # get the sites meeting each criterion individually
    sites <- vector('character')
    for (k in 1:length(data_names)){
      sites <- append(sites, get_sites(
        with_dataset_name = data_names[[k]],
        with_ts_version=with_ts_version, with_ts_archived=with_ts_archived, limit=limit))
    }
  }

  # tally the sites and select according to logic
  tbl_sites <- data.frame(table(sites))
  sites <- as.character(tbl_sites$sites[
    switch(
      logic,
      "all" = tbl_sites$Freq == length(with_var_src), # if all, sites must be repeated as many times as the number of data_names used
      "any" = TRUE # if any, just need them to show up once
    )])
  
  return(sort(sites))
}