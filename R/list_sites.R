#' Higher-level site querying function
#' 
#' @param with_var_src character vector of data variables (i.e., 1+ of those
#'   listed in get_var_src_codes(out='var_src'))
#' @param logic how to join the constraints in with_var_src, ...: is any of the
#'   listed parameters sufficient, or do you need all of them to be available
#'   for a site to qualify?
#' @param ... additional querying arguments yet to be implemented
#' @return a character vector of site IDs
#' @export
#' @examples 
#' \dontrun{
#'   list_sites()
#'   list_sites(with_var_src=c("wtr_nwis","doobs_nwis","watershed"), logic="any")
#' }
list_sites <- function(with_var_src = NULL, logic=c("all","any"), ...) {
  
  # process args
  logic <- match.arg(logic)
  if(any(bad_var_src <- !(with_var_src %in% get_var_src_codes(out="var_src"))))
    stop("with_var_src=",paste0(with_var_src[bad_var_src],collapse=",")," not in get_var_src_codes(out='var_src')")
  
  if(is.null(with_var_src)){
    # if no criteria are specified, return all sites
    sites <- get_sites()
  } else {
    # convert with_var_src to a vector of dataset names
    data_codes <- get_var_src_codes(out = c('data_type','var_src'))
    data_types <- data_codes[match(with_var_src, data_codes$var_src),'data_type']
    data_names <- ifelse(data_types=="ts", make_ts_name(with_var_src), with_var_src)
    
    # get the sites meeting each criterion individually
    sites <- vector('character')
    for (k in 1:length(data_names)){
      sites <- append(sites, get_sites(with_dataset_name = data_names[k]))
    }
    tbl_sites <- data.frame(table(sites))
    
    sites <- as.character(tbl_sites$sites[
      switch(
        logic,
        "all" = tbl_sites$Freq == length(data_names), # if all, sites must be repeated as many times as the number of data_names used
        "any" = TRUE # if any, just need them to show up once
      )])
  }
  
  return(sites)
}