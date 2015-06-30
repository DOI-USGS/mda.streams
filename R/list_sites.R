#' Higher-level site querying function
#' 
#' @param with_var_src character vector of data variables (i.e., 1+ of those
#'   listed in get_var_codes(out='var_src'))
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
  
  logic <- match.arg(logic)
  
  if(is.null(with_var_src)){
    sites <- get_sites()
  } else {
    var_codes <- get_var_codes(out = c('type','var_src'))
    types <- vector(length = length(with_var_src))
    for (i in 1:length(types)){
      types[i] <- ifelse(with_var_src[i] %in% var_codes$var_src && 
                           var_codes$type[which(var_codes$var_src %in% with_var_src[i])] == 'ts',
                         make_ts_name(with_var_src[i]), 
                         with_var_src[i])
    }
    
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