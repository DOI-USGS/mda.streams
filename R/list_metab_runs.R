#' List the available metabolism runs
#' 
#' @return a character vector of titles of the metab_runs posted on SB
#' @import sbtools
#' @export
list_metab_runs = function(){
  
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before querying metab_runs")
  
  # get list of site items, then filter to those of the proper data_type
  run_items <- query_item_identifier(scheme = get_scheme(), type = 'metab_run', limit = 10000)

  if(nrow(run_items) > 0) {
    return(run_items$title)
  } else {
    return(character())
  }
}
