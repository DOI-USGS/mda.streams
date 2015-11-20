#' List the available metab_model objects
#' 
#' @return a character vector of titles of the metab_model .RData files posted on SB
#' @import sbtools
#' @export
list_metab_models = function() {
  
  if(is.null(current_session()) || !session_validate()) stop("need ScienceBase access; call login_sb() first")
  
  # get list of site items, then filter to those of the proper data_type
  model_query <- query_item_identifier(scheme = get_scheme(), type = 'metab_model', limit = 10000, pagesize = 500)$title
  
  if(length(model_query) > 0) {
    # check unique vs total in case an old SB bug comes back (there was
    # duplication & omission of items when paging through many results)
    model_items <- unique(model_query)
    if(length(model_items) != length(model_query)) warning("failed to retrieve all metab models; a retry might work")
    # return
    return(model_items)
  } else {
    return(character())
  }
}
