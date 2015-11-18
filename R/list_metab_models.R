#' List the available metab_model objects
#' 
#' @return a character vector of titles of the metab_model .RData files posted on SB
#' @import sbtools
#' @export
list_metab_models = function() {
  
  if(is.null(current_session()) || !session_validate()) stop("need ScienceBase access; call login_sb() first")
  
  # get list of site items, then filter to those of the proper data_type. the 
  # repetition is here to protect against a known bug in ScienceBase as of 
  # 11/17/15 where paging through results doesn't work perfectly. see
  # https://github.com/USGS-R/sbtools/issues/140
  model_queries <- lapply(c(700,900,800,1000), function(pgsize) {
    query_item_identifier(scheme = get_scheme(), type = 'metab_model', limit = 10000, pagesize = pgsize)$title
  })
  
  if(nrow(model_queries[[1]]) > 0) {
    model_items <- unique(do.call(c, model_queries))
    if(length(model_items) != length(model_queries[[1]])) warning("failed to retrieve all metab models; a retry might work")
    return(model_items)
  } else {
    return(character())
  }
}
