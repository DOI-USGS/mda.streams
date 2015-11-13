#' List the available metab_model objects
#' 
#' @return a character vector of titles of the metab_model .RData files posted on SB
#' @import sbtools
#' @export
list_metab_models = function(){
  
  if(is.null(current_session()) || !session_validate()) stop("need ScienceBase access; call login_sb() first")
  
  # get list of site items, then filter to those of the proper data_type
  model_items <- query_item_identifier(scheme = get_scheme(), type = 'metab_model', limit = 10000)
  
  if(nrow(model_items) > 0) {
    return(sort(model_items$title))
  } else {
    return(character())
  }
}
