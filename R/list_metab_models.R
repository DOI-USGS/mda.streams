#' List the available metab_model objects
#' 
#' @param order_by character vector of aspects of the model names to sort on.
#'   Options are the same as those in the \code{out} argument to
#'   \code{\link{parse_metab_model_name}}
#' @return a character vector of titles of the metab_model .RData files posted
#'   on SB
#' @import sbtools
#' @export
list_metab_models = function(order_by=c("date","tag","row","site","strategy","title")) {
  
  order_by <- match.arg(order_by, several.ok = TRUE)
  if(is.null(current_session()) || !session_validate()) stop("need ScienceBase access; call login_sb() first")
  
  # get list of site items, then filter to those of the proper data_type
  model_query <- query_item_identifier(scheme = get_scheme(), type = 'metab_model', limit = 10000, pagesize = 500)$title
  
  if(length(model_query) > 0) {
    # check unique vs total in case an old SB bug comes back (there was
    # duplication & omission of items when paging through many results)
    model_items <- unique(model_query)
    if(length(model_items) != length(model_query)) warning("failed to retrieve all metab models; a retry might work")
    # return
    return(model_items[do.call(order, as.list(parse_metab_model_name(model_items))[order_by])])
  } else {
    return(character())
  }
}
