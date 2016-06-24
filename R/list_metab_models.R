#' List the available metab_model objects
#' 
#' @param text if specified, the query only returns metab_models whose text (or 
#'   description, if available) matches the word[s] in \code{text}. Note that 
#'   partial words are not matched -- e.g., text='nwis_0138' will not match
#'   models whose title includes 'nwis_01388000'
#' @param order_by character vector of aspects of the model names to sort on. 
#'   Options are the same as those in the \code{out} argument to 
#'   \code{\link{parse_metab_model_name}}
#' @return a character vector of titles of the metab_model .RData files posted 
#'   on SB
#' @import sbtools
#' @import dplyr
#' @export
#' @examples 
#' \dontrun{
#' mms <- list_metab_models('0.0.18')
#' }
list_metab_models = function(text, order_by=c("date","tag","row","site","strategy","title")) {
  
  order_by <- match.arg(order_by, several.ok = TRUE)
  sb_require_login("stop")
  
  # get list of model items
  model_items <- 
    if(missing(text)) {
      query_item_identifier(scheme = get_scheme(), type = 'metab_model', limit=10000) 
    } else {
      query_item_in_folder(text=text, folder=locate_folder('metab_models'), limit=10000) 
    }
  model_titles <- sapply(model_items, function(item) item$title)
  
  if(length(model_titles) > 0) {
    # check unique vs total in case an old SB bug comes back (there was
    # duplication & omission of items when paging through many results)
    unique_model_titles <- unique(model_titles)
    if(length(unique_model_titles) != length(model_titles)) warning("failed to retrieve all metab models; a retry might work")
    # return
    return(unique_model_titles[do.call(order, as.list(parse_metab_model_name(unique_model_titles))[order_by])])
  } else {
    return(character())
  }
}
