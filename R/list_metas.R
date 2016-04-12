#' List the available metadata tables
#' 
#' @return a character vector of titles of the meta tables posted on SB
#' @import sbtools
#' @export
list_metas = function() {
  # get list of site items, then filter to those of the proper data_type
  meta_items <- query_item_identifier(scheme = get_scheme(), type = 'sites_meta', limit = 10000)
  if(length(meta_items) > 0) {
    return(sort(sapply(meta_items, function(item) substring(item$title, 6))))
  } else {
    return(character())
  }
}
