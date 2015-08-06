#' Find a metadata object on ScienceBase
#' 
#' @param type the type of metadata you want
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_meta('basic')
#' }
locate_meta <- function(type, format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  meta_type <- paste0("meta_",type)
  meta_folder <- locate_folder("sites_meta", by="tag")
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="item_url")
  locate_item(key=meta_type, type="sites_meta", parent=meta_folder, title=meta_type, by=by, format=format, limit=limit, browser=browser)
}