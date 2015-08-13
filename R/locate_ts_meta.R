#' Find a timeseries metadata object on ScienceBase
#' 
#' @param type the type of ts metadata you want (currently there's only var_src_codes)
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_ts_meta('basic')
#' }
locate_ts_meta <- function(type, format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  meta_type <- paste0("tsmeta_",type)
  sites_folder <- locate_folder("sites", by="tag")
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="item_url")
  locate_item(key=meta_type, type="ts_meta", parent=sites_folder, title=meta_type, by=by, format=format, limit=limit, browser=browser)
}