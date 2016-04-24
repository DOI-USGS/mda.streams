#' Find a timeseries item on ScienceBase
#' 
#' @param var_src the variable name, e.g., "doobs_nwis", for which you want the 
#'   timeseries
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want to
#'   look in
#' @inheritParams locate_item
#' @import sbtools
#' @export
#' @examples 
#' \dontrun{
#' locate_ts(c("doobs_nwis","baro_nldas"), "nwis_02322688")
#' locate_ts("doobs_nwis", c("nwis_01203000","nwis_01208990","nwis_01304057"))
#' locate_ts("doamp_calcDAmp", "nwis_02322688", format="url")
#' }
locate_ts <- function(var_src="doobs_nwis", site_name="nwis_02322688", format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  var_src <- make_ts_name(var_src) # check that the variable name is in a valid format and add prefix
  site_name <- do.call(make_site_name, parse_site_name(site_name, out=c("sitenum","database"), use_names=FALSE)) # check the site name
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="item_url")
  if(by %in% c("dir","either")) {
    site_parent <- locate_site(site_name, by="either")
    if(any(na_site <- is.na(site_parent))) stop("couldn't find the site folder[s] ", paste0(site_name[na_site], collapse=", "))
  } else {
    site_parent <- NA
  }
  if(by=='tag' && length(site_name) > 3 && length(var_src) == 1) {
    item_list <- query_item_identifier(scheme=get_scheme(), type=var_src, limit=10000)
    item_df <- bind_rows(lapply(item_list, function(item) data_frame(id=item$id, parentId=item$parentId)))
    site_ids <- locate_site(site_name, by='tag')
    matches <- item_df[match(site_ids, item_df$parentId),]
    format_item(matches, format=format, browser=browser)
  } else {
    locate_item(key=site_name, type=var_src, parent=site_parent, title=var_src, by=by, format=format, limit=limit, browser=browser)
  }
}