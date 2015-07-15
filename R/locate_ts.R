#' Find a timeseries item on ScienceBase
#' 
#' @param var_src the variable name, e.g., "doobs_nwis", for which you want the 
#'   timeseries
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want to
#'   look in
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_ts(c("doobs","wtr","disch"), "nwis_02322688")
#' locate_ts("doobs", "nwis_02322688", format="url")
#' locate_ts("doobs", "nwis_notasite", format="url")
#' testthat::expect_error(locate_ts("notavar", "nwis_notasite"))
#' }
locate_ts <- function(var_src="doobs_nwis", site_name="nwis_02322688", format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  var_src <- make_ts_name(var_src) # check that the variable name is valid and add prefix
  site_name <- do.call(make_site_name, parse_site_name(site_name, out=c("sitenum","database"), use_names=FALSE)) # check the site name
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="item_url")
  if(by %in% c("dir","either")) {
    site_parent <- locate_site(site_name, by="either")
    if(any(na_site <- is.na(site_parent))) stop("couldn't find the site folder[s] ", paste0(site_name[na_site], collapse=", "))
  } else {
    site_parent <- NA
  }
  locate_item(key=site_name, type=var_src, parent=site_parent, title=var_src, by=by, format=format, limit=limit, browser=browser)
}