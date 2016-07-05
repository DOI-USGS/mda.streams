#' Find a site folder on ScienceBase
#' 
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want
#' @inheritParams locate_item
#' @import sbtools
#' @import dplyr
#' @import tibble
#' @export
#' @examples 
#' \dontrun{
#' locate_site("nwis_02322688", format="url")
#' locate_site(c("nwis_02322688", "nwis_03259813", "nwis_04024000"))
#' locate_site("nwis_notasite", format="url")
#' testthat::expect_error(locate_site("notasite", format="url"))
#' }
locate_site <- function(site_name, format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  site_name <- do.call(make_site_name, parse_site_name(site_name, out=c("sitenum","database"), use_names=FALSE)) # check the site name
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="folder_url")
  if(length(site_name) > 5 & by %in% c('tag')) {
    site_items <- sbtools::query_item_identifier(scheme=get_scheme(), type='site_root', limit=10000)
    site_df <- bind_rows(lapply(site_items, function(item) tibble(title=item$title, id=item$id)))
    matches <- site_df[match(site_name, site_df$title),]
    format_item(matches, format=format, browser=browser)
  } else {
    locate_item(key=site_name, type="site_root", parent=locate_folder("sites", by="tag"), title=site_name, by=by, format=format, limit=limit, browser=browser)
  }
}