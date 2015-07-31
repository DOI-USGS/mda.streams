#' Get, and by default browse to, a URL to the site location on Google Maps
#' 
#' @param site_names a list of site names such as those returned from 
#'   make_site_name()
#' @param browser logical. Should the URL be opened in a browser?
#' @import httr
#' @export
#' @examples 
#' \dontrun{
#' cat(view_google_map("nwis_01484680"))
#' view_google_map(c("nwis_01467200","nwis_09327000","nwis_351111089512501"))
#' }
view_google_map <- function(site_names, browser=TRUE) {
  coords <- find_site_coords(site_names, format="normal", attach.units = FALSE)
  url <- setNames(
    ifelse(complete.cases(coords),
           paste0("https://www.google.com/maps/place//@", coords$lat, ",", coords$lon,",5z/data=!3m1!4b1!4m2!3m1!1s0x0:0x0"),
           NA), 
    site_names)
  if(browser) BROWSE(url)
  return(url)
}
