#' Pull the latitude and longitude of a site from ScienceBase
#' 
#' @import dplyr
#' @importFrom dataRetrieval readNWISsite
#' @param site_names a list of site names such as those returned from 
#'   make_site_name()
#' @param format character specifying the desired output format. normal is a 
#'   human-readable, dplyr-friendly format. geoknife works well for geoknife and
#'   excludes sites with incomplete coordinate data.
#' @param attach.units logical. Should units be attached?
#' @return a data.frame of
#' @export
#' @examples 
#' find_site_coords(c("nwis_01467200","styx_001001","nwis_351111089512501")) 
#' find_site_coords(c("nwis_01467200","nwis_09327000","nwis_351111089512501",
#'     "styx_0001001"), format="geoknife")
find_site_coords <- function(site_names, format=c("normal","geoknife"), attach.units=(format=="normal")) {

  format <- match.arg(format)
    
  # retrieve the coordinates from meta_basic
  . <- '.dplyr.var'
  lon_lat <- get_meta('basic', out=c('site_name','lat','lon')) %>% 
    .[match(site_names, .$site_name),] %>% mutate(site_name=site_names)
  
  if(format=="geoknife") {
    lon_lat <- v(lon_lat)[c("lon","lat")] %>%
      t() %>% as.data.frame() %>% setNames(lon_lat$site_name)
    if(attach.units) stop("sorry - can't attach units to geoknife format")
  }
  
  if(!attach.units) {
    lon_lat <- v(lon_lat)
  }
  return(lon_lat)
}
