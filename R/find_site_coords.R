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
#' # middle site has missing coords:
#' find_site_coords(c("nwis_01467200","nwis_09327000","nwis_351111089512501")) 
#' find_site_coords(c("nwis_01467200","nwis_09327000","nwis_351111089512501"), format="geoknife")
find_site_coords <- function(site_names, format=c("normal","geoknife"), attach.units=(format=="normal")) {

  format <- match.arg(format)
    
  nwis_sites <- parse_site_name(site_names)
  site_data <- readNWISsite(nwis_sites)
  
  # data.frame of longitude and latitude for each site, averaged if there's more than one record for a site
  . <- site_no <- dec_lat_va <- dec_long_va <- lat <- lon <- ".dplyr.var"
  lon_lat <- site_data %>% group_by(site_no) %>%
    summarize(lon = mean(dec_long_va, na.rm = T), lat = mean(dec_lat_va, na.rm = T)) %>%
    transmute(site_name=make_site_name(site_no), lat, lon) %>%
    as.data.frame() # see unitted issue #14 - data.frame will be better than tbl_df for units for now
 
  if(format=="geoknife") {
    lon_lat <- lon_lat[c("lon","lat")] %>%
      t() %>% as.data.frame() %>% setNames(lon_lat$site_name)
    if(attach.units) stop("sorry - can't attach units to geoknife format")
  }
  
  if(attach.units) {
    units <- c(site_name=NA, lat="degN", lon="degE")
    lon_lat <- u(lon_lat, units[match(names(lon_lat), names(units))])
  }
   
  lon_lat
}

