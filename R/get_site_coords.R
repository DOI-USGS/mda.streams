#' Pull the latitude and longitude of a site from ScienceBase
#' 
#' @import dplyr
#' @importFrom dataRetrieval readNWISsite
#' @param site_names a list of site names such as those returned from 
#'   make_site_name()
#' @param format character specifying the desired output format. normal is a 
#'   human-readable, dplyr-friendly format. geoknife works well for geoknife and
#'   excludes sites with incomplete coordinate data.
#' @param on_missing character specifying how to treat missing sites. Use "NA" 
#'   to include missing sites in the output but with NA for both lon and lat. 
#'   Use "omit" to omit those sites from the output.
#' @param attach.units logical. Should units be attached?
#' @return a data.frame of
#' @export
#' @examples 
#' get_site_coords(c("nwis_01467200","styx_001001","nwis_351111089512501")) 
#' get_site_coords(c("nwis_01467200","nwis_09327000","nwis_351111089512501",
#'     "styx_0001001"), format="geoknife")
get_site_coords <- function(site_names, format=c("normal","geoknife"), on_missing=c("NA","omit"), attach.units=(format=="normal")) {

  format <- match.arg(format)
  on_missing <- match.arg(on_missing)
    
  # retrieve the coordinates from meta_basic
  lon_lat <- get_meta('basic', out=c('site_name','lat','lon'))
  ll_row <- match(site_names, lon_lat$site_name)
  if(length(na_rows <- which(is.na(ll_row))) > 0) {
    if(on_missing == "omit") {
      ll_row <- ll_row[!is.na(ll_row)]
      site_names <- site_names[site_names %in% lon_lat$site_name]
      on_missing_info <- "omitting"
    } else {
      # already does the right thing for on_missing="NA"
      on_missing_info <- "returning NAs for"
    }
    warning(on_missing_info, " unrecognized site_name[s]: ", paste0(site_names[na_rows], collapse=", "))
  }
  lon_lat <- lon_lat[ll_row,]
  lon_lat$site_name <- site_names
  
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
