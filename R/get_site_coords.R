#' Pull the latitude and longitude of a site from ScienceBase
#' 
#' @import dplyr
#' @importFrom dataRetrieval readNWISsite
#' @importFrom stats setNames
#' @param site_names a list of site names such as those returned from 
#'   make_site_name()
#' @param format character specifying the desired output format. normal is a 
#'   human-readable, dplyr-friendly format. geoknife works well for geoknife and
#'   excludes sites with incomplete coordinate data.
#' @param on_missing character specifying how to treat missing sites. Use "NA" 
#'   to include missing sites in the output but with NA for both lon and lat. 
#'   Use "omit" to omit those sites from the output.
#' @param out vector of column names to include in the output; only honored if
#'   format=='normal'
#' @param use_basedon logical. If TRUE, and if there are sites among site_names 
#'   that have NA in their lat or lon fields but also have a character value in 
#'   their styx.basedon metadata field, then lat and lon will be pulled from the
#'   site named in styx.basedon. use_basedon=TRUE is appropriate primarily for 
#'   styx sites (those with simulated data, whose initial data are based on some
#'   real site) and so use_basedon=FALSE by default.
#' @param attach.units logical. Should units be attached?
#' @return a data.frame of site names and coordinates, with units if 
#'   attach.units=TRUE, or a matrix if format='geoknife'
#' @export
#' @examples 
#' get_site_coords(c("nwis_01467200","styx_001001","nwis_351111089512501","nwis_07239450")) 
#' get_site_coords(c("styx_001001","nwis_07239450"), use_basedon=TRUE) 
#' get_site_coords(c("nwis_01467200","nwis_09327000","nwis_351111089512501",
#'     "styx_000001001"), format="geoknife")
get_site_coords <- function(site_names, format=c("normal","geoknife"), on_missing=c("NA","omit"), out=c('site_name','lat','lon'), use_basedon=FALSE, attach.units=(format=="normal")) {

  format <- match.arg(format)
  on_missing <- match.arg(on_missing)
    
  # retrieve the coordinates from meta_basic
  basic_meta <- get_meta('basic', out=union(c('site_name','lat','lon'), out))
  ll_row <- match(site_names, basic_meta$site_name)
  if(length(na_rows <- which(is.na(ll_row))) > 0) {
    missing_site_info <- paste0(" unrecognized site_name[s]: ", paste0(site_names[na_rows], collapse=", "))
    if(on_missing == "omit") {
      ll_row <- ll_row[!is.na(ll_row)]
      site_names <- site_names[site_names %in% basic_meta$site_name]
      on_missing_info <- "omitting"
    } else {
      # already does the right thing for on_missing="NA"
      on_missing_info <- "returning NAs for"
    }
    warning(on_missing_info, missing_site_info)
  }
  lon_lat <- basic_meta[ll_row,]
  #lon_lat$site_name <- site_names
  
  # patch NAs for sites with proxies (basedons) for sites where ALL values 
  # besides site_name are NA and where a proxy is available
  if(use_basedon) {
    na_lat_lons <- which(sapply(1:nrow(lon_lat), function(i) all(is.na(lon_lat[i, setdiff(out, 'site_name')]))))
    if(length(na_lat_lons) > 0) {
      styx_meta <- get_meta("styx")
      sites_w_proxies <- match(lon_lat$site_name[na_lat_lons], styx_meta$site_name)
      proxies <- styx_meta[sites_w_proxies, 'styx.basedon']
      non_site_cols <- setdiff(out, 'site_name')
      lon_lat[na_lat_lons, non_site_cols] <- basic_meta[match(proxies, basic_meta$site_name), non_site_cols]
    }
  }
  
  if(format=="geoknife") {
    lon_lat <- v(lon_lat)[c("lon","lat")] %>%
      t() %>% as.data.frame() %>% setNames(lon_lat$site_name)
    if(attach.units) stop("sorry - can't attach units to geoknife format")
  } else {
    # if not geoknife foramt, honor the selection for 'out'
    lon_lat <- lon_lat[,out]
  }
  
  if(!attach.units) {
    lon_lat <- v(lon_lat)
  }
  return(lon_lat)
}
