#'@title stage ndlas data into a time series file
#'@description get data from nldas and return a file handle
#'
#'@param sites a character vector of valid NWIS site IDs
#'@param variable_name short name of variable \code{\link{get_ts_variables}}
#'@param p_code NLDAS variable name (see \code{\link[geoknife]{variables}})
#'@param times a length 2 vector of POSIXct dates
#'@param ... additional arguments passed to \code{\link[geoknife]{geoknife}}
#'@return a file handle for time series file created 
#'@importFrom geoknife simplegeom webdata geoknife loadOutput
#'@importFrom dataRetrieval readNWISsite
#'
#'@examples
#'\dontrun{
#'file <- stage_nldas_ts(sites = c("nwis_06893820","nwis_01484680"), variable_name = "baro", p_code = "pressfc", 
#'                 times = c('2014-01-01','2014-02-01')
#'}
#'@export
stage_nldas_ts <- function(sites, variable_name, p_code, start_date, end_date){
  
  nwis_sites <- split_site(sites)
  site_data <- readNWISsite(nwis_sites)
  
  lon_lat <- matrix(data = NA, ncol = length(sites), nrow = 2)
  for (i in 1:length(sites)){
    lon_lat[1, i] <- site_data$dec_long_va[site_data$site_no == nwis_sites[i]]
    lon_lat[2, i] <- site_data$dec_lat_va[site_data$site_no == nwis_sites[i]]
  }
  lon_lat_df <- as.data.frame(lon_lat)
  names(lon_lat_df) <- sites
  
  
  stencil <- simplegeom(lon_lat_df)
  fabric <- webdata('nldas', variables = p_code, times = times)
  
  job <- geoknife(stencil, fabric, wait = TRUE, ...)
  data_out <- loadOutput(job)
  
  # -- coerce into input format for write_ts here --
  
  return(write_ts(data_out))
}