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
#'@importFrom unitted u
#'
#'@examples
#'\dontrun{
#'file <- stage_nldas_ts(sites = c("nwis_06893820","nwis_01484680"), variable_name = "baro", p_code = "pressfc", 
#'                 times = c('2014-01-01','2014-02-01'))
#'}
#'@export
stage_nldas_ts <- function(sites, variable_name, p_code, times, ...){
  
  if (length(p_code) > 1) 
    stop ('p_code must be single value.')
  
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
  data_out <- loadOutput(job, with.units = TRUE)
  
  # -- coerce into input format for write_ts here --
  #parsing      DateTime nwis_01484680 nwis_06893820 variable statistic    units
  #1  2011-01-01         88.26         19.34      ppt      MEAN mm/month
  #2  2011-02-01         42.07         63.05      ppt      MEAN mm/month
  #3  2011-03-01        108.60         47.59      ppt      MEAN mm/month
  file_handles <- c()
  for (i in 1:length(sites)){
    site_data <- select(data_out, DateTime, matches(sites[i]), variable, units) %>%
      filter(variable == p_code) %>%
      select(-variable)
    
    units <- unique(site_data$units)
    site_data <- select(site_data, -units)
    names(site_data) <- c('DateTime',p_code)
    site_data <- u(site_data, c(NA, units))
    file_handle <- sprintf('%s_%s.tsv',site,ts_name)
    write_unitted(site_data, file = file_handle, ...)
    file_handles <- c(file_handles, file_handle)
    #file_handle <- write.unitted(site_df)
    #file_handles <- c(file_handles, file_handle)
  }
  return(file_handles)
}