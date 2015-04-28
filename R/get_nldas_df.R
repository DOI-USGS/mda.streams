#'@title get nwis data as data.frame
#'@description get data from nwis and return as create data.frame
#'
#'@param site valid NWIS site ID
#'@param variable_name short name of variable \code{\link{get_ts_variables}}
#'@param das_code NLDAS variable name (see \code{\link[geoknife]{getDataIDs}})
#'@param ... additional arguments passed to geoknife, e.g., startDate
#'@return a data.frame of timeseries data from NWIS, or NULL if no data exist
#'@importFrom geoknife simplegeom webdata geoknife loadOutput
#'@importFrom dataRetrieval readNWISsite
#'
#'@examples
#'\dontrun{
#'df <- get_nldas_df(site = "06893820", variable_name = "baro", das_code = "pressfc", 
#'                  start_date = '2014-01-01', end_date = '2014-02-01')
#'}
#'@export
get_nldas_df <- function(site, variable_name, das_code, start_date, end_date){
  
  nwis_site <- split_site(site)
  site_data <- readNWISsite(nwis_site)
  lat <- site_data$dec_lat_va
  lon <- site_data$dec_long_va
  
  stencil <- simplegeom(c(lon, lat))
  fabric <- webdata(list(
    times = as.POSIXct(c(start_date,end_date)),
    url = 'dods://hydro1.sci.gsfc.nasa.gov:80/dods/NLDAS_FORA0125_H.002',
    variables = das_code))
  
  job <- geoknife(stencil, fabric, waitUntilFinished = TRUE)
  
  return(loadOutput(job))
}