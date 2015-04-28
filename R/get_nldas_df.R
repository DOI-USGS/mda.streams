#'@title get nwis data as data.frame
#'@description get data from nwis and return as create data.frame
#'
#'@param site valid NWIS site ID
#'@param variable_name short name of variable \code{\link{get_ts_variables}}
#'@param das_code NLDAS variable name (see \code{\link[geoknife]{getDataIDs}})
#'@param ... additional arguments passed to geoknife, e.g., startDate
#'@return a data.frame of timeseries data from NWIS, or NULL if no data exist
#'@import geoknife
#'@importFrom dataRetrieval readNWISsite
#'
#'@examples
#'\dontrun{
#'#'df <- get_nldas_df(site = "06893820", variable_name = "baro", das_code = "pressfc", 
#'                  start_date = '2014-01-01', end_date = '2014-02-01')
#'}
#'
get_nldas_df <- function(site, variable_name, das_code, start_date, end_date){
  
  nwis_site <- split_site(site)
  site_data <- readNWISsite(nwis_site)
  lat <- site_data$dec_lat_va
  lon <- site_data$dec_long_va
  
  gk <- geoknife() # create geoknife object
  
  linearRing = bufferPoint(c(lon, lat))
  setFeature(gk) <- list(LinearRing=linearRing)
  

  setAlgorithm(gk) <- list('Area Grid Statistics (weighted)' = 'gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm')
  
  # set the post inputs for the processing dataset
  setProcessInputs(gk) <- list('DATASET_ID'=das_code,
                               'DATASET_URI'='dods://hydro1.sci.gsfc.nasa.gov:80/dods/NLDAS_FORA0125_H.002',
                               'TIME_START'= sprintf('%sT00:00:00Z', start_date),
                               'TIME_END'= sprintf('%sT00:00:00Z', end_date),
                               'DELIMITER'='TAB')
#   
#   # kick off your request
#   gk <- startProcess(gk)
#   repeat{
#     if (!is.null(status.gk$URL) | status.gk$status!=""){
#       break
#     }
#     cat('checking process...\n') Sys.sleep(10)
#     if (is.null(status.gk$URL)){
#       status.gk <- checkProcess(gk) }
#   }
#   if (status.gk$status=='Process successful'){ 
#     cat(paste(status.gk$status,
#     '\nDownload available at: ',status.gk$URL,sep='')) cat(status.gk$status)
#   } else { }
}