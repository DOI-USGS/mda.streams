#'@title get nwis data as data.frame
#'@description get data from nwis and return as create data.frame
#'
#'@param site valid NWIS site ID
#'@param variable_name short name of variable \code{\link{get_ts_variables}}
#'@param p_code NWIS parameter code
#'@param ... additional arguments passed to \code{\link{readNWISuv}}
#'@return a data.frame of timeseries data from NWIS, or empty if no data exist
#'@importFrom dataRetrieval readNWISuv
#'
#'@examples
#'\dontrun{
#'df <- get_nwis_df(site = "06893820", variable_name = "doobs", p_code = "00300", 
#'                  startDate = '2014-01-01', endDate = '2014-02-01')
#'df <- get_nwis_df(site = "nwis_06893820", variable_name = "doobs", p_code = "00300", 
#'                  startDate = '2014-01-01', endDate = '2014-02-01')
#'}
#'@export
get_nwis_df <- function(site, variable_name, p_code, ...){
  
  #if nwis_site, split and use "site"
  
  site <- split_site(site)
  
  nwis_data <- readNWISuv(siteNumbers = site, parameterCd = p_code, ...)
  

  ts_name <- make_ts_variable(variable_name)
  nwis_df <- data.frame('DateTime' = as.POSIXct(nwis_data$dateTime, tz = nwis_data$tz_cd),
                        ts_name = as.numeric(nwis_data[, ncol(nwis_data)]))
  names(nwis_df) <- c('DateTime', ts_name)

  return(nwis_df)
}

