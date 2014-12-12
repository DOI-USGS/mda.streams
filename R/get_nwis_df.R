#'@title get nwis data as data.frame
#'@description get data from nwis and return as create data.frame
#'
#'@param site valid NWIS site ID
#'@param variable_name short name of variable \code{\link{get_ts_variables}}
#'@param p_code NWIS parameter code
#'@param ... additional arguments passed to \code{\link{readNWISuv}}
#'@return a data.frame of timeseries data from NWIS, or NULL if no data exist
#'@import dataRetrieval
#'
#'@examples
#'\dontrun{
#'#'df <- get_nwis_df(site = "06893820", variable_name = "doobs", p_code = "00300", 
#'                  startDate = '2014-01-01', endDate = '2014-02-01')
#'df <- get_nwis_df(site = "nwis_06893820", variable_name = "doobs", p_code = "00300", 
#'                  startDate = '2014-01-01', endDate = '2014-02-01')
#'}
#'@export
get_nwis_df <- function(site, variable_name, p_code, ...){
  
  #if nwis_site, split and use "site"
  
  site <- split_site(site)
  
  nwis_data <- dataRetrieval::readNWISuv(siteNumbers = site, parameterCd = p_code, ...)
  
  if (is.na(nwis_data[1])[1]){
    # bad pcode or site or no data
    return(NULL)
  } else {
    
    ts_name <- paste('ts', variable_name, sep = '_')
    nwis_df <- data.frame('DateTime' = nwis_data$dateTime, 
                          ts_name = as.numeric(nwis_data[, ncol(nwis_data)]))
    names(nwis_df) <- c('DateTime', ts_name)
    
    return(nwis_df)
  }
  
}
split_site <- function(site){
  
  site <- tail(strsplit(x = site, split = '_')[[1]], 1)
  
  return(site)
}