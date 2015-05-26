#'@title stage nwis data into a file
#'@description get data from nwis and return created file handle
#'
#'@param sites a character vector of valid NWIS site IDs
#'@param variable_name short name of variable \code{\link{get_ts_variables}}
#'@param p_code NWIS parameter code
#'@param times a length 2 vector of POSIXct dates
#'@param ... additional arguments passed to \code{\link{readNWISuv}}
#'@return a character vector of file handles
#'@importFrom dataRetrieval readNWISuv
#'
#'@examples
#'\dontrun{
#'df <- get_nwis_df(sites = c("nwis_06893820","nwis_01484680"), variable_name = "doobs", p_code = "00300", 
#'                  times = ('2014-01-01','2014-02-01'))
#'df <- get_nwis_df(site = "nwis_06893820", variable_name = "doobs", p_code = "00300", 
#'                  startDate = '2014-01-01', endDate = '2014-02-01')
#'}
#'@export
stage_nwis_ts <- function(sites, variable_name, p_code, ...){
  
  #if nwis_site, split and use "site"
  
  sites <- split_site(sites)
  
  nwis_data <- readNWISuv(siteNumbers = sites, parameterCd = p_code, ...)
  if (ncol(nwis_data)==0){
    return(nwis_data)
  }

  ts_name <- make_ts_variable(variable_name)
  nwis_df <- data.frame('DateTime' = as.POSIXct(nwis_data$dateTime, tz = nwis_data$tz_cd),
                        ts_name = as.numeric(nwis_data[, ncol(nwis_data)]))
  names(nwis_df) <- c('DateTime', ts_name)

  return(nwis_df)
}

