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
#'@importFrom unitted u
#'
#'@examples
#'\dontrun{
#'files <- stage_nwis_ts(sites = c("nwis_06893820","nwis_01484680"), variable_name = "doobs", p_code = "00300", 
#'                  times = c('2014-01-01','2014-02-01'))
#'files <- stage_nwis_ts(sites = "nwis_06893820", variable_name = "doobs", p_code = "00300", 
#'                  times = c('2014-01-01', '2014-02-01'))
#'}
#'@export
stage_nwis_ts <- function(sites, variable_name, p_code, times, ...){
  
  #if nwis_site, split and use "site"
  
  sites <- split_site(sites)
  
  file_handles <- c()
  ts_name <- make_ts_variable(variable_name)
  
  nwis_data <- readNWISuv(siteNumbers = sites, parameterCd = p_code, startDate = times[1], endDate = times[2], ...)
  if (ncol(nwis_data)!=0){
    un_sites <- unique(sites)
    for (i in 1:length(un_sites)){
      site <- un_sites[i]
      site_data <- filter(nwis_data, site_no == site) %>%
        mutate(DateTime = as.POSIXct(dateTime, tz = tz_cd)) %>%
        select(DateTime, matches(tail(names(nwis_data),1)), -ends_with("_cd"))
      
      names(site_data) <- c("DateTime",ts_name)
      site_data <- u(site_data,c(NA,'units'))
      file_handle <- sprintf('nwis_%s_%s.tsv',site,ts_name)
      write_unitted(site_data, file = file_handle, ...)
      file_handles <- c(file_handles, file_handle)
    }
  }

  return(file_handles)
}

