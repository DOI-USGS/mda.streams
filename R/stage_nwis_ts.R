#'@title stage nwis data into a file
#'@description get data from nwis and return created file handle
#'
#'@param sites a character vector of valid NWIS site IDs
#'@param variable short name of variable \code{\link{get_ts_variables}}
#'@param times a length 2 vector of POSIXct dates
#'@param folder a folder to place the file outputs in (defaults to temp directory)
#'@param verbose provide verbose output (currently not implemented)
#'@param ... additional arguments passed to \code{\link{readNWISuv}} and \code{\link[unitted]{write_unitted}}
#'@return a character vector of file handles
#'@importFrom dataRetrieval readNWISuv
#'@importFrom unitted u write_unitted
#'
#'@examples
#'
#'\dontrun{
#'files <- stage_nwis_ts(sites = c("nwis_06893820","nwis_01484680"), variable = "doobs",
#'                  times = c('2014-01-01','2014-02-01'))
#'                  
#'files <- stage_nwis_ts(sites = get_sites(), variable = "par",
#'                  times = c('2014-01-01', '2014-02-01'))
#'}
#'@export
stage_nwis_ts <- function(sites, variable, times, folder = tempdir(), verbose = FALSE, ...){
  
  site_no <- dateTime <- tz_cd <- DateTime <- matches <- ends_with <- ".dplyr.var"
  
  file_paths <- c()
  
  p_code <- get_var_codes(variable, "p_code")
  nwis_data <- readNWISuv(siteNumbers = parse_site_name(sites), parameterCd = p_code, startDate = times[1], endDate = times[2], ...)
  if (ncol(nwis_data)!=0){
    un_sites <- unique(sites)
    for (i in 1:length(un_sites)){
      
      site <- parse_site_name(un_sites[i])
      site_data <- filter(nwis_data, site_no == site) %>%
        mutate(DateTime = as.POSIXct(dateTime, tz = tz_cd)) %>%
        select(DateTime, matches(tail(names(nwis_data),1)), -ends_with("_cd")) %>%
        setNames(c("DateTime",variable)) %>%
        u(c('UTC',get_var_codes(variable, 'units')))

      fpath <- write_ts(site_data, un_sites[i], variable, folder)
      file_paths <- c(file_paths, fpath)
    }
  }

  return(file_paths)
}

