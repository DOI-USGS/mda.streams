#'@title stage ndlas data into a time series file
#'@description get data from nldas and return a file handle
#'
#'@param sites a character vector of valid NWIS site IDs
#'@param variable short name of variable \code{\link{get_ts_variables}}
#'@param times a length 2 vector of POSIXct dates
#'@param folder a folder to place the file outputs in (defaults to temp directory)
#'@param verbose provide verbose output (currently not implemented)
#'@param ... additional arguments passed to \code{\link[geoknife]{geoknife}} and \code{\link[unitted]{write_unitted}}
#'@return a file handle for time series file created 
#'@importFrom geoknife simplegeom webdata geoknife loadOutput webprocess
#'@importFrom dataRetrieval readNWISsite
#'@importFrom unitted u write_unitted
#'
#'@examples
#'\dontrun{
#'files <- stage_nldas_ts(sites = c("nwis_06893820","nwis_01484680"), variable = "baro", 
#'                 times = c('2014-01-01 00:00','2014-01-01 05:00'))
#'}
#'@export
stage_nldas_ts <- function(sites, variable, times, folder = tempdir(), verbose = FALSE, ...){
  
  if (length(variable) > 1) 
    stop ('variable must be single value.')
  
  nwis_sites <- parse_site_name(sites)
  site_data <- readNWISsite(nwis_sites)
  
  lon_lat <- matrix(data = NA, ncol = length(sites), nrow = 2)
  site_no <- dec_lat_va <- dec_long_va <- ".dplyr.var"
  for (i in 1:length(sites)){
    location <- filter(site_data, site_no == nwis_sites[i]) %>%
      select(dec_lat_va, dec_long_va) %>% 
      summarize(lon = mean(dec_long_va, na.rm = T), lat = mean(dec_lat_va, na.rm = T))
    lon_lat[1:2, i]  = as.numeric(location)

  }
  rmv_sites <- is.na(lon_lat[1,]) | is.na(lon_lat[2,])
  lon_lat_df <- as.data.frame(lon_lat[, !rmv_sites])
  names(lon_lat_df) <- sites[!rmv_sites]
  
  p_code <- get_var_codes(variable, "p_code")
  
  stencil <- simplegeom(lon_lat_df)
  fabric <- webdata('nldas', variables = p_code, times = times)
  
  knife <- webprocess(wait = TRUE)
  knife@processInputs$REQUIRE_FULL_COVERAGE = 'false'
  data_out <- geoknife(stencil, fabric, knife, ...) %>%
    loadOutput(with.units = TRUE)
  
  file_paths <- c()
  DateTime <- matches <- ".dplyr.var"
  for (i in 1:length(sites)){
    site_data <- select(data_out, DateTime, matches(sites[i]), variable, units) %>%
      filter(variable == p_code) %>%
      select(-variable)
    
    units <- as.character(site_data$units) %>% unique()
    
    site_data <- select(site_data, -units) %>%
      setNames(c('DateTime',variable)) %>%
      u(c("UTC", units))
    
    fpath <- write_ts(site_data, sites[i], variable, folder)
    file_paths <- c(file_paths, fpath)
  }
  return(file_paths)
}