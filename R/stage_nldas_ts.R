#' @title stage nldas data into a time series file
#' @description get data from nldas and return a file handle
#'   
#' @param sites a character vector of valid NWIS site IDs
#' @param variable short name of variable \code{\link{get_ts_variables}}
#' @param times a length 2 vector of POSIXct dates
#' @param folder a folder to place the file outputs in (defaults to temp
#'   directory)
#' @param verbose logical. provide verbose output?
#' @param ... additional arguments passed to \code{\link[geoknife]{geoknife}}
#'   and \code{\link[unitted]{write_unitted}}
#' @return a file handle for time series file created
#' @importFrom geoknife simplegeom webdata geoknife loadOutput webprocess
#' @importFrom dataRetrieval readNWISsite
#' @importFrom unitted u write_unitted
#'   
#' @examples
#' \dontrun{
#' files <- stage_nldas_ts(sites = c("nwis_06893820","nwis_01484680"), variable = "baro", 
#'   times = c('2014-01-01 00:00','2014-01-01 05:00'), verbose = TRUE)
#' read_ts(files[1])
#' }
#' @export
stage_nldas_ts <- function(sites, variable, times, folder = tempdir(), verbose = FALSE, ...){
  
  if (length(variable) > 1) 
    stop ('variable must be single value.')
  
  # get site coordinates and format for geoknife
  lon_lat <- find_site_coords(sites, format="geoknife")
  lon_lat_df <- lon_lat[complete.cases(t(lon_lat))]
  
  p_code <- get_var_codes(variable, "p_code")
  
  stencil <- simplegeom(lon_lat_df)
  fabric <- webdata('nldas', variables = p_code, times = times)
  
  if(isTRUE(verbose)) message("Starting remote processing and data download")
  
  data_out <- geoknife(stencil, fabric, REQUIRE_FULL_COVERAGE = 'false', wait = TRUE) %>%
    loadOutput(with.units = TRUE)
  
  if(isTRUE(verbose)) message("Finished downloading data; now writing to file[s]")
  
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
    
    fpath <- write_ts(site_data, site=sites[i], var=variable, src="nldas", folder)
    file_paths <- c(file_paths, fpath)
  }
  return(file_paths)
}