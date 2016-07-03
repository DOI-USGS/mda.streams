#' @title stage gldas data into a time series file
#' @description get data from nldas and return a file handle
#'   
#' @param sites a character vector of valid NWIS site IDs
#' @param var short name of var as in
#'   \code{unique(get_var_src_codes(out='var'))}
#' @param times a length 2 vector of POSIXct dates
#' @param folder a folder to place the file outputs in (defaults to temp 
#'   directory)
#' @param version character string indicating whether you want to stage the 
#'   \code{ts} as a .tsv or .rds
#' @param verbose logical. provide verbose output?
#' @param url for web dataset. If missing, uses geoknife's "gldas" url
#' @param ... additional arguments passed to \code{\link[geoknife]{geoknife}}
#' @return a file handle for time series file created
#' @importFrom geoknife simplegeom webdata geoknife result webprocess times times<- url
#' @importFrom dataRetrieval readNWISsite
#' @importFrom unitted u get_units unitbundle
#' @importFrom stats setNames
#' @keywords internal
stage_ldas_ts <- function(sites, var, p_code, times, folder, version, verbose, url, expected_units, ...){
  
  # get site coordinates and format for geoknife
  lon_lat <- get_site_coords(sites, format="geoknife")
  lon_lat_df <- lon_lat[complete.cases(t(lon_lat))]
  
  apply.offset <- FALSE
  time.offset <- 86400*2
  stencil <- simplegeom(lon_lat_df)
  if (missing(url)){
    fabric <- webdata('nldas', variable = p_code, times = times)
  } else {
    fabric <- webdata(url=url, variable = p_code, times = times)
    if (url == "dods://cida-eros-netcdfdev.er.usgs.gov:8080/thredds/dodsC/thredds_workspace/stream_metab/nldas.ncml" |
        url == "dods://cida-eros-netcdfdev.er.usgs.gov:8080/thredds/dodsC/thredds_workspace/stream_metab/gldas.ncml"){
      apply.offset <- TRUE
      if(isTRUE(verbose)) message("applying 2 day offset to request due to data hosting bug")
      times(fabric) <- times(fabric) + time.offset
    } 
  }
  
  if(isTRUE(verbose)) message("Starting remote processing and data download")
  
  data_out <- geoknife(stencil, fabric, REQUIRE_FULL_COVERAGE = 'false', wait = TRUE) %>%
    result(with.units = TRUE)
  
  if(isTRUE(verbose)) message("Finished downloading data; now writing to file[s]")
  
  file_paths <- c()
  DateTime <- matches <- variable <- ".dplyr.var"
  for (i in 1:length(sites)){
    
    if (sites[i] %in% names(data_out)){
      site_data <- select_(data_out,'DateTime',sites[i],'variable','units') %>%
        filter(variable == p_code) %>%
        select(-variable)
      
      units <- as.character(site_data$units) %>% unique()
      if (p_code == "dswrfsfc" & units == 'W/m^2')
        units = "W m^-2"
      
      if(get_units(unitbundle(units)) != expected_units) 
        warning("expected units of ", expected_units, " but found units of ", units)
      
      
      site_data <- select(site_data, -units) %>%
        setNames(c('DateTime',var)) %>%
        u(c(NA, units))
      
      if (!all(is.na(site_data[var]))){
        if (apply.offset)
          site_data$DateTime = site_data$DateTime - time.offset
        fpath <- write_ts(site_data, site=sites[i], var=var, src="nldas", folder, version)
        file_paths <- c(file_paths, fpath)
      } else {
        if(isTRUE(verbose)) message("site ",sites[i], " has all NA values. Skipping file write")
      }
    } else {
      if(isTRUE(verbose)) message("site ",sites[i], " not found. Skipping file write")
    }
  }
  return(file_paths)
}
