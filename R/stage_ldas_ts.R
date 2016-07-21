#' Stage NASA NLDAS or GLDAS data into a time series file
#' 
#' Downloads data from NLDAS/GLDAS and returns a vector of file handles.
#' 
#' @param sites a character vector of valid NWIS site IDs
#' @param var short name of var as in 
#'   \code{unique(get_var_src_codes(out='var'))}
#' @param src short name of the NASA-LDAS source, either 'nldas' or 'gldas'
#' @param times a length 2 vector of POSIXct dates, or characters convertable to
#'   POSIXct dates
#' @param url for web dataset. Required because there are corrections for 
#'   certain urls
#' @param folder a folder to place the file outputs in (defaults to temp 
#'   directory)
#' @param version character string indicating whether you want to stage the 
#'   \code{ts} as a .tsv or .rds
#' @param verbose logical. provide verbose output?
#' @param ... additional arguments passed to \code{\link[geoknife]{geoknife}}
#' @return a file handle for time series file created
#' @import dplyr
#' @importFrom geoknife simplegeom webdata geoknife check result webprocess 
#'   times times<- url
#' @importFrom unitted u verify_units
#' @importFrom stats setNames
#' @examples 
#' \dontrun{
#' files <- stage_ldas_ts(sites=c("nwis_06893820","nwis_50048580","nwis_01484680"), 
#'   var="baro", src='nldas', times=c('2014-01-01 00:00','2014-01-01 05:00'), verbose=TRUE)
#' lapply(setNames(nm=files), read_ts)
#' files <- stage_ldas_ts(sites=c("nwis_50048580","nwis_06893820","nwis_01484680"), 
#'   url=paste0("dods://cida-eros-netcdfdev.er.usgs.gov:8080/thredds/dodsC/",
#'              "thredds_workspace/stream_metab/gldas.ncml"),
#'   var="baro", src='gldas', times=c('2014-01-01 00:00','2014-01-01 05:00'), verbose=TRUE)
#' lapply(setNames(nm=files), read_ts)
#' }
#' @export
stage_ldas_ts <- function(sites, var, src=c('nldas','gldas'), times, url, folder=tempdir(), version=c('rds','tsv'), verbose=FALSE, ...) {
  
  # check arguments
  if(length(var) > 1) stop("one var at a time, please")
  src <- match.arg(src)
  verify_var_src(var, src, on_fail=warning)
  version <- match.arg(version)
  
  # get the p_code & expected_units
  dp <- list(var=var, src=src) # need a renamed version for get_var_src_codes filters
  p_code <- '.dplyr_var'
  var_src_info <- get_var_src_codes(src==dp$src, var==dp$var, !is.na(p_code), out=c('p_code','units'))
  p_code <- var_src_info$p_code
  expected_units <- var_src_info$units
  
  # define the geoknife stencil
  lon_lat <- get_site_coords(sites, format="geoknife", use_basedon=FALSE) # could do use_basedon=TRUE for styx
  if(verbose) 
    message('omitting these sites for lack of coordinates: ', 
            paste0(names(lon_lat)[!complete.cases(t(lon_lat))], collapse=', '))
  lon_lat_df <- lon_lat[complete.cases(t(lon_lat))]
  stencil <- simplegeom(lon_lat_df)
  
  # define the geoknife fabric
  if (missing(url)){
    fabric <- webdata(src, variable = p_code, times = times)
  } else {
    fabric <- webdata(url=url, variable = p_code, times = times)
  }
  
  # prepare, if needed, to correct DateTime for a known data hosting bug
  apply.offset <- fabric@url %in% c(
    "dods://cida-eros-netcdfdev.er.usgs.gov:8080/thredds/dodsC/thredds_workspace/stream_metab/nldas.ncml",
    "dods://cida-eros-netcdfdev.er.usgs.gov:8080/thredds/dodsC/thredds_workspace/stream_metab/gldas.ncml")
  if(apply.offset) {
    if(isTRUE(verbose)) message("applying 2 day offset to request due to data hosting bug")
    time.offset <- as.difftime(86400*2, units='secs')
    times(fabric) <- times(fabric) + time.offset
  }
  
  # run the geoknife job
  if(isTRUE(verbose)) message("starting remote processing and data download")
  job_out <- geoknife(stencil, fabric, REQUIRE_FULL_COVERAGE = 'false', wait = TRUE)
  if(check(job_out)$statusType == 'ProcessFailed') {
    message('the geoknife job failed: ', check(job_out)$status)
    return(c())
  }
  data_out <- result(job_out, with.units = TRUE)
  if(isTRUE(verbose)) message("finished downloading data; now writing to file[s]")
  
  # if nothing was found, return now
  if(!any(sites %in% names(data_out))) {
    if(isTRUE(verbose)) message("none of the sites were found. Skipping file write")
    return(c())
  }
  
  # check units & convert to unitted format if needed
  data_out <- filter(data_out, variable == p_code)
  units <- as.character(data_out$units) %>% unique()
  if(p_code == "dswrfsfc" && units == 'W/m^2') units <- "W m^-2"
  if(p_code %in% c("psurf","swdown") && is.na(units)) units <- expected_units # see GH issue #124
  verify_units(u(1,units), expected_units, violation.handler=warning)

  # write the output into one file per site
  file_paths <- c()
  DateTime <- matches <- variable <- ".dplyr.var"
  for (i in 1:length(sites)){

    if (sites[i] %in% names(data_out)){
      # pull out the data columns for this site, rename, apply the datetime
      # re-offset if needed, and attach units
      site_data <- select_(data_out, 'DateTime', sites[i]) %>%
        setNames(c('DateTime', var)) %>%
        mutate(DateTime = if(apply.offset) DateTime - time.offset else DateTime) %>%
        u(c(NA, units))
      
      # if non-NA data exist, write the file
      if (!all(is.na(site_data[var]))){
        fpath <- write_ts(site_data, site=sites[i], var=var, src=src, folder, version)
        file_paths <- c(file_paths, fpath)
      } else {
        if(isTRUE(verbose)) message("site ", sites[i], " has all NA values. Skipping file write")
      }
    } else {
      if(isTRUE(verbose)) message("site ", sites[i], " not found. Skipping file write")
    }
  }
  return(file_paths)
}
