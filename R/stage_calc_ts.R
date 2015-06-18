#' @title stage calculated or modeled data into a time series file
#' @description accept calculated/modeled data as a data.frame and return a file handle
#' 
#' @param sites a character vector of valid NWIS site IDs
#' @param var short name of variable as in \code{get_var_codes(out='var')}
#' @param src short name of src as in \code{get_src_codes()}
#' @param folder a folder to place the file outputs in (defaults to temp directory)
#' @param verbose provide verbose output (currently not implemented)
#' @param ... additional arguments passed to \code{\link[geoknife]{geoknife}} and \code{\link[unitted]{write_unitted}}
#' @return a file handle for time series file created 
#' @importFrom unitted u write_unitted
#' @import streamMetabolizer
#' 
#' @examples
#' \dontrun{
#' files <- stage_calc_ts(sites="nwis_01484680", var="dosat", src="calcGG")
#' }
#' @export
stage_calc_ts <- function(sites, var, src, folder = tempdir(), verbose = FALSE, ...){
  
  if(length(var) > 1) stop("one var at a time, please")
  if(length(src) > 1) stop("one src at a time, please")
  if(!(src %in% get_src_codes(out="src"))) stop("couldn't find src in get_src_codes(out='src')")
  
  # loop through sites, adding to file_paths for any successfully computed & written ts files
  file_paths <- c()
  un_sites <- unique(sites)
  for (i in 1:length(un_sites)){
    site <- un_sites[i]
    
    # compute the data
    if(isTRUE(verbose)) message("computing the data for ", site, "-", var, "_", src)
    tryCatch({
      switch(
        paste0(var, "_", src),
        # baro is actually probably a metadata item, not a ts, even with the elevation corerction.
        # 'baro_calcElev' = {
        #   meta_calc <- calc_air_pressure(
        #     elevation=NA)
        # },
        'dosat_calcGG' = {
          wtr_nwis <- read_ts(download_ts("wtr_nwis", site, on_local_exists="replace"))
          baro_avg <- calc_air_pressure(attach.units=TRUE) * u(0.01, "mb Pa^-1")
          ts_calc <- data.frame(
            DateTime=wtr_nwis$DateTime,
            dosat=calc_DO_at_sat(
              temp.water=wtr_nwis$wtr, 
              pressure.air=baro_avg,
              salinity.water=u(0, 'PSU')))
        },
        'depth_calcDisch' = {
          disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
          ts_calc <- data.frame(
            DateTime=disch_nwis$DateTime,
            depth=calc_depth(
              Q=disch_nwis$disch * u(0.0283168466,"m3/s cfs^-1")))
        },
        'suntime_calcLon' = {
          #warning("need the date.time from the full join of all relevant data. this isn't right yet.")
          utctime <- read_ts(download_ts("doobs_nwis", site, on_local_exists="replace"))$DateTime # should be using merged data, not just doobs...or maybe doobs is good enough
          lon_site <- find_site_coords(site)$lon
          ts_calc <- data.frame(
            DateTime=utctime,
            suntime=convert_GMT_to_solartime(
              date.time=utctime, 
              longitude=lon_site, 
              time.type="apparent solar"))
        },
        'light_calcLat' = {
          suntime_calcLon <- read_ts(download_ts('suntime_calcLon', site))
          lat_site <- find_site_coords(site)$lat
          ts_calc <- data.frame(
            DateTime=suntime_calcLon$DateTime,
            light=calc_solar_insolation(
              date.time=suntime_calcLon$suntime,
              latitude=lat_site))
        },
        # 'K600_calcNight' = {
        #   warning('entirely unimplemented')
        # }, 
        {
          stop("the calculation for var_src ", paste0(var, "_", src), " isn't implemented yet")
        }
      )}, error=function(e) {
        message(e)
        ts_calc <- data.frame()
      }
    )
    
    # ensure proper format
    
    # write the data to file
    if(isTRUE(verbose)) message("writing the downloaded data to file")
    if(nrow(ts_calc) > 0) {
      fpath <- write_ts(ts_calc, site=site, var=var, src=src, folder)
      file_paths <- c(file_paths, fpath)
    } else {
      if(isTRUE(verbose)) message("data couldn't be computed for site ", site)
      # leave file_paths untouched if there's no new file
    }
  }
  
  return(file_paths)
}