#' Generate a list of files to post as a styx site
#' 
#' A styx site, named after the famous lotic barrier between life and non-life, 
#' Earth and the Underworld, is a repository for simulated metabolism data.
#' 
#' @param site a single styx site ID for which to generate data
#' @param basedon an NWIS or other site ID from which to pull the site 
#'   coordinates, elevation, and water temperature data
#' @param times the starting and ending date-times by which to bound the STYX 
#'   data. Give these as character strings in the Y-m-d H:M:S format (or just 
#'   Y-m-d) in local solar mean time.
#' @param doinit optional. If missing or NULL, and if day_start is specified, 
#'   doinit values will be pulled from the doobs data of the \code{basedon} site
#'   at the time specified by day_start. If doinit is specified, it should be a 
#'   numeric vector of doinit values (values of DO.obs on whcih to start each 
#'   day's simulated DO data, asusming these values are passed to a metab_sim 
#'   model).
#' @param day_start the time (as numeric hours, possibly negative) relative to 
#'   each date from which to collect dates and possibly daily doinit (DO.mod.1) 
#'   values
#' @param day_end the time (as numeric hours, possibly >24) relative to each
#'   date from which to collect dates
#' @param gpp a vector of daily GPPs to store with this site
#' @param er a vector of daily ERs to store with this site
#' @param K600 a vector of daily K600s to store with this site
#' @param info length-1 character string description of the site, how it differs
#'   from other Styx sites, etc., to be included in the styx metadata
#' @examples
#' x <- stage_styx_site()
#' @import streamMetabolizer
#' @export
stage_styx_site <- function(
  site="styx_000000", basedon="nwis_07239450", times=c("2012-05-14 21:00:00","2012-05-16 07:00:00"),
  day_start, day_end, doinit, gpp, er, K600,
  info) {
  
  # coords
  meta_basic <- get_meta("basic")
  coords <- meta_basic[meta_basic$site_name == basedon,c("lat","lon","alt")]
  if(is.na(coords$alt)) {
    coords$alt <- lookup_usgs_elevation(coords$lat, coords$lon, units="Feet")$elevation
  }
  
  # function to filter by datetimes assuming times are in local clock time (standard, not DST)
  times.sitetime <- as.POSIXct(times, tz="UTC")
  filter_by_date <- function(df) {
    sitetime <- convert_GMT_to_solartime(date.time = df$DateTime, longitude = coords$lon, time.type = "mean solar")
    df[sitetime >= times.sitetime[1] & sitetime <= times.sitetime[2], ] 
  }
  
  # water temperature
  wtr <- stage_calc_ts(
    sites=site, var="wtr", src="simCopy", verbose=TRUE, 
    inputs=list(from_src="nwis", from_site=basedon, 
                filter_fun=filter_by_date))
  wtr_ts <- read_ts(wtr)
  
#   # DO observed concentration
#   doobs <- stage_calc_ts(
#     sites=site, var="doobs", src="simCopy", verbose=TRUE, 
#     inputs=list(from_src="nwis", from_site=basedon, 
#                 filter_fun=filter_by_date))
#   doobs_ts <- read_ts(doobs)
# 
#   # locate the values of doinit. do this even if they were supplied, because
#   # this is also how we'll get dates. if they weren't supplied, also use the
#   # calculated values to create the ts file
#   find_doinit_fun <- function(data_ply, data_daily_ply, ..., day_start, day_end, local_date) {
#     data.frame(doinit=data_ply$DO.obs[1])
#   }
#   doinit_ts <- streamMetabolizer:::mm_model_by_ply(
#     find_doinit_fun, data=data.frame(local.time=doobs_ts$DateTime, DO.obs=doobs_ts$doobs), data_daily_ply=NULL, 
#     day_start=day_start, day_end=day_end)
#   dates <- doinit_ts$DateTime
#   if(missing(doinit) || is.null(doinit)) {
#     stage_calc_ts(sites=site, var="doinit", src="simNew", inputs=list(figureoutwhatgoeshere))
#   }
#   
#   # at this point we've detemrined how many complete dates there are and check
#   # the lengths of doinit, gpp, er, and K600
#   dates <- unique()
  
  # sitetime
  sitetime <- stage_calc_ts(
    sites=site, var="sitetime", src="simLon", verbose=TRUE, 
    inputs=list(utctime=wtr_ts$DateTime, longitude=coords$lon))
  
  # depth
  depth <- stage_calc_ts(
    sites=site, var="depth", src="simNew", verbose=TRUE,
    inputs=list(utctime=NA, value=u(0.2, "m")))
  
  # baro
  baro <- stage_calc_ts(
    sites=site, var="baro", src="simNew", verbose=TRUE,
    inputs=list(
      utctime=NA,
      value=calc_air_pressure(elevation=coords$alt*u(0.3048,"m ft^-1"), attach.units=TRUE)))
  
  # dosat
  dosat <- stage_calc_ts(
    sites=site, var="dosat", src="simGGbconst", verbose=TRUE,
    inputs=list(
      utctime=wtr_ts$DateTime,
      wtr=wtr_ts$wtr,
      baro=read_ts(baro)$baro))
  
  # suntime
  suntime <- stage_calc_ts(
    sites=site, var="suntime", src="simLon", verbose=TRUE, 
    inputs=list(utctime=wtr_ts$DateTime, longitude=coords$lon))
  
  # par
  par <- stage_calc_ts(
    sites=site, var="par", src="simLat", verbose=TRUE,
    inputs=list(utctime=wtr_ts$DateTime, suntime=read_ts(suntime)$suntime, latitude=coords$lat))
  
  # bundle the filenames into a list
  list(metadata=c(site_name=site, basedon=basedon), 
       wtr=wtr, sitetime=sitetime, depth=depth, baro=baro, dosat=dosat, suntime=suntime, par=par) 
}