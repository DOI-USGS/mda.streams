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
#' @param depth unitted numeric mean stream depth in m
#' @param day_start the time (as numeric hours, possibly negative) relative to 
#'   each date from which to collect dates and possibly daily doinit (DO.mod.1) 
#'   values
#' @param day_end the time (as numeric hours, possibly >24) relative to each 
#'   date from which to collect dates
#' @param dates either a vector of Dates, to be used as-is, or a vector of 
#'   character strings in Y-m-d form to be interpreted as dates. If supplied as 
#'   character AND length 2, will be converted to Date and turned into a 
#'   sequence of consecutive Dates
#' @param doinit optional. If missing or NULL, and if day_start is specified, 
#'   doinit values will be pulled from the doobs data of the \code{basedon} site
#'   at the time specified by day_start. If doinit is specified, it should be a 
#'   unitted numeric vector of doinit values (values of DO.obs on whcih to start
#'   each day's simulated DO data, asusming these values are passed to a 
#'   metab_sim model).
#' @param gpp a unitted vector of daily GPPs to store with this site
#' @param er a unittedvector of daily ERs to store with this site
#' @param K600 a unitted vector of daily K600s to store with this site
#' @param info length-1 character string description of the site, how it differs
#'   from other Styx sites, etc., to be included in the styx metadata
#' @param verbose logical. provide status messages?
#' @examples
#' \dontrun{
#' x <- stage_styx_site(dates=NA, gpp=unitted::u(c(4,3,6), "g m^-2 d^-1"), 
#'   er=unitted::u(-10, "g m^-2 d^-1"), K600=unitted::u(15, "d^-1"), verbose=FALSE)
#' head(read_ts(x$wtr))
#' read_ts(x$K600)
#' }
#' @import streamMetabolizer
#' @importFrom lubridate is.Date
#' @importFrom unitted u
#' @export
stage_styx_site <- function(
  site="styx_000000", basedon="nwis_07239450", 
  times=c("2012-05-14 21:00:00","2012-05-16 07:00:00"), depth=u(0.2, "m"),
  day_start=-1.5, day_end=30, 
  dates=NA, doinit=u(as.numeric(NA), "mgO2 L^-1"), gpp=u(as.numeric(NA), "gO2 m^-2 d^-1"), 
  er=u(as.numeric(NA), "gO2 m^-2 d^-1"), K600=u(as.numeric(NA), "d^-1"),
  info=NA, verbose=TRUE) {
  
  # coords
  meta_basic <- get_meta("basic")
  coords <- meta_basic[meta_basic$site_name == basedon,c("lat","lon","alt")]
  if(is.na(coords$alt)) {
    coords$alt <- lookup_usgs_elevation(coords$lat, coords$lon, units="Feet")$elevation
  }
  
  ## INSTANTANEOUS (UNIT) TIMESERIES ##
  
  # function to filter by datetimes
  times.sitetime <- as.POSIXct(times, tz="UTC")
  filter_by_date <- function(df) {
    sitetime <- convert_UTC_to_solartime(date.time = df$DateTime, longitude = coords$lon, time.type = "mean solar")
    after_start <- if(is.na(times[1])) TRUE else sitetime >= times.sitetime[1]
    before_end <- if(length(times)<2 || is.na(times[2])) TRUE else sitetime <= times.sitetime[2]
    df[after_start & before_end, ] 
  }
  
  # water temperature
  wtr <- stage_calc_ts(
    sites=site, var="wtr", src="simCopy", verbose=verbose, 
    inputs=list(from_src="nwis", from_site=basedon, 
                filter_fun=filter_by_date))
  wtr_ts <- read_ts(wtr)
  
  # sitetime to match wtr
  sitetime <- stage_calc_ts(
    sites=site, var="sitetime", src="simLon", verbose=verbose, 
    inputs=list(utctime=wtr_ts$DateTime, longitude=coords$lon))
  sitetime_ts <- read_ts(sitetime)
  
  # DO observed concentration because (1) why not? and (2) it's a way to get
  # doinit values
  doobs <- stage_calc_ts(
    sites=site, var="doobs", src="simCopy", verbose=verbose, 
    inputs=list(from_src="nwis", from_site=basedon, 
                filter_fun=filter_by_date))
  
  # depth
  depth <- stage_calc_ts(
    sites=site, var="depth", src="simNew", verbose=verbose,
    inputs=list(utctime=NA, value=depth))
  
  # baro
  baro <- stage_calc_ts(
    sites=site, var="baro", src="simNew", verbose=verbose,
    inputs=list(
      utctime=NA,
      value=calc_air_pressure(elevation=coords$alt*u(0.3048,"m ft^-1"), attach.units=TRUE)*u(100,"Pa mb^-1")))
  
  # dosat
  dosat <- stage_calc_ts(
    sites=site, var="dosat", src="simGGbconst", verbose=verbose,
    inputs=list(
      utctime=wtr_ts$DateTime,
      wtr=wtr_ts$wtr,
      baro=read_ts(baro)$baro))
  
  # suntime
  suntime <- stage_calc_ts(
    sites=site, var="suntime", src="simLon", verbose=verbose, 
    inputs=list(utctime=wtr_ts$DateTime, longitude=coords$lon))
  
  # par
  par <- stage_calc_ts(
    sites=site, var="par", src="simLat", verbose=verbose,
    inputs=list(utctime=wtr_ts$DateTime, suntime=read_ts(suntime)$suntime, latitude=coords$lat))

  ## DAILY TIMESERIES ##
  
  # sitedate - discover / reformat as implied by the args, but also always check
  # for mismatches between data and data_daily
  sitedate_file_sitetime <- stage_calc_ts(
    sites=site, var="sitedate", src="simLon", verbose=verbose, 
    inputs=list(
      sitetime=sitetime_ts$sitetime, # infer dates from wtr/sitetime timeseries
      longitude=coords$lon,
      day_start = day_start,
      day_end = day_end))
  sitedate_ts_sitetime <- read_ts(sitedate_file_sitetime)
  if(is.na(dates[1])) { 
    # determine from sitetime_ts data series
    message("dates is NA, so pulling dates from the wtr/sitetime timeseries")
    sitedate_file <- sitedate_file_sitetime
  } else {
    # either use dates as-is or treat as the bounds of a date sequence. if you 
    # definitely want as-is, just use dates
    if(is.character(dates)) {
      dates <- as.Date(dates)
      if(length(dates) == 2) {
        message("converting dates to Date and converting to a sequence because is.character(dates) && length(dates) == 2")
        dates <- seq(dates[1], dates[2], by=1)
      } else {
        message("converting dates to Date and using as-is because is.character(dates) && length(dates) != 2")
      }
    } else {
      if(!lubridate::is.Date(dates)) stop("expecting dates to be either character or Date")
      message("using dates as-is because is.Date(dates)")
    }
    # convert dates (now known to be a character or Date vector of all desired 
    # dates) to a vector of POSIXcts whose date is definitely the date we want 
    # (UTC noons seem safe). then convert the vector of POSIXcts to a sitedate
    # ts of DateTime=UTC-noon and sitedate=Date
    sitedate_file_dates <- stage_calc_ts(
      sites=site, var="sitedate", src="simLon", verbose=verbose, 
      inputs=list(
        sitetime=as.POSIXct(paste0(dates, " 12:00:00"), tz="UTC"), # use provided dates
        longitude=coords$lon,
        day_start = day_start,
        day_end = day_end))
    sitedate_ts_dates <- read_ts(sitedate_file_dates)
    
    # check for mismatches between sitetime-inferred dates and param-specified dates
    daily_not_inst <- sitedate_ts_dates$sitedate[!(sitedate_ts_dates$sitedate %in% sitedate_ts_sitetime$sitedate)]
    if(length(daily_not_inst) > 0) 
      warning("dates in 'dates' but not 'sitetime' (kept): ", paste(daily_not_inst, collapse=", "))
    inst_not_daily <- sitedate_ts_sitetime$sitedate[!(sitedate_ts_sitetime$sitedate %in% sitedate_ts_dates$sitedate)]
    if(length(inst_not_daily) > 0) 
      warning("dates in 'sitetime' but not 'dates' (dropped): ", paste(inst_not_daily, collapse=", "))
    
    # regardless, sitedate_file_dates is still our final choice
    sitedate_file <- sitedate_file_dates
  }
  sitedate_ts <- read_ts(sitedate_file)
  
  # locate the values of doinit, either directly from the call or inferred from doobs
  if(missing(doinit) || is.na(doinit)) {
    doobs_ts <- read_ts(doobs)
    doinit_file_doobs <- stage_calc_ts(
      sites=site, var="doinit", src="simDStart", verbose=verbose, 
      inputs=list(
        doobs=doobs_ts$doobs,
        sitetime=doobs_ts$DateTime,
        longitude=coords$lon,
        day_start = day_start,
        day_end = day_end))
    doinit_ts_doobs <- read_ts(doinit_file_doobs)
    
    doinit_ts <- left_join(sitedate_ts, doinit_ts_doobs, by='DateTime')
    doinit_NAs <- is.na(doinit_ts$doinit)
    if(any(doinit_NAs)) {
      warning("doinit could not be identified for these dates: ", paste0(doinit_ts[doinit_NAs,'sitedate'], collapse=", "))
    }
    DateTime <- '.dplyr.var'
    doinit_ts <- doinit_ts %>% select(DateTime, doinit)
    doinit_file <- write_ts(doinit_ts, site=site, var='doinit', src='simDStart', folder=tempdir())
  } else {
    if(length(doinit) == 1) doinit <- rep(doinit, nrow(sitedate_ts))
    doinit_file <- stage_calc_ts(sites=site, var="doinit", src="simNew", inputs=list(utctime=sitedate_ts$DateTime, value=doinit), verbose=verbose)
  }
  
  # gpp
  if(missing(gpp) || is.na(gpp)) {
    warning("gpp was not supplied. using NAs in data.frame")
  }
  if(length(gpp) == 1) gpp <- rep(gpp, nrow(sitedate_ts))
  gpp_file <- stage_calc_ts(sites=site, var="gpp", src="simNew", inputs=list(utctime=sitedate_ts$DateTime, value=gpp), verbose=verbose)
  
  # er
  if(missing(er) || is.na(er)) {
    warning("er was not supplied. using NAs in data.frame")
  }
  if(length(er) == 1) er <- rep(er, nrow(sitedate_ts))
  er_file <- stage_calc_ts(sites=site, var="er", src="simNew", inputs=list(utctime=sitedate_ts$DateTime, value=er), verbose=verbose)
  
  # K600
  if(missing(K600) || is.na(K600)) {
    warning("K600 was not supplied. using NAs in data.frame")
  }
  if(length(K600) == 1) K600 <- rep(K600, nrow(sitedate_ts))
  K600_file <- stage_calc_ts(sites=site, var="K600", src="simNew", inputs=list(utctime=sitedate_ts$DateTime, value=K600), verbose=verbose)
  
  # bundle the filenames into a list
  list(metadata=c(site_name=v(site), basedon=v(basedon), info=v(info)),
       wtr=wtr, sitetime=sitetime, depth=depth, baro=baro, dosat=dosat, suntime=suntime, par=par, doobs=doobs,
       sitedate=sitedate_file, doinit=doinit_file, gpp=gpp_file, er=er_file, K600=K600_file) 
}