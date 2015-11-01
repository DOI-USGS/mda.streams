#' @title stage calculated or modeled data into a time series file
#' @description accept calculated/modeled data as a data.frame and return a file
#'   handle
#'   
#' @param sites a character vector of valid NWIS site IDs
#' @param var short name of variable as in 
#'   \code{unique(get_var_src_codes(out='var'))}
#' @param src short name of src as in \code{get_var_src_codes(var==myvar,
#'   out='src')}
#' @param folder a folder to place the file outputs in (defaults to temp 
#'   directory)
#' @param inputs appropriate only when src is a simXxxx type. a list of named 
#'   inputs (data.frames, constants, etc.) to pass to the specified calculation 
#'   function. These inputs are downloaded from standard locations for the 
#'   calcXxxx variants.
#' @param verbose logical. provide status messages?
#' @param ... additional arguments passed to \code{\link[geoknife]{geoknife}} 
#'   and \code{\link[unitted]{write_unitted}}
#' @return a file handle for time series file created
#'   
#' @importFrom unitted u write_unitted
#' @importFrom lubridate tz
#' @import streamMetabolizer
#' @import dplyr
#' @importFrom stats complete.cases
#'   
#' @examples
#' \dontrun{
#' set_scheme("mda_streams_dev")
#' 
#' # calc
#' 
#' file_suntime <- stage_calc_ts(sites="nwis_08062500", var="suntime", src="calcLon", verbose=TRUE)
#' head(read_ts(file_suntime))
#' post_ts(file_suntime, on_exists="skip", verbose=TRUE) # need this posted for next calcs
#' 
#' file_sitetime <- stage_calc_ts(sites="nwis_08062500", var="sitetime", src="calcLon", verbose=TRUE)
#' head(read_ts(file_sitetime))
#' post_ts(file_sitetime, on_exists="skip", verbose=TRUE)
#' 
#' file_par <- stage_calc_ts(sites="nwis_08062500", var="par", src="calcLat", verbose=TRUE)
#' head(read_ts(file_par))
#' post_ts(file_par, on_exists="skip", verbose=TRUE) # don't need this later, but try it out
#' 
#' file_depth <- stage_calc_ts(sites="nwis_08062500", var="depth", src="calcDisch", verbose=TRUE)
#' head(read_ts(file_depth))
#' post_ts(file_depth, on_exists="skip", verbose=TRUE) # don't need this later, but try it out
#' 
#' file_dosat <- stage_calc_ts(sites="nwis_08062500", var="dosat", src="calcGGbconst", verbose=TRUE)
#' head(read_ts(file_dosat))
#' post_ts(file_dosat, on_exists="skip", verbose=TRUE) # don't need this later, but try it out
#' 
#' # sim
#' 
#' library(dplyr); library(unitted)
#' real_doobs <- read_ts(download_ts("doobs_nwis", "nwis_08062500", on_local_exists="replace")) %>%
#'   subset(v(DateTime) > as.POSIXct("2014-07-13", tz="UTC") & 
#'     v(DateTime) <= as.POSIXct("2014-07-15", tz="UTC"))
#' 
#' file_suntime <- stage_calc_ts(sites="nwis_08062500", var="suntime", src="simLon", verbose=TRUE, 
#'   inputs=list(utctime=real_doobs$DateTime, longitude=u(-96.46304, "degE")))
#' head(read_ts(file_suntime))
#' 
#' file_par <- stage_calc_ts(sites="nwis_08062500", var="par", src="simLat", verbose=TRUE,
#'   inputs=list(utctime=real_doobs$DateTime, suntime=read_ts(file_suntime)$suntime, 
#'   latitude=u(32.42653, "degN")))
#' head(read_ts(file_par))
#' 
#' file_depth <- stage_calc_ts(sites="nwis_08062500", var="depth", src="simDisch", verbose=TRUE,
#'   inputs=list(utctime=real_doobs$DateTime, disch=u(rep(2900, nrow(real_doobs)), "ft^3 s^-1")))
#' head(read_ts(file_depth))
#' 
#' file_dosat <- stage_calc_ts(sites="nwis_08062500", var="dosat", src="simGGbconst", verbose=TRUE,
#'   inputs=list(utctime=real_doobs$DateTime, wtr=u(rep(12, 192), "degC"), baro=u(90000, "Pa")))
#' head(read_ts(file_dosat))
#' 
#' # simNew
#' 
#' file_par2 <- stage_calc_ts(sites="nwis_08062500", var="par", src="simNew", verbose=TRUE,
#'   inputs=list(utctime=real_doobs$DateTime, 
#'               value=u(rnorm(real_doobs$DateTime, 10, 2), "umol m^-2 s^-1")))
#' head(read_ts(file_par2))
#' 
#' # simCopy
#' 
#' file_suntime2 <- stage_calc_ts(sites="styx_001001", var="suntime", src="simCopy", verbose=TRUE,
#'   inputs=list(from_src="calcLon", from_site="nwis_08062500", filter_fun=function(df) {
#'     df[df$DateTime > as.POSIXct("2015-05-14", tz="UTC") &
#'          df$DateTime < as.POSIXct("2015-05-17", tz="UTC"), ] }))
#' file_suntime2
#' head(read_ts(file_suntime2))
#' 
#' # daily means
#' 
#' file_sitetimedaily <- stage_calc_ts(sites="nwis_08062500", 
#'   var="sitetimedaily", src="calcLon", verbose=TRUE)
#' head(read_ts(file_sitetimedaily))
#' 
#' file_sitedate <- stage_calc_ts(sites="nwis_08062500", 
#'   var="sitedate", src="calcLon", verbose=TRUE)
#' str(read_ts(file_sitedate))
#' 
#' file_dischdaily <- stage_calc_ts(sites="nwis_08062500", 
#'   var="dischdaily", src="calcDMean", verbose=TRUE)
#' head(read_ts(file_dischdaily))
#' 
#' set_scheme("mda_streams")
#' }
#' @export
stage_calc_ts <- function(sites, var, src, folder = tempdir(), inputs=list(), verbose = FALSE, ...){
  
  if(length(var) > 1) stop("one var at a time, please")
  if(length(src) > 1) stop("one src at a time, please")
  verify_var_src(var, src, on_fail=warning)
  
  # loop through sites, adding to file_paths for any successfully computed & written ts files
  file_paths <- c()
  un_sites <- unique(sites)
  for (i in 1:length(un_sites)){
    site <- un_sites[i]
    
    # compute the data
    if(isTRUE(verbose)) message("computing the data for ", site, "-", var, "_", src)
    ts_calc <- tryCatch({
      if(src %in% c('simNew', 'simCopy')) {
        switch(
          src,
          'simNew' = calc_ts_with_input_check(inputs=c(list(var=var), inputs), 'calc_ts_simNew'),
          'simCopy' = calc_ts_with_input_check(inputs=c(list(var=var), inputs), 'calc_ts_simCopy')
        )
      } else {
        switch(
          paste0(var, "_", src),
          'sitetime_calcLon' = {
            calc_ts_sitetime_calcLon(
              utctime = read_ts(download_ts("doobs_nwis", site, on_local_exists="replace"))$DateTime, 
              longitude = get_site_coords(site)$lon)
          },
          'sitetime_simLon' = {
            calc_ts_with_input_check(inputs, 'calc_ts_sitetime_calcLon')
          },
          'sitetimedaily_calcLon' = {
            sitetime <- read_ts(download_ts("sitetime_calcLon", site, on_local_exists="replace"))
            calc_ts_sitetimedaily_calcLon(
              sitetime = as.POSIXct(paste0(unique(format(sitetime$sitetime, "%Y-%m-%d")), " 12:00:00"), tz="UTC"), 
              longitude = get_site_coords(site)$lon)
          },
          'sitedate_calcLon' = {
            sitetime <- read_ts(download_ts("sitetime_calcLon", site, on_local_exists="replace"))
            calc_ts_sitedate_calcLon(
              sitetime = as.POSIXct(paste0(unique(format(sitetime$sitetime, "%Y-%m-%d")), " 12:00:00"), tz="UTC"), 
              longitude = get_site_coords(site)$lon)
          },
          'sitedate_simLon' = {
            calc_ts_with_input_check(inputs, 'calc_ts_sitedate_calcLon')
          },
          'suntime_calcLon' = {
            calc_ts_suntime_calcLon(
              utctime = read_ts(download_ts("doobs_nwis", site, on_local_exists="replace"))$DateTime, 
              longitude = get_site_coords(site)$lon)
          },
          'suntime_simLon' = {
            calc_ts_with_input_check(inputs, 'calc_ts_suntime_calcLon')
          },
          'par_calcLat' = {
            suntime_calcLon <- read_ts(download_ts('suntime_calcLon', site, on_local_exists="replace"))
            calc_ts_par_calcLat(
              utctime = suntime_calcLon$DateTime,
              suntime = suntime_calcLon$suntime,
              latitude = get_site_coords(site)$lat)
          },
          'par_calcSw' = {
            sw_nldas <- read_ts(download_ts('sw_nldas', site, on_local_exists="replace"))
            calc_ts_par_calcSw(
              utctime = sw_nldas$DateTime,
              sw = sw_nldas$sw)
          },
          'par_simLat' = {
            calc_ts_with_input_check(inputs, 'calc_ts_par_calcLat')
          },
          'depth_calcDisch' = {
            disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
            calc_ts_depth_calcDisch(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch)
          },
          'depth_calcDischRaymond' = {
            disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
            calc_ts_depth_calcDischRaymond(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch)
          },
          'depth_calcDischHarvey' = {
            disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
            dvqcoefs <- get_meta('dvqcoefs')
            dvqcoef <- dvqcoefs[which(dvqcoefs$site_name==site),]
            if(nrow(dvqcoef) == 0) dvqcoef[1,'c'] <- u(NA,'m')
            calc_ts_depth_calcDischHarvey(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch,
              c = dvqcoef[[1,'dvqcoefs.c']],
              f = dvqcoef[[1,'dvqcoefs.f']])
          },
          'veloc_calcDischRaymond' = {
            disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
            calc_ts_veloc_calcDischRaymond(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch)
          },
          'veloc_calcDischHarvey' = {
            disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
            dvqcoefs <- get_meta('dvqcoefs')
            dvqcoef <- dvqcoefs[which(dvqcoefs$site_name==site),]
            if(nrow(dvqcoef) == 0) dvqcoef[1,'k'] <- u(NA,'m s^-1')
            calc_ts_veloc_calcDischHarvey(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch,
              k = dvqcoef[[1,'dvqcoefs.k']],
              m = dvqcoef[[1,'dvqcoefs.m']])
          },
          'depth_simDisch' = {
            calc_ts_with_input_check(inputs, 'calc_ts_depth_calcDisch')
          },
          'doamp_calcDAmp' = {
            dopsat_calcObsSat <- read_ts(download_ts("dopsat_calcObsSat", site, on_local_exists="replace"))
            sitetime_calcLon <- read_ts(download_ts('sitetime_calcLon', site, on_local_exists="replace"))
            combo <- combine_ts(sitetime_calcLon, dopsat_calcObsSat, method='approx')
            combo <- combo[complete.cases(combo),]
            calc_ts_doamp_calcDAmp(
              sitetime = combo$sitetime,
              longitude = get_site_coords(site)$lon,
              dopsat = combo$dopsat)
          },
          'dosat_calcGGbts' = {
            best_wtr <- paste0("wtr_", choose_data_source("wtr", site, logic="priority local")$src)
            wtr_best <- read_ts(download_ts(best_wtr, site, on_local_exists="replace"))
            baro_nldas <- read_ts(download_ts("baro_nldas", site, on_local_exists="replace"))
            if(nrow(baro_nldas) != nrow(wtr_best)/2) stop("need nrow(baro)~=nrow(wtr) for dosat_calcGGbts")
            combo <- combine_ts(wtr_best, baro_nldas, method='approx')
            calc_ts_dosat_calcGG(
              utctime = combo$DateTime,
              wtr = combo$wtr,
              baro = combo$baro)
          },
          'dosat_calcGGbconst' = {
            best_wtr <- paste0("wtr_", choose_data_source("wtr", site, logic="priority local")$src)
            wtr_best <- read_ts(download_ts(best_wtr, site, on_local_exists="replace"))
            elev_ft <- get_meta('basic') %>% .[.$site_name==site,'alt']
            baro_const <- u(data.frame(DateTime=NA, baro=calc_air_pressure(elevation=elev_ft*u(0.3048,"m ft^-1"), attach.units=TRUE)*u(100, "Pa mb^-1")))
            if(!is.na(baro_const$DateTime)) stop("need non-NA baro$DateTime for dosat_calcGGbconst")
            combo <- combine_ts(wtr_best, baro_const, method='approx')
            calc_ts_dosat_calcGG(
              utctime = combo$DateTime,
              wtr = combo$wtr,
              baro = combo$baro)
          },
          'dosat_simGGbts' = {
            if(length(inputs$baro) < length(inputs$utctime)/2) stop("need length(baro)~=length(utctime) for dosat_simGGbts")
            calc_ts_with_input_check(inputs, 'calc_ts_dosat_calcGG')
          },
          'dosat_simGGbconst' = {
            if(length(inputs$baro) != 1) stop("need 1-row baro for dosat_simGGbconst")
            calc_ts_with_input_check(inputs, 'calc_ts_dosat_calcGG')
          },
          'dopsat_calcObsSat' = {
            doobs_nwis <- read_ts(download_ts("doobs_nwis", site, on_local_exists="replace"))
            best_dosat <- paste0("dosat_", choose_data_source("dosat", site, logic="priority local")$src)
            if(best_dosat == "dosat_NA") stop("could not locate an appropriate dosat ts for site ", site)
            dosat_best <- read_ts(download_ts(best_dosat, site, on_local_exists="replace"))
            combo <- combine_ts(doobs_nwis, dosat_best, method='approx')
            calc_ts_dopsat_calcObsSat(
              utctime = combo$DateTime, 
              doobs = combo$doobs, 
              dosat = combo$dosat)
          },
          'dischdaily_calcDMean' = {
            disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
            sitetime_calcLon <- read_ts(download_ts('sitetime_calcLon', site, on_local_exists="replace"))
            combo <- combine_ts(sitetime_calcLon, disch_nwis, method='approx', approx_tol=as.difftime(3, units="hours"))
            combo <- combo[complete.cases(combo),]
            calc_ts_dischdaily_calcDMean(
              sitetime = combo$sitetime,
              longitude = get_site_coords(site)$lon,
              disch = combo$disch)
          },
          'velocdaily_calcDMean' = {
            best_veloc <- paste0("veloc_", choose_data_source("veloc", site, logic="priority local")$src)
            if(best_veloc == "veloc_NA") stop("could not locate an appropriate dosat ts for site ", site)
            veloc_best <- read_ts(download_ts(best_veloc, site, on_local_exists="replace"))
            sitetime_calcLon <- read_ts(download_ts('sitetime_calcLon', site, on_local_exists="replace"))
            combo <- combine_ts(sitetime_calcLon, veloc_best, method='approx')
            combo <- combo[complete.cases(combo),]
            calc_ts_velocdaily_calcDMean(
              sitetime = combo$sitetime,
              longitude = get_site_coords(site)$lon,
              veloc = combo$veloc)
          },
          {
            stop("the calculation for var_src ", paste0(var, "_", src), " isn't implemented yet")
          }
        )
      }}, error=function(e) {
        message(e, "\n")
        data.frame()
      }
    )
    
    # ensure proper format here
    
    # write the data to file
    if(isTRUE(verbose)) message("writing the computed data to file")
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


#' Helper to stage_calc_ts for simXxx-style functions
#' 
#' Checks to see that the inputs are just the right ones to supply to the
#' function named by calc_fun
#' 
#' @param inputs a list of named inputs to check
#' @param calc_fun the name (character) of the function against which to check 
#'   the inputs
#' @import streamMetabolizer
calc_ts_with_input_check <- function(inputs, calc_fun) {
  expected <- names(formals(calc_fun))
  provided <- names(inputs)
  if(length(omitted <- expected[!(expected %in% provided)]) > 0) {
    stop("still need these inputs: ", paste0(omitted, collapse=", "))
  }
  if(length(excess <- provided[!(provided %in% expected)]) > 0) {
    stop("found unnecessary inputs: ", paste0(excess, collapse=", "))
  }
  do.call(calc_fun, inputs)
}

#' Internal - calculate sitetime_calcLon from any data
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param longitude the site longitude in degrees E
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_sitetime_calcLon <- function(utctime, longitude) {
  data.frame(
    DateTime = utctime,
    sitetime = convert_GMT_to_solartime(
      date.time = utctime, 
      longitude = longitude, 
      time.type = "mean solar")) %>%
    as.data.frame() %>% u()
}

#' Internal - calculate sitetimedaily_calcLon from any data
#' 
#' @param sitetime the DateTimes of the local noons of interest, in UTC/GMT
#' @param longitude the site longitude in degrees E
#' @importFrom unitted u
#'   
#' @keywords internal
calc_ts_sitetimedaily_calcLon <- function(sitetime, longitude) {
  data.frame(
    DateTime = convert_solartime_to_GMT(
      solar.time = sitetime, 
      longitude = longitude, 
      time.type = "mean solar"),
    sitetimedaily = sitetime) %>%
    as.data.frame() %>% u()
}

#' Internal - calculate sitedate_calcLon from any data
#' 
#' @param sitetime the DateTimes of the local noons of interest, in UTC/GMT
#' @param longitude the site longitude in degrees E
#' @importFrom unitted u
#'   
#' @keywords internal
calc_ts_sitedate_calcLon <- function(sitetime, longitude) {
  data.frame(
    DateTime = convert_solartime_to_GMT(
      solar.time = sitetime, 
      longitude = longitude, 
      time.type = "mean solar"),
    sitedate = as.Date(format(sitetime, "%Y-%m-%d"))) %>%
    as.data.frame() %>% u()
}

#' Internal - calculate suntime_calcLon from any data
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param longitude the site longitude in degrees E
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_suntime_calcLon <- function(utctime, longitude) {
  data.frame(
    DateTime = utctime,
    suntime = convert_GMT_to_solartime(
      date.time = utctime, 
      longitude = longitude, 
      time.type = "apparent solar")) %>%
    as.data.frame() %>% u()
}

#' Internal - calculate par_calcLat from any data
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param suntime the apparent solar time at the site
#' @param latitude the site latitude in degrees N
#' @import streamMetabolizer
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_par_calcLat <- function(utctime, suntime, latitude) {
  data.frame(
    DateTime = utctime,
    par = convert_SW_to_PAR(
      calc_solar_insolation(
        solar.time = suntime,
        latitude = latitude))) %>% 
    u()
}

#' Internal - calculate par from SW data
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param sw shortwave radiation in W m^-2
#' @import streamMetabolizer
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_par_calcSw <- function(utctime, sw) {
  data.frame(
    DateTime = utctime,
    par = convert_SW_to_PAR(sw)) %>% 
    u()
}


#' Internal - calculate depth_calcDisch from any data using the Raymond et al.
#' coefficients
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param disch the discharge in ft^3 s^-1
#' @importFrom unitted u
#' @import streamMetabolizer
#'   
#' @keywords internal
calc_ts_depth_calcDisch <- function(utctime, disch) {
  Q <- verify_units(disch * u(0.0283168466,"m^3 ft^-3"), 'm^3 s^-1')
  data.frame(
    DateTime = utctime,
    depth = calc_depth(Q=Q)) %>% u()
}

#' Internal - calculate depth_calcDisch from any data using the Raymond et al.
#' coefficients
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param disch the discharge in ft^3 s^-1
#' @importFrom unitted u
#' @import streamMetabolizer
#'   
#' @keywords internal
calc_ts_depth_calcDischRaymond <- function(utctime, disch) {
  Q <- verify_units(disch * u(0.0283168466,"m^3 ft^-3"), 'm^3 s^-1')
  data.frame(
    DateTime = utctime,
    depth = calc_depth(Q=Q)) %>% u()
}

#' Internal - calculate depth_calcDisch from discharge and depth-vs-discharge
#' coefficients from Jud Harvey
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param disch the discharge in ft^3 s^-1
#' @param c the multiplier in d = c * Q^f
#' @param f the exponent in d = c * Q^f
#' @importFrom unitted u verify_units v
#' @import streamMetabolizer
#'    
#' @keywords internal
calc_ts_depth_calcDischHarvey <- function(utctime, disch, c, f) {
  Q <- verify_units(disch * u(0.0283168466,"m^3 ft^-3"), 'm^3 s^-1')
  data.frame(
    DateTime = utctime, 
    depth = calc_depth(Q=Q, c=c, f=f)) %>% u()
}

#' Internal - calculate depth_calcDisch from any data using the Raymond et al.
#' coefficients
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param disch the discharge in ft^3 s^-1
#' @importFrom unitted u
#' @import streamMetabolizer
#'   
#' @keywords internal
calc_ts_veloc_calcDischRaymond <- function(utctime, disch) {
  Q <- verify_units(disch * u(0.0283168466,"m^3 ft^-3"), 'm^3 s^-1')
  data.frame(
    DateTime = utctime,
    veloc = calc_velocity(Q=Q)) %>% u()
}


#' Internal - calculate velocity from discharge and velocity-vs-discharge 
#' coefficients from Jud Harvey
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param disch the discharge in ft^3 s^-1
#' @param k the multiplier in U = k * Q^m
#' @param m the exponent in U = k * Q^m
#' @importFrom unitted u verify_units v
#' @import streamMetabolizer
#'   
#' @keywords internal
calc_ts_veloc_calcDischHarvey <- function(utctime, disch, k, m) {
  Q <- verify_units(disch * u(0.0283168466,"m^3 ft^-3"), 'm^3 s^-1')
  data.frame(
    DateTime = utctime, 
    veloc = calc_velocity(Q=Q, k=k, m=m)) %>% u()
}

#' Internal - calculate dosat_calcGG from any data
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param wtr the water temperature
#' @param baro the barometric pressure
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_dosat_calcGG <- function(utctime, wtr, baro) {
  data.frame(
    DateTime = utctime,
    dosat = calc_DO_at_sat(
      temp.water = wtr, 
      pressure.air = baro * u(0.01, "mb Pa^-1"),
      salinity.water = u(0, 'PSU'))) %>% u()
}

#' Internal - calculate dopsat_calcObsSat from any data
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param doobs the observed DO concentration
#' @param dosat the DO concentration at saturation
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_dopsat_calcObsSat <- function(utctime, doobs, dosat) {
  data.frame(
    DateTime = utctime,
    dopsat = (doobs / dosat) * u(100, "%")) %>% u()
}

#' Internal - calculate any variable with simNew from any data
#' 
#' @param var the variable name
#' @param utctime the DateTime with tz of UTC/GMT
#' @param value unitted vector of ts values
#' @importFrom unitted u
#' @importFrom stats setNames
#' 
#' @keywords internal
calc_ts_simNew <- function(var, utctime, value) {
  data.frame(utctime, value) %>%
    setNames(c('DateTime', var)) %>%
    u()
}

#' Internal - calculate any variable with simNew from any data
#' 
#' @param var the variable name
#' @param from_src the src for var to copy from
#' @param from_site the site to copy from
#' @param filter_fun NULL, or a function to apply to the copied data before 
#'   staging it to a new file. filter_fun needs to accept and return a unitted
#'   data.frame of the standard ts form for mda.streams
#'    
#' @keywords internal
calc_ts_simCopy <- function(var, from_src, from_site, filter_fun) {
  from_data <- read_ts(download_ts(
    var_src=make_var_src(var, from_src), 
    site_name=from_site, on_local_exists="replace"))
  if(!is.null(filter_fun)) filter_fun(from_data) else from_data
}

#' Internal - calculate daily mean discharge from instantaneous discharge
#' 
#' Daily means are computed for the 24 hour periods from midnight to midnight in
#' sitetime. Their DateTime values are noon in sitetime, converted back to UTC. 
#' These centers approximately correspond to the daily centers for the
#' metabolism estimation data.
#' 
#' @param sitetime the local time, e.g. from sitetime_calcLon
#' @import streamMetabolizer
#' @import dplyr
#' @importFrom unitted v u get_units
#'   
#' @keywords internal
calc_ts_dischdaily_calcDMean <- function(sitetime, longitude, disch) {
  onedate <- DateTime <- dischdaily <- '.dplyr.var'
  disch_units <- get_units(disch)
  data.frame(
    date = as.Date(v(sitetime)),
    disch = v(disch)
  ) %>% 
    group_by(date) %>%
    summarize(
      onedate = date[1],
      dischdaily = mean(disch)) %>%
    mutate(
      DateTime = convert_solartime_to_GMT(
        as.POSIXct(paste0(onedate, " 12:00:00"), tz="UTC"), 
        longitude=longitude, time.type="mean solar")) %>%
    select(DateTime, dischdaily) %>%
    as.data.frame() %>%
    transform(dischdaily = verify_units(u(dischdaily, disch_units) * u(0.0283168466,"m^3 ft^-3"), 'm^3 s^-1')) %>%
    u()
}

#' Internal - calculate daily mean velocity from instantaneous
#' 
#' Daily means are computed for the 24 hour periods from midnight to midnight in
#' sitetime. Their DateTime values are noon in sitetime, converted back to UTC. 
#' These centers approximately correspond to the daily centers for the
#' metabolism estimation data.
#' 
#' @param sitetime the local time, e.g. from sitetime_calcLon
#' @import streamMetabolizer
#' @import dplyr
#' @importFrom unitted v u get_units
#'   
#' @keywords internal
calc_ts_velocdaily_calcDMean <- function(sitetime, longitude, veloc) {
  onedate <- DateTime <- velocdaily <- '.dplyr.var'
  data.frame(
    date = as.Date(v(sitetime)),
    veloc = v(veloc)
  ) %>% 
    group_by(date) %>%
    summarize(
      onedate = date[1],
      velocdaily = mean(veloc)) %>%
    mutate(
      DateTime = convert_solartime_to_GMT(
        as.POSIXct(paste0(onedate, " 12:00:00"), tz="UTC"), 
        longitude=longitude, time.type="mean solar")) %>%
    select(DateTime, velocdaily) %>%
    as.data.frame() %>%
    u(c(NA, get_units(veloc)))
}


#' Internal - calculate the daily amplitude in DO
#' 
#' @param sitetime the local time, e.g. from sitetime_calcLon
#' @param doobs the observed DO concentration
#' @import streamMetabolizer
#' @importFrom unitted u v
#' 
#' @keywords internal
calc_ts_doamp_calcDAmp <- function(sitetime, longitude, dopsat) {
  onedate <- DateTime <- doamp <- '.dplyr.var'
  data.frame(
    date = as.Date(v(sitetime)),
    dopsat = v(dopsat)
  ) %>% 
    group_by(date) %>%
    summarize(
      onedate = date[1],
      doamp = diff(range(dopsat))) %>%
    mutate(
      DateTime = convert_solartime_to_GMT(
        as.POSIXct(paste0(onedate, " 12:00:00"), tz="UTC"), 
        longitude=longitude, time.type="mean solar")) %>%
    select(DateTime, doamp) %>%
    as.data.frame() %>%
    u(c(NA, get_units(dopsat)))
}