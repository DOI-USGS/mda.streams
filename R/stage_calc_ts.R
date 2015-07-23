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
#' @param verbose provide verbose output (currently not implemented)
#' @param ... additional arguments passed to \code{\link[geoknife]{geoknife}} 
#'   and \code{\link[unitted]{write_unitted}}
#' @return a file handle for time series file created
#'   
#' @importFrom unitted u write_unitted
#' @importFrom lubridate tz
#' @import streamMetabolizer
#' @import dplyr
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
      switch(
        paste0(var, "_", src),
        'sitetime_calcLon' = {
          calc_ts_sitetime_calcLon(
            utctime = read_ts(download_ts("doobs_nwis", site, on_local_exists="replace"))$DateTime, 
            longitude = find_site_coords(site)$lon)
        },
        'suntime_calcLon' = {
          calc_ts_suntime_calcLon(
            utctime = read_ts(download_ts("doobs_nwis", site, on_local_exists="replace"))$DateTime, 
            longitude = find_site_coords(site)$lon)
        },
        'suntime_simLon' = {
          calc_ts_with_input_check(inputs, 'calc_ts_suntime_calcLon')
        },
        'suntime_simNew' = {
          calc_ts_with_input_check(inputs=c(list(var='suntime'), inputs), 'calc_ts_simNew')
        },
        'suntime_simCopy' = {
          calc_ts_with_input_check(inputs=c(list(var='suntime'), inputs), 'calc_ts_simCopy')
        },
        
        'par_calcLat' = {
          suntime_calcLon <- read_ts(download_ts('suntime_calcLon', site, on_local_exists="replace"))
          calc_ts_par_calcLat(
            utctime = suntime_calcLon$DateTime,
            suntime = suntime_calcLon$suntime,
            latitude = find_site_coords(site)$lat)
        },
        'par_simLat' = {
          calc_ts_with_input_check(inputs, 'calc_ts_par_calcLat')
        },
        'par_simNew' = {
          calc_ts_with_input_check(inputs=c(list(var='par'), inputs), 'calc_ts_simNew')
        },
        'depth_calcDisch' = {
          disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
          calc_ts_depth_calcDisch(
            utctime = disch_nwis$DateTime,
            disch = disch_nwis$disch)
        },
        'depth_simDisch' = {
          calc_ts_with_input_check(inputs, 'calc_ts_depth_calcDisch')
        },
        'depth_simNew' = {
          calc_ts_with_input_check(inputs=c(list(var='depth'), inputs), 'calc_ts_simNew')
        },
        'dosat_calcGGbts' = {
          wtr_nwis <- read_ts(download_ts("wtr_nwis", site, on_local_exists="replace"))
          baro_nldas <- read_ts(download_ts("baro_nldas", site, on_local_exists="replace"))
          combo <- combine_ts(wtr_nwis, baro_nldas, method='approx')
          calc_ts_dosat_calcGG(
            utctime = combo$DateTime,
            wtr = combo$wtr,
            baro = combo$baro)
        },
        'dosat_calcGGbconst' = {
          wtr_nwis <- read_ts(download_ts("wtr_nwis", site, on_local_exists="replace"))
          baro_const <- u(data.frame(DateTime=NA, baro=calc_air_pressure(attach.units=TRUE)))
          combo <- combine_ts(wtr_nwis, baro_const, method='approx')
          calc_ts_dosat_calcGG(
            utctime = combo$DateTime,
            wtr = combo$wtr,
            baro = combo$baro)
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
        'dosat_simGGbts' = {
          if(!is.na(inputs$baro$DateTime)) stop("need non-NA baro$DateTime for dosat_simGGbts")
          calc_ts_with_input_check(inputs, 'calc_ts_dosat_calcGG')
        },
        'dosat_simGGbconst' = {
          if(nrow(inputs$baro) != 1) stop("need 1-row baro for dosat_simGGbconst")
          calc_ts_with_input_check(inputs, 'calc_ts_dosat_calcGG')
        },
        'dosat_simNew' = {
          calc_ts_with_input_check(inputs=c(list(var='dosat'), inputs), 'calc_ts_simNew')
        },
        'dischdaily_calcDMean' = {
          disch_nwis <- read_ts(download_ts("disch_nwis", site, on_local_exists="replace"))
          sitetime_calcLon <- read_ts(download_ts('sitetime_calcLon', site, on_local_exists="replace"))
          combo <- combine_ts(sitetime_calcLon, disch_nwis, method='approx')
          combo <- combo[complete.cases(combo),]
          calc_ts_dischdaily_calcDMean(
            sitetime = combo$sitetime,
            longitude = find_site_coords(site)$lon,
            disch = combo$disch)
        },
        {
          stop("the calculation for var_src ", paste0(var, "_", src), " isn't implemented yet")
        }
      )}, error=function(e) {
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

#' Internal - calculate depth_calcDisch from any data
#' 
#' @param utctime the DateTime with tz of UTC/GMT
#' @param disch the discharge in ft^3 s^-1
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_depth_calcDisch <- function(utctime, disch) {
  data.frame(
    DateTime = utctime,
    depth = calc_depth(
      Q=disch * u(0.0283168466,"m^3 ft^-3"))) %>% u()
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
    u(c(NA, get_units(disch)))
}
