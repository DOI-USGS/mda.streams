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
#' @param version the format (rds/tsv) in which to save the calculated ts
#' @param inputs appropriate only when src is a simXxxx type. a list of named 
#'   inputs (data.frames, constants, etc.) to pass to the specified calculation 
#'   function. These inputs are downloaded from standard locations for the 
#'   calcXxxx variants.
#' @param day_start hour of day start as in \code{mm_model_by_ply}
#' @param day_end hour of day end as in \code{mm_model_by_ply}
#' @param verbose logical. provide status messages?
#' @inheritParams ts_has_file
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
#' # calc
#' 
#' file_par <- stage_calc_ts(sites="nwis_08062500", var="par", src="calcLat", verbose=TRUE)
#' head(read_ts(file_par))
#' attr(file_par, 'choices')
#' 
#' file_depth <- stage_calc_ts(sites="nwis_08062500", var="depth", src="calcDischHarvey", verbose=TRUE)
#' head(read_ts(file_depth))
#' attr(file_depth, 'choices')
#' 
#' file_dosat <- stage_calc_ts(sites="nwis_08062500", var="dosat", src="calcGGbconst", verbose=TRUE)
#' head(read_ts(file_dosat))
#' 
#' # with data provenance
#' 
#' some_sites <- c('nwis_08062500','styx_001002','nwis_05515500','nwis_295826095082200')
#' file_par <- stage_calc_ts(sites=some_sites, var="par", src="calcLat", verbose=TRUE)
#' attr(file_par, 'choices')
#' 
#' # sim calc
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
#' file_dischdaily <- stage_calc_ts(sites="nwis_08062500", 
#'   var="dischdaily", src="calcDMean", verbose=TRUE)
#' head(read_ts(file_dischdaily))
#' }
#' @export
stage_calc_ts <- function(sites, var, src, folder = tempdir(), version=c('rds','tsv'), 
                          inputs=list(), day_start=4, day_end=28, verbose = FALSE, 
                          with_ts_version='rds', with_ts_archived=FALSE, with_ts_uploaded_after='2015-01-01',
                          ...){
  
  version <- match.arg(version)
  if(length(var) > 1) stop("one var at a time, please")
  if(length(src) > 1) stop("one src at a time, please")
  verify_var_src(var, src, on_fail=warning)
  
  # define helper functions that know which site we're in (in the loop that
  # follows these definitions) and pull data while noting data provenance
  site <- choices <- '.local.var' #just to confirm scope; would break if these helpers didn't use site & choices from for loop below
  choose_ts <- function(var) {
    best_src <- tryCatch(
      choose_data_source(
        var, site, logic="priority local",
        with_ts_version=with_ts_version, with_ts_archived=with_ts_archived, with_ts_uploaded_after=with_ts_uploaded_after)$src,
      warning=function(w) {
        stop('with var=',var,', site=',site,', version=',with_ts_version,
             ', archived=',with_ts_archived,', uploaded_after=',with_ts_uploaded_after,': ',
             w$message)
      })
    
    make_var_src(var, best_src)
  }
  get_staging_ts <- function(var_src, ...) {
    choices <<- c(choices, setNames(parse_var_src(var_src, out='src'), paste0('var.', parse_var_src(var_src, out='var'))))
    ts <- get_ts(var_src, site, version=with_ts_version, on_local_exists="replace", ..., quietly=TRUE) # only finds non-archived, ignores upload dates
    ts <- ts[complete.cases(ts),]
    if(nrow(ts) == 0) 
      stop('with var_src=c(',paste0(var_src, collapse=', '),'), site=',site,', version=',with_ts_version,': no complete rows returned')
    ts
  }
  get_staging_coord <- function(type=c('lat','lon','alt')) {
    type <- match.arg(type)
    best_src <- if(parse_site_name(site, out='database')=='styx') 'styx' else 'basic'
    coords <- get_site_coords(site, out=c('site_name', type), use_basedon=(best_src=='styx'))
    choices <<- c(choices, setNames(best_src, paste0('coord.', type)))
    coords[[type]]
  }
  get_staging_coef <- function(coef=c('c','f','a','b','k','m')) {
    coef <- match.arg(coef)
    choices <<- c(choices, setNames('dvqcoefs', paste0('dvqcoef.', coef)))
    dvqcoefs <- get_meta('dvqcoefs')
    dvqcoef <- dvqcoefs[which(dvqcoefs$site_name==site),]
    if(nrow(dvqcoef) != 1) {
      u(NA, switch(coef, c='m', f='', a='m', b='', k='m s^-1', m=''))
    } else {
      dvqcoef[[1, paste0('dvqcoefs.',coef)]]
    }
  }

  # loop through sites, adding to file_paths for any successfully computed & written ts files
  file_paths <- c()
  file_choices <- list()
  un_sites <- unique(sites)
  for (i in 1:length(un_sites)){
    site <- un_sites[i]
    choices <- c() # gets reset for each site & modified each time get_staging_ts, get_staging_coord, or get_staging_coef is called
    
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
          make_var_src(var, src),
          'sitetime_calcLon' = {
            calc_ts_sitetime_calcLon(
              utctime = get_staging_ts(choose_ts('doobs'))$DateTime, 
              longitude = get_staging_coord('lon'))
          },
          'sitetime_simLon' = {
            calc_ts_with_input_check(inputs, 'calc_ts_sitetime_calcLon')
          },
          'sitedate_calcLon' = {
            sitetime <- get_staging_ts(choose_ts("sitetime"))
            calc_ts_sitedate_calcLon(
              sitetime = sitetime$sitetime, 
              longitude = get_staging_coord('lon'),
              day_start = day_start,
              day_end = day_end)
          },
          'sitedate_simLon' = {
            calc_ts_with_input_check(inputs, 'calc_ts_sitedate_calcLon')
          },
          'suntime_calcLon' = {
            calc_ts_suntime_calcLon(
              utctime = get_staging_ts(choose_ts('doobs'))$DateTime, 
              longitude = get_staging_coord('lon'))
          },
          'suntime_simLon' = {
            calc_ts_with_input_check(inputs, 'calc_ts_suntime_calcLon')
          },
          'par_calcLat' = {
            suntime_calcLon <- get_staging_ts('suntime_calcLon')
            calc_ts_par_calcLat(
              utctime = suntime_calcLon$DateTime,
              suntime = suntime_calcLon$suntime,
              latitude = get_staging_coord('lat'))
          },
          'par_calcSw' = {
            sw_best <- get_staging_ts(choose_ts('sw'))
            calc_ts_par_calcSw(
              utctime = sw_best$DateTime,
              sw = sw_best$sw)
          },
          'par_calcLatSw' = {
            par_calcLat <- get_staging_ts('par_calcLat')
            par_calcSw <- get_staging_ts('par_calcSw')
            calc_ts_par_calcLatSw(
              utctime = par_calcLat$DateTime,
              parlat = par_calcLat$par,
              parsw = par_calcSw$par)
          },
          'par_simLat' = {
            calc_ts_with_input_check(inputs, 'calc_ts_par_calcLat')
          },
          'depth_calcDischRaymond' = {
            disch_nwis <- get_staging_ts(choose_ts('disch'))
            calc_ts_depth_calcDischRaymond(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch)
          },
          'depth_calcDischHarvey' = {
            disch_nwis <- get_staging_ts(choose_ts('disch'))
            calc_ts_depth_calcDischHarvey(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch,
              c = get_staging_coef('c'),
              f = get_staging_coef('f'))
          },
          'depth_simDischRaymond' = {
            calc_ts_with_input_check(inputs, 'calc_ts_depth_calcDischRaymond')
          },
          'veloc_calcDischRaymond' = {
            disch_nwis <- get_staging_ts(choose_ts('disch'))
            calc_ts_veloc_calcDischRaymond(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch)
          },
          'veloc_calcDischHarvey' = {
            disch_nwis <- get_staging_ts(choose_ts('disch'))
            calc_ts_veloc_calcDischHarvey(
              utctime = disch_nwis$DateTime,
              disch = disch_nwis$disch,
              k = get_staging_coef('k'),
              m = get_staging_coef('m'))
          },
          'veloc_simDischRaymond' = {
            calc_ts_with_input_check(inputs, 'calc_ts_veloc_calcDischRaymond')
          },
          'dosat_calcGGbts' = {
            baro_best <- choose_ts('baro')
            if(baro_best == "baro_calcElev") stop("need baro_best != baro_calcElev for dosat_calcGGbts")
            combo <- get_staging_ts(
              var_src=c(choose_ts('wtr'), baro_best), method='approx')
            calc_ts_dosat_calcGG(
              utctime = combo$DateTime,
              wtr = combo$wtr,
              baro = combo$baro)
          },
          'baro_calcElev' = {
            elev_ft <- get_staging_coord('alt')
            u(data.frame(
              DateTime=NA, 
              baro=calc_air_pressure(elevation=elev_ft*u(0.3048,"m ft^-1"), attach.units=TRUE)*u(100, "Pa mb^-1")))
          },
          'dosat_calcGGbconst' = {
            wtr_best <- get_staging_ts(choose_ts('wtr'))
            baro_const <- if('baro_src' %in% names(inputs)) {
              ignore <- get_staging_ts('baro_src') # just so the choice gets logged
              inputs$baro_src 
            } else {
              get_staging_ts('baro_calcElev')
            }
            if(nrow(baro_const)!=1 || !is.na(baro_const$DateTime)) 
              stop("need nrow(baro)==1, is.na(baro$DateTime) for dosat_calcGGbconst")
            combo <- combine_ts(wtr_best, baro_const, method='approx')
            calc_ts_dosat_calcGG(
              utctime = combo$DateTime,
              wtr = combo$wtr,
              baro = combo$baro)
          },
          'dosat_simGGbts' = {
            #if(length(inputs$baro) < length(inputs$utctime)/2) stop("need length(baro)~=length(utctime) for dosat_simGGbts")
            calc_ts_with_input_check(inputs, 'calc_ts_dosat_calcGG')
          },
          'dosat_simGGbconst' = {
            if(length(inputs$baro) != 1) stop("need 1-row baro for dosat_simGGbconst")
            calc_ts_with_input_check(inputs, 'calc_ts_dosat_calcGG')
          },
          'dopsat_calcObsSat' = {
            combo <- get_staging_ts(
              var_src=c(choose_ts('doobs'), choose_ts('dosat')), method='approx')
            calc_ts_dopsat_calcObsSat(
              utctime = combo$DateTime, 
              doobs = combo$doobs, 
              dosat = combo$dosat)
          },
          'dopsat_simObsSat' = {
            calc_ts_with_input_check(inputs, 'calc_ts_dopsat_calcObsSat')
          },
          'doinit_simDStart' = {
            calc_ts_with_input_check(inputs, 'calc_ts_doinit_calcDStart')
          },
          'doamp_calcDAmp' = {
            DateTime <- dopsat <- '.dplyr.var'
            get_staging_ts(
              var_src=c('sitedate_calcLon', 'dopsat_calcObsSat'), 
              condense_stat = function(dopsat) diff(range(dopsat)), day_start=day_start, day_end=day_end) %>%
              select(DateTime, doamp=dopsat)
          },
          'dischdaily_calcDMean' = {
            DateTime <- disch <- dischdaily <- '.dplyr.var'
            get_staging_ts(
              var_src=c('sitedate_calcLon', choose_ts('disch')), 
              condense_stat = mean, day_start=day_start, day_end=day_end) %>%
              select(DateTime, dischdaily=disch) %>%
              mutate(dischdaily = dischdaily * u(0.0283168466, "m^3 ft^-3"))
          },
          'velocdaily_calcDMean' = {
            DateTime <- veloc <- '.dplyr.var'
            get_staging_ts(
              var_src=c('sitedate_calcLon', choose_ts('veloc')), 
              condense_stat = mean, day_start=day_start, day_end=day_end) %>%
              select(DateTime, velocdaily=veloc)
          },
          {
            stop("the calculation for ", make_var_src(var, src), " isn't implemented yet")
          }
        )
      }}, error=function(e) {
        warning("trouble in stage_calc_ts: ", e, "\n")
        data.frame()
      }
    )
    
    # write the data to file
    if(isTRUE(verbose)) message("writing the computed data to file")
    if(nrow(ts_calc) > 0) ts_calc <- ts_calc[!is.na(ts_calc[,2]),]
    if(nrow(ts_calc) > 0) {
      fpath <- write_ts(ts_calc, site=site, var=var, src=src, folder=folder, version=version)
      file_choices <- c(file_choices, list(
        mutate(as.data.frame(as.list(choices), stringsAsFactors=FALSE), site_name=site, file_path=fpath)))
      file_paths <- c(file_paths, fpath)
    } else {
      warning("no non-NA values were calculated for var_src ", make_var_src(var, src), ", site ", site)
      # leave file_paths untouched if there's no new file
    }
  }
  
  if(length(file_paths) > 0) attr(file_paths, 'choices') <- bind_rows(file_choices)
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
#' @param utctime the DateTime with tz of UTC
#' @param longitude the site longitude in degrees E
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_sitetime_calcLon <- function(utctime, longitude) {
  data.frame(
    DateTime = utctime,
    sitetime = convert_UTC_to_solartime(
      date.time = utctime, 
      longitude = longitude, 
      time.type = "mean solar")) %>%
    as.data.frame() %>% u()
}

# #' Internal - calculate sitetimedaily_calcLon from any data
# #' 
# #' @param sitetime the DateTimes of the local noons of interest, in UTC
# #' @param longitude the site longitude in degrees E
# #' @importFrom unitted u
# #'   
# #' @keywords internal
# calc_ts_sitetimedaily_calcLon <- function(sitetime, longitude) {
#   data.frame(
#     DateTime = convert_solartime_to_UTC(
#       any.solar.time = sitetime, 
#       longitude = longitude, 
#       time.type = "mean solar"),
#     sitetimedaily = sitetime) %>%
#     as.data.frame() %>% u()
# }

#' Internal - calculate sitedate_calcLon from any data
#' 
#' @param sitetime the DateTimes of the local noons of interest, in UTC
#' @param longitude the site longitude in degrees E
#' @importFrom unitted u
#' @import streamMetabolizer
#' @import dplyr
#'   
#' @keywords internal
calc_ts_sitedate_calcLon <- function(sitetime, longitude, day_start=4, day_end=28) {
  sitenoon<- '.dplyr.var'
  mm_model_by_ply(
    function(data_ply, data_daily_ply, ply_date, ...) {
      data.frame(
        sitenoon=as.POSIXct(paste0(ply_date, " 12:00:00"), tz="UTC"))
    }, 
    data=data.frame(solar.time=sitetime, dummycol=NA), # need dummycol so df is >1 col so mm_model_by_ply works
    data_daily=NULL, 
    day_start=day_start, day_end=day_end, day_tests=c(), timestep_days=FALSE
  ) %>%
    transmute(
      DateTime = convert_solartime_to_UTC(
        any.solar.time = sitenoon, 
        longitude = longitude, 
        time.type = "mean solar"),
      sitedate = date) %>% u()
}

#' Internal - calculate doinit_calcDStart from any data
#' 
#' @param sitetime the DateTimes of the local noons of interest, in UTC
#' @param doobs vector of dissolved oxygen values, same length as sitetime
#' @param longitude the site longitude in degrees E
#' @importFrom unitted u
#' @import streamMetabolizer
#' @import dplyr
#'   
#' @keywords internal
calc_ts_doinit_calcDStart <- function(sitetime, doobs, longitude, day_start=4, day_end=28) {
  sitenoon <- doinit <- '.dplyr.var'
  mm_model_by_ply(
    function(data_ply, data_daily_ply, ply_date, ...) {
      data.frame(
        sitenoon=as.POSIXct(paste0(ply_date, " 12:00:00"), tz="UTC"),
        doinit=data_ply[1,'doobs'])
    },
    data=data.frame(solar.time=sitetime, doobs=doobs), # need dummycol so df is >1 col so mm_model_by_ply works
    data_daily=NULL, 
    day_start=day_start, day_end=day_end, day_tests=c(), timestep_days=FALSE
  ) %>%
    transmute(
      DateTime = convert_solartime_to_UTC(
        any.solar.time = sitenoon, 
        longitude = longitude, 
        time.type = "mean solar"),
      doinit = doinit) %>% 
    u(c(NA, get_units(doobs))) # transmute loses the units
}

#' Internal - calculate suntime_calcLon from any data
#' 
#' @param utctime the DateTime with tz of UTC
#' @param longitude the site longitude in degrees E
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_suntime_calcLon <- function(utctime, longitude) {
  data.frame(
    DateTime = utctime,
    suntime = convert_UTC_to_solartime(
      date.time = utctime, 
      longitude = longitude, 
      time.type = "apparent solar")) %>%
    as.data.frame() %>% u()
}

#' Internal - calculate par_calcLat from any data
#' 
#' @param utctime the DateTime with tz of UTC
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
        app.solar.time = suntime,
        latitude = latitude))) %>% 
    u()
}

#' Internal - calculate par from SW data
#' 
#' @param utctime the DateTime with tz of UTC
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

#' Internal - calculate par by merging modeled (calcLat) and observed (calcSw) data
#' 
#' @param utctime the DateTime with tz of UTC
#' @param parlat PAR in umol m^2 s^-1
#' @import streamMetabolizer
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_par_calcLatSw <- function(utctime, parlat, parsw, latitude, longitude) {
  data.frame(
    DateTime = utctime,
    par = calc_light_merged(
      PAR.obs=parsw, solar.time=parlat$solar.time, 
      latitude=latitude, longitude=longitude)
  )
}


#' #' Internal - calculate depth_calcDisch from any data using the Raymond et al.
#' #' coefficients
#' #' 
#' #' @param utctime the DateTime with tz of UTC
#' #' @param disch the discharge in ft^3 s^-1
#' #' @importFrom unitted u
#' #' @import streamMetabolizer
#' #'   
#' #' @keywords internal
#' calc_ts_depth_calcDisch <- function(utctime, disch) {
#'   Q <- verify_units(disch * u(0.0283168466,"m^3 ft^-3"), 'm^3 s^-1')
#'   data.frame(
#'     DateTime = utctime,
#'     depth = calc_depth(Q=Q)) %>% u()
#' }

#' Internal - calculate depth_calcDisch from any data using the Raymond et al.
#' coefficients
#' 
#' @param utctime the DateTime with tz of UTC
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
#' @param utctime the DateTime with tz of UTC
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
#' @param utctime the DateTime with tz of UTC
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
#' @param utctime the DateTime with tz of UTC
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
#' @param utctime the DateTime with tz of UTC
#' @param wtr the water temperature
#' @param baro the barometric pressure
#' @importFrom unitted u
#' 
#' @keywords internal
calc_ts_dosat_calcGG <- function(utctime, wtr, baro) {
  data.frame(
    DateTime = utctime,
    dosat = calc_DO_sat(
      temp.water = wtr, 
      pressure.air = baro * u(0.01, "mb Pa^-1"),
      salinity.water = u(0, 'PSU'))) %>% u()
}

#' Internal - calculate dopsat_calcObsSat from any data
#' 
#' @param utctime the DateTime with tz of UTC
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
#' @param utctime the DateTime with tz of UTC
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
  from_data <- tryCatch({
    get_ts(var_src=make_var_src(var, from_src), site_name=from_site, version='rds', on_local_exists="replace")
  }, error=function(e) {
    get_ts(var_src=make_var_src(var, from_src), site_name=from_site, version='tsv', on_local_exists="replace")
  })
  if(!is.null(filter_fun)) filter_fun(from_data) else from_data
}
