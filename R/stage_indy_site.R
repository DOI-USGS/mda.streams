#' Generate a list of files to post as a custom ('indy') site
#' 
#' This function is designed for truly isolated (non-NWIS, etc.) sites. The site
#' that is created will be named 'indy_[site_num]', where 'indy' is a recognized
#' prefix in mda.streams. If an NWIS gage is available, even if some data is not
#' from NWIS, the site should be established as an 'nwis' site (see post_site) 
#' as for other NWIS sites; this will enable lookups of other NWIS data and 
#' metadata for the site.
#' 
#' @param site_num The id, not necessarily numeric that is to follow 'indy_' in 
#'   the site_name
#' @param data data.frame of data to parse and post
#' @param info character string describing the primary source of data for this 
#'   site
#' @param long_name Longer character string describing the site, or blank if 
#'   site_name is prefixed with 'nwis' (in which case this will be looked up)
#' @param lat latitude in degN
#' @param lon longitude in degE
#' @param alt altitude in ft
#' @param var_format What format are the data column names in - mda.streams 
#'   format (e.g., sitetime, doobs, dosat) or streamMetabolizer format (e.g., 
#'   solar.time. DO.obs, DO.sat)?
#' @param remove_NAs logical. Should ts rows with NAs be removed?
#' @param collapse_const If a data column has all identical values, should these
#'   be collapsed to a single const row in the ts file?
#' @inheritParams write_ts
#' @param folder the folder in which to save the staged information
#' @export
#' @importFrom unitted u v
#' @importFrom lubridate force_tz
#' @import streamMetabolizer
stage_indy_site <- function(
  site_num, 
  data,
  info=as.character(NA), long_name=as.character(NA), 
  lat=u(as.numeric(NA),'degN'), lon=u(as.numeric(NA),'degE'),
  alt=u(as.numeric(NA),'ft'),
  var_format=c('mda.streams','streamMetabolizer'),
  remove_NAs=TRUE,
  collapse_const=TRUE,
  version=c('rds','tsv'),
  folder=tempdir()
) {
  
  var_format <- match.arg(var_format)
  version <- match.arg(version)
  
  site_name <- paste0("indy_", site_num)
  staged_meta <- stage_meta_indy(rows=u(data.frame(
    site_name=site_name,
    long_name=long_name,
    lat=lat,
    lon=lon,
    alt=alt,
    info=info)),
    folder=folder,
    on_exists='replace')

  if(var_format == 'streamMetabolizer') {
    codes <- get_var_src_codes()
    mda_names <- codes[match(names(data), codes$metab_var), 'var']
    names(data) <- mda_names
  }

  # add the appropriate src code to each column name
  add_src <- function(varname, srcname='indy') {
    datnames <- names(data)
    datnames[datnames==varname] <- paste0(varname, '_', srcname)
    datnames
  }
  
  
  # check/force tzs first so can be used in datetime and/or sitetime calcs
  if(('sitetime' %in% names(data)) && (lubridate::tz(v(data$sitetime)) != 'UTC')) {
    warning('forcing tz(sitetime) to UTC')
    data$sitetime <- u(force_tz(v(data$sitetime), tzone='UTC'), NA)
  }
  if(('DateTime' %in% names(data)) && (lubridate::tz(v(data$DateTime)) != 'UTC')) {
    stop('tz(DateTime) must be UTC')
  }
  
  if(!('DateTime' %in% names(data))) {
    if(!('sitetime' %in% names(data))) stop("need DateTime and/or sitetime columns in data")
    data$DateTime <- u(convert_solartime_to_UTC(v(data$sitetime), lon), NA)
  }
  
  if('sitetime' %in% names(data)) {
    names(data) <- add_src('sitetime', 'indy')
  } else {
    data$sitetime_calcLon <- u(convert_UTC_to_solartime(v(data$DateTime), longitude=lon), NA)
  }
  
  if('doobs' %in% names(data)) {
    names(data) <- add_src('doobs', 'indy')
  } else {
    stop("need doobs in dataset")
  }
  
  if('wtr' %in% names(data)) {
    names(data) <- add_src('wtr', 'indy')
  } else {
    stop("need wtr in dataset")
  }
  
  if('dosat' %in% names(data)) {
    names(data) <- add_src('dosat', 'indy')
  } else {
    if('baro' %in% names(data)) {
      baro_src = 'indy'
    } else {
      data$baro <- calc_air_pressure(elevation=alt*u(0.3048,"m ft^-1"), attach.units = TRUE)*u(100,"Pa mb^-1")
      baro_src = 'calcElev'
    }
    data$dosat <- calc_DO_sat(temp.water=data$wtr, pressure.air=data$baro*u(0.01,"mb Pa^-1"))
    names(data) <- add_src('dosat', if(length(unique(data$baro))==1) 'calcGGbconst' else 'calcGGbts')
    names(data) <- add_src('baro', baro_src)
  }
  
  if('depth' %in% names(data)) {
    names(data) <- add_src('depth', 'indy')
  } else {
    stop("need depth in dataset")
  }
  
  if('par' %in% names(data)) {
    names(data) <- add_src('par', 'indy')
  } else {
    # ignores the possibility that sw is present. if this feature becomes needed, add it here
    suntime <- convert_UTC_to_solartime(data$DateTime, longitude=lon, time.type='apparent solar')
    data$par_calcLat <- convert_SW_to_PAR(calc_solar_insolation(
      app.solar.time=v(suntime), latitude=lat, max.insolation=convert_PAR_to_SW(2326), attach.units=TRUE))
  } 
  
  # write the data (instantaneous) timeseries files
  data_names <- names(data)[which(names(data) != 'DateTime')]
  data_list <- lapply(
    setNames(data_names, parse_var_src(data_names, 'var')),
    function(datcol) {
      tsdat <- data[c('DateTime',datcol)]
      names(tsdat)[2] <- parse_var_src(datcol, 'var')
      if(collapse_const) {
        if(length(unique(tsdat[,2])) == 1) {
          tsdat[1,1] <- as.POSIXct(NA)
          tsdat <- tsdat[1,]
        }
      }
      if(remove_NAs) {
        tsdat <- tsdat[!is.na(tsdat[,2]), ]
      }
      tryCatch({
        write_ts(data=tsdat, site=site_name, var=parse_var_src(datcol, 'var'), src=parse_var_src(datcol, 'src'), 
                 version=version, folder=folder)
      }, error=function(e) NULL)
    }
  )
  # remove any failed attempts to write_ts; just don't write them
  data_list <- data_list[!sapply(data_list, is.null)]
    
  # bundle the filenames into a list
  c(list(site_name=site_name, metadata=staged_meta), data_list)
}
